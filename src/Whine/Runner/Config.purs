module Whine.Runner.Config where

import Whine.Runner.Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Error.Class (throwError)
import Data.Array.NonEmpty as NEA
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Strict as CJS
import Data.Map as Map
import Data.String as String
import Data.String.NonEmpty as NES
import Effect.Exception as Err
import JSON as JSON
import Node.FS.Sync (readTextFile)
import Record (merge)
import Whine.Runner.Glob (Globs)
import Whine.Runner.PackageVersion (Version, formatVersion, parseVersion)
import Whine.Runner.Yaml (parseYaml)
import Whine.Types (class MonadReport, Rule, RuleFactories, RuleId, WithFile, WithMuted, WithRule, reportViolation)
import WhineM (WhineM, mapViolations)

data PackageSpec
  = JustPackage
  | PackageVersion Version
  | LocalPackage { path :: FilePath, module :: Maybe String }

type Config =
  { rules :: RuleSet
  , files :: Globs
  }

-- | For every rule ID we have a rule implementation `Rule m` and some common
-- | config values that can be applied to any rule and are handled by the
-- | framework.
type RuleSet = Map RuleId
  { rule :: Rule
  , globs :: Globs
  }

type ConfigJson =
  { rulePackages :: Map { package :: String } PackageSpec
  , include :: Maybe (Array NonEmptyString)
  , exclude :: Maybe (Array NonEmptyString)
  , rules :: Maybe (Map String JSON)
  }

-- | The config has the general shape of:
-- |
-- |     RuleId:
-- |       include: ["one/file.purs", "many/**/*.purs"]
-- |       exclude: ["same.purs"]
-- |       enabled: true
-- |       ruleSpecificSetting1: "foo"
-- |       ruleSpecificSetting2: 42
-- |       ruleSpecificSetting3: ["a", "b", "c"]
-- |
-- |     AnotherRuleId:
-- |       ... samesies ...
-- |
-- | This is what this function takes as parameter - a map of JS objects. The
-- | keys are rule IDs, the inner objects are rule-specific settings, plus
-- | common options of "include", "exclude", and "enable", all optional.
-- |
-- | The other parameter is an array of rule factories, each a tuple of
-- | (ruleId /\ factoryFunction), where `factoryFunction :: JSON -> Either String Rule`
-- |
-- | The result is a `RuleSet` value, which is a map of rule IDs to rule
-- | configs.
-- |
-- | The whole thing runs in a writer monad to which it can report any errors
-- | while parsing the config. The errors doesn't stop the parsing, but
-- | erroneous rules do not make it into the resulting map.
parseRuleConfigs :: ∀ m. MonadReport (WithRule + ()) m => RuleFactories -> Map String JSON -> m RuleSet
parseRuleConfigs factories config =
  Map.fromFoldable <$> catMaybes <$> concat <$>

    for factories \(ruleId /\ factory) -> do

      let configs =
            Map.lookup ruleId configsByRule
            <#> map (rmap Just)
            # fromMaybe [ruleId /\ Nothing]

      for configs \(effectiveRuleId /\ ruleConfig) -> do
        let
          ruleConfigAsMap :: Maybe (Map String JSON)
          ruleConfigAsMap = ruleConfig >>= (CJ.decode (CJ.Common.strMap CJ.json) >>> hush)

          orFail :: ∀ a. _ -> Either String a -> m (Maybe a)
          orFail message = case _ of
            Right x ->
              pure $ Just x
            Left err -> do
              reportViolation { rule: ruleId, source: Nothing, message: message <> ": " <> err }
              pure Nothing

          commonProp :: ∀ a. String -> CJ.Codec a -> m (Maybe a)
          commonProp name codec = case Map.lookup name =<< ruleConfigAsMap of
            Nothing ->
              pure Nothing
            Just value ->
              value # CJ.decode codec # lmap DecodeError.print # orFail ("Malformed '" <> name <> "'")

        include <- commonProp "include" (CJ.array CJ.Common.nonEmptyString)
        exclude <- commonProp "exclude" (CJ.array CJ.Common.nonEmptyString)
        enabled <- commonProp "enabled" CJ.boolean <#> fromMaybe true

        if enabled then
          factory (fromMaybe JSON.null ruleConfig)
            <#> (\rule -> effectiveRuleId /\
              { rule
              , globs:
                { include: include # fromMaybe []
                , exclude: exclude # fromMaybe []
                }
              }
            )
            # orFail "Malformed config"
        else
          pure Nothing

  where
    -- If the config contains the same rule ID multiple times, with different subids,
    -- like "rule/subid1" and "rule/subid2", we group them by the main rule ID, and
    -- the map values become arrays of the individual configs.
    configsByRule :: Map String (Array (String /\ JSON))
    configsByRule =
      (Map.toUnfoldable config :: Array _)
      >>= (\(key /\ value) -> key # String.split (Pattern "/") # take 1 <#> (_ /\ key /\ value))
      # groupAllBy (comparing fst)
      <#> (\g -> fst (NEA.head g) /\ (snd <$> NEA.toArray g))
      # Map.fromFoldable

-- | Reads config from a given file and parses it.
readConfig :: ∀ m. MonadEffect m => RuleFactories -> FilePath -> WhineM (WithRule + WithFile + WithMuted + ()) Env m Config
readConfig factories configFile = do
  text <- lmap Err.message <$> liftEffect (try $ readTextFile UTF8 configFile)

  config <- case text >>= parseYaml >>= (CJ.decode configCodec >>> lmap DecodeError.print) of
    Left err -> do
      reportViolation
        { message: "Error reading config: " <> err
        , source: Nothing
        , file: { path: configFile, lines: Nothing }
        , muted: false
        , rule: ""
        }
      pure defaultConfig

    Right c ->
      pure c

  res <-
    mapViolations (merge { file: { path: configFile, lines: Nothing }, muted: false })
      (parseRuleConfigs factories (config.rules # fromMaybe Map.empty))

  pure
    { rules: res
    , files:
        { include: config.include # fromMaybe (NES.fromString `mapMaybe` ["src/**/*.purs", "test/**/*.purs"])
        , exclude: config.exclude # fromMaybe []
        }
    }

configCodec :: CJ.Codec ConfigJson
configCodec = CJS.objectStrict $ CJS.record
  # CJS.recordProp @"rulePackages" packagesCodec
  # CJS.recordPropOptional @"include" (CJ.array CJ.Common.nonEmptyString)
  # CJS.recordPropOptional @"exclude" (CJ.array CJ.Common.nonEmptyString)
  # CJS.recordPropOptional @"rules" (CJ.Common.strMap CJ.json)

packagesCodec :: CJ.Codec (Map { package :: String } PackageSpec)
packagesCodec = dimap Map.toUnfoldable Map.fromFoldable $ CJ.array packageCodec
  where
    packageCodec :: CJ.Codec ({ package :: String } /\ PackageSpec)
    packageCodec = Codec.codec' dec enc
      where
        enc ({ package } /\ JustPackage) = Codec.encode CJ.string package
        enc ({ package } /\ PackageVersion v) = Codec.encode (CJ.Common.strMap CJ.string) $ Map.singleton package (formatVersion v)
        enc ({ package } /\ LocalPackage p) = Codec.encode localPackageCodec $ Map.singleton package { local: p.path, module: p.module }

        dec json = justPackage <|> withVersion <|> localPackage
          where
            justPackage = do
              package <- Codec.decode CJ.Common.string json
              pure $ { package } /\ JustPackage

            withVersion = do
              m <- Codec.decode (CJ.Common.strMap CJ.string) json
              case Map.toUnfoldable m of
                [package /\ version] ->
                  case parseVersion version of
                    Right v -> pure $ { package } /\ PackageVersion v
                    Left err -> throwError $ DecodeError.basic err
                _ ->
                  throwError $ DecodeError.basic "Malformed package specification"

            localPackage = do
              m <- Codec.decode localPackageCodec json
              case Map.toUnfoldable m of
                [package /\ p] -> pure $ { package } /\ LocalPackage { path: p.local, module: p.module }
                _ -> throwError $ DecodeError.basic "Malformed package specification"

    localPackageCodec = CJ.Common.strMap $
      CJS.objectStrict $ CJS.record
        # CJS.recordProp @"local" CJ.string
        # CJS.recordPropOptional @"module" CJ.string

defaultConfig :: ConfigJson
defaultConfig =
  { rulePackages: Map.singleton { package: "whine-core" } JustPackage
  , include: Nothing
  , exclude: Nothing
  , rules: Nothing
  }
