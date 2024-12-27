module Whine.Runner.Config where

import Whine.Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Error.Class (throwError)
import Control.Monad.Writer (runWriter)
import Data.Array.NonEmpty as NEA
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Strict as CJS
import Data.Map as Map
import Effect.Exception as Err
import JSON as JSON
import Node.FS.Sync (readTextFile)
import Record (merge)
import Whine.Types (RuleFactories, RuleSet, Violations, WithFile, WithRule)
import Whine.Runner.Yaml (parseYaml)

data PackageSpec
  = JustPackage
  | PackageVersion String
  | LocalPackage { path :: FilePath, module :: Maybe String }

type Config =
  { rulePackages :: Map { package :: String } PackageSpec
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
parseConfig :: ∀ m n. MonadWriter (Violations (WithRule + ())) m => RuleFactories n -> Map String JSON -> m (RuleSet n)
parseConfig factories config =
  Map.fromFoldable <$> catMaybes <$>
    for factories \(ruleId /\ factory) -> do

      let ruleConfig = Map.lookup ruleId config
          ruleConfigAsMap = ruleConfig >>= (CJ.decode (CJ.Common.strMap CJ.json) >>> hush)

          orFail :: ∀ a. _ -> Either String a -> m (Maybe a)
          orFail message = case _ of
            Right x ->
              pure $ Just x
            Left err -> do
              tell [{ rule: ruleId, source: Nothing, message: message <> ": " <> err }]
              pure Nothing

          commonProp :: ∀ a. String -> CJ.Codec a -> m (Maybe a)
          commonProp name codec = case Map.lookup name =<< ruleConfigAsMap of
            Nothing ->
              pure Nothing
            Just value ->
              value # CJ.decode codec # lmap DecodeError.print # orFail ("Malformed '" <> name <> "'")

      include <- commonProp "include" (CJ.array CJ.string)
      exclude <- commonProp "exclude" (CJ.array CJ.string)
      enabled <- commonProp "enabled" CJ.boolean <#> fromMaybe true

      if enabled then
        factory (fromMaybe JSON.null ruleConfig)
          <#> (\rule -> ruleId /\
            { rule
            , globs:
              { include: include >>= NEA.fromArray
              , exclude: exclude >>= NEA.fromArray
              }
            }
          )
          # orFail "Malformed config"
      else
        pure Nothing

-- | Reads config from a given file and parses it.
readConfig :: ∀ m n. MonadEffect m => MonadWriter (Violations (WithRule + WithFile + ())) m => RuleFactories n -> FilePath -> m (RuleSet n)
readConfig factories configFile = do
  text <- lmap Err.message <$> liftEffect (try $ readTextFile UTF8 configFile)

  config <- case text >>= parseYaml >>= (CJ.decode configCodec >>> lmap DecodeError.print) of
    Left err -> do
      tell
        [ { message: "Error reading config: " <> err
          , source: Nothing
          , file: { path: configFile, lines: Nothing }
          , rule: ""
          }
        ]
      pure defaultConfig

    Right c ->
      pure c

  let res /\ violations = runWriter $ parseConfig factories (config.rules # fromMaybe Map.empty)

  tell $ violations <#> merge { file: { path: configFile, lines: Nothing } }
  pure res

configCodec :: CJ.Codec Config
configCodec = CJS.objectStrict $ CJS.record
  # CJS.recordProp @"rulePackages" packagesCodec
  # CJS.recordPropOptional @"rules" (CJ.Common.strMap CJ.json)

packagesCodec :: CJ.Codec (Map { package :: String } PackageSpec)
packagesCodec = dimap Map.toUnfoldable Map.fromFoldable $ CJ.array packageCodec
  where
    packageCodec :: CJ.Codec ({ package :: String } /\ PackageSpec)
    packageCodec = Codec.codec' dec enc
      where
        enc ({ package } /\ JustPackage) = Codec.encode CJ.string package
        enc ({ package } /\ PackageVersion v) = Codec.encode (CJ.Common.strMap CJ.string) $ Map.singleton package v
        enc ({ package } /\ LocalPackage p) = Codec.encode localPackageCodec $ Map.singleton package { local: p.path, module: p.module }

        dec json = justPackage <|> withVersion <|> localPackage
          where
            justPackage = do
              package <- Codec.decode CJ.Common.string json
              pure $ { package } /\ JustPackage

            withVersion = do
              m <- Codec.decode (CJ.Common.strMap CJ.string) json
              case Map.toUnfoldable m of
                [package /\ version] -> pure $ { package } /\ PackageVersion version
                _ -> throwError $ DecodeError.basic "Malformed package specification"

            localPackage = do
              m <- Codec.decode localPackageCodec json
              case Map.toUnfoldable m of
                [package /\ p] -> pure $ { package } /\ LocalPackage { path: p.local, module: p.module }
                _ -> throwError $ DecodeError.basic "Malformed package specification"

    localPackageCodec = CJ.Common.strMap $
      CJS.objectStrict $ CJS.record
        # CJS.recordProp @"local" CJ.string
        # CJS.recordPropOptional @"module" CJ.string

defaultConfig :: Config
defaultConfig =
  { rulePackages: Map.singleton { package: "whine-core" } JustPackage
  , rules: Nothing
  }
