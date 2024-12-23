module Whine.Config where

import Whine.Prelude

import Control.Monad.Writer (runWriter)
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.Nullable as N
import Effect.Exception as Err
import Foreign (unsafeToForeign)
import Foreign.Object as FO
import Node.FS.Sync (readTextFile)
import Record (merge)
import Whine.Types (RuleFactories, RuleSet, Violations, WithFile, WithRule)
import Whine.Yaml (parseYaml)

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
-- | This is what this function takes as parameter - a JS object of JS objects. The outer
-- | object's keys are rule IDs, the inner objects' keys are rule-specific settings, plus
-- | common options of "include", "exclude", and "enable", all optional.
-- |
-- | The other parameter is an array of rule factories, each a tuple of (ruleId /\ factoryFunction),
-- | where `factoryFunction :: Foreign -> Either String Rule`
-- |
-- | The result is a `RuleSet` value, which is a map of rule IDs to rule configs.
-- |
-- | The whole thing runs in a writer monad to which it can report any errors while parsing
-- | the config. The errors doesn't stop the parsing, but erroneous rules do not make it
-- | into the resulting map.
parseConfig :: ∀ m n. MonadWriter (Violations (WithRule + ())) m => RuleFactories n -> FO.Object (FO.Object Foreign) -> m (RuleSet n)
parseConfig factories config =
  Map.fromFoldable <$> catMaybes <$>
    for factories \(ruleId /\ factory) -> do
      let ruleConfig = FO.lookup ruleId config # fromMaybe FO.empty

          orFail :: ∀ a. _ -> Either String a -> m (Maybe a)
          orFail message = case _ of
            Right x ->
              pure $ Just x
            Left err -> do
              tell [{ rule: ruleId, source: Nothing, message: message <> ": " <> err }]
              pure Nothing

          commonProp :: ∀ a. CanReceiveFromJavaScript a => String -> m (Maybe a)
          commonProp name = case FO.lookup name ruleConfig of
            Nothing ->
              pure Nothing
            Just value -> do
              nValue <- value # readForeign' # orFail ("Malformed '" <> name <> "'")
              pure $ nValue >>= N.toMaybe

      include <- commonProp "include"
      exclude <- commonProp "exclude"
      enabled <- commonProp "enabled" <#> fromMaybe true

      if enabled then
        factory (unsafeToForeign ruleConfig)
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

  config <- case text >>= parseYaml >>= readForeign' of
    Left err -> do
      tell
        [ { message: "Error reading config: " <> err
          , source: Nothing
          , file: { path: configFile, lines: Nothing }
          , rule: ""
          }
        ]
      pure FO.empty

    Right c ->
      pure c

  let res /\ violations = runWriter $ parseConfig factories config

  tell $ violations <#> merge { file: { path: configFile, lines: Nothing } }
  pure res
