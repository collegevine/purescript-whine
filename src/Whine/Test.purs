module Whine.Test where

import Whine.Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Map as Map
import Data.String as String
import Data.String.CodeUnits as StringCU
import Data.String.Utils as StringU
import Effect.Exception (Error, error)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import Whine (runRules)
import Whine.Log as Log
import Whine.Runner.Glob (emptyGlobs)
import Whine.Types (Rule, Violations)
import WhineM (runWhineM)

runRule :: ∀ m. MonadEffect m => MonadThrow Error m =>
  { rule :: Rule
  , module :: String
  , assertViolationMessage :: String -> m Unit
  }
  -> m (Array String)
runRule args =
  runRule' { rule: args.rule, module: args.module }
  >>= traverse \r -> do
    args.assertViolationMessage r.message
    case r.source of
      Just s -> pure $ formatRange s
      Nothing -> throwError $ error "Expected source range"

formatRange :: SourceRange -> String
formatRange s = fold [show s.start.line, ":", show s.start.column, "-", show s.end.line, ":", show s.end.column]

moduleText :: String -> String
moduleText source =
  joinWith "\n" $ lines <#> \s ->
    fromMaybe s $ String.stripPrefix (Pattern firstLinePadding) s
  where
    lines = String.split (Pattern "\n") source # dropWhile (eq "")
    firstLine = lines !! 0 # fromMaybe ""
    firstLinePadding = firstLine # StringCU.takeWhile (eq ' ')

runRule' :: ∀ m. MonadEffect m => MonadThrow Error m =>
  { rule :: Rule
  , module :: String
  }
  -> m (Violations ())
runRule' args = do
  _ /\ violations <- runWhineM { logLevel: Log.LogInfo }
    case parseModule mod of
      ParseFailed _ -> throwError $ error "Failed to parse"
      ParseSucceededWithErrors _ _ -> throwError $ error "Failed to parse"
      ParseSucceeded m -> runRules ruleSet m
  pure $ violations <#> \r ->
    { source: shiftSource <$> r.source, message: r.message }
  where
    ruleSet = Map.singleton "ThisRuleId" { rule: args.rule, globs: emptyGlobs }

    shiftSource s = s { start { line = s.start.line - 1 }, end { line = s.end.line - 1 } }

    mod' = moduleText args.module
    mod =
      (if StringU.startsWith "module" mod' then "" else "module A where\n")
      <> mod'
