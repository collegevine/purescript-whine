module Whine where

import Whine.Prelude

import Data.Map as Map
import Data.String as String
import Effect.Exception as Err
import Node.FS.Sync (readTextFile)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Range (class RangeOf)
import PureScript.CST.Types (Module)
import Record (merge)
import Whine.Log (LogSeverity, logDebug)
import Whine.Muting (MutedRange(..), mutedRanges)
import Whine.Runner.Config (RuleSet)
import Whine.Runner.Glob as Glob
import Whine.Types (Rule(..), WithFile, WithMuted, WithRule, reportViolation)
import WhineM (WhineM, mapViolations)

-- | Given a parsed PS module, runs all the rules on it. The rules report
-- | violations via Writer side-effects.
runRules :: ∀ m e. MonadEffect m => RangeOf e => RuleSet -> Module e -> WhineM (WithRule + ()) { logLevel :: LogSeverity } m Unit
runRules rs mdl =
  forWithIndex_ rs \rid { rule } ->
    let (Rule r) = rule
    in mapViolations (merge { rule: rid }) (r mdl)

-- | Given a file path, reads the file, then passes it to `checkModule` (see
-- | comments there).
checkFile :: ∀ m. MonadEffect m => RuleSet -> FilePath -> WhineM (WithRule + WithMuted + WithFile + ()) { logLevel :: LogSeverity } m Unit
checkFile rules path = do
  eText <- liftEffect $ try $ readTextFile UTF8 path
  case eText of
    Left err ->
      reportViolation
        { message: "Failed to read the file: " <> Err.message err
        , source: Nothing
        , muted: false
        , rule: ""
        , file: { path, lines: Nothing }
        }
    Right text ->
      checkModule rules { path, text }

-- | Given full text of a PureScript module, parses it and runs all the rules on
-- | it, then amends the rules with the `muted :: Boolean` flag based on the
-- | muting directives in the file itself as well as include/exclude globs in
-- | each rule's config. If reading the file or parsing it fails, those
-- | conditions are reported as linter violations, rather than a big loud crash.
checkModule :: ∀ m. MonadEffect m => RuleSet -> { path :: FilePath, text :: String } -> WhineM (WithRule + WithMuted + WithFile + ()) { logLevel :: LogSeverity } m Unit
checkModule rules { path, text } =
  mapViolations addMutedAndFile do
    logDebug $ "Parsing " <> path
    case parseModule text of
      ParseFailed _ ->
        reportViolation { rule: "", source: Nothing, message: "Failed to parse the file" }
      ParseSucceededWithErrors m _ -> do
        logDebug $ "Parsed " <> path <> ", running rules"
        runRules rules m
      ParseSucceeded m -> do
        logDebug $ "Parsed " <> path <> ", running rules"
        runRules rules m
  where
    addMutedAndFile v = v # merge
      { file
      , muted: isMutedByDirective v || isExcludedByPath v
      }

    lines = String.split (Pattern "\n") text
    file = { path, lines: Just lines }
    ranges = mutedRanges { lines }

    isIntersecting src = case _ of
      MutedLine l -> src.start.line == l
      MutedRange { start, end } -> src.start.line >= start && src.end.line <= end

    isMutedByDirective v = fromMaybe false do
      source <- v.source
      rs <- Map.lookup v.rule ranges
      guard $ any (isIntersecting source) rs
      pure true

    isExcludedByPath v =
      let globs = Map.lookup v.rule rules <#> _.globs # fromMaybe Glob.emptyGlobs
      in not Glob.test globs path
