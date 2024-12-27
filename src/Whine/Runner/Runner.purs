module Whine.Runner where

import Whine.Runner.Prelude

import Control.Monad.Reader (runReaderT)
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.String as String
import Effect.Class.Console as Console
import Effect.Exception as Err
import Node.FS.Sync (readTextFile)
import Node.Process (exit')
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Range (class RangeOf)
import PureScript.CST.Traversal (traverseBinder, traverseDecl, traverseExpr, traverseModule, traverseType)
import PureScript.CST.Types (Module(..), ModuleHeader(..), Separated(..), Wrapped(..))
import Record (merge)
import Whine.Muting (MutedRange(..), mutedRanges)
import Whine.Print (printViolation)
import Whine.Runner.Cli as Cli
import Whine.Runner.Config (readConfig)
import Whine.Runner.Glob (glob)
import Whine.Runner.Glob as Glob
import Whine.Runner.LanguageServer (startLanguageServer)
import Whine.Types (Handle(..), RuleFactories, RuleSet, Violations, WithFile, WithMuted, WithRule, mapViolation)

-- | The main entry point into the linter. It takes some basic parameters and
-- | runs the whole thing: reads the config, parses it, instantiates the rules,
-- | globs the input files, parses them, and runs the rules through every file.
runWhineAndPrintResultsAndExit :: RuleFactories (WriterT (Violations ()) RunnerM) -> Effect Unit
runWhineAndPrintResultsAndExit factories = launchAff_ do
  args <- Cli.parseCliArgs

  let env = { logLevel: Cli.determineLogLevel args }
      main = case args.command of
        Cli.JustWhine -> do
          results <- runWhine { factories, globs: ["src/**/*.purs"], configFile: "whine.yaml" }
          unless args.quiet $
            Console.log `traverse_` (printViolation `mapMaybe` results)
          liftEffect $ exit' if results # any (not _.muted) then 1 else 0

        Cli.LanguageServer ->
          startLanguageServer { factories, configFile: "whine.yaml" }

  runReaderT main env

-- | The main entry point into the linter. It takes some basic parameters and
-- | runs the whole thing: reads the config, parses it, instantiates the rules,
-- | globs the input files, parses them, and runs the rules through every file.
runWhine :: ∀ m. MonadEffect m =>
  { factories :: RuleFactories (WriterT (Violations ()) m)
  , globs :: Array String
  , configFile :: FilePath
  }
  -> m (Violations (WithRule + WithMuted + WithFile + ()))
runWhine { factories, globs, configFile } = execWriterT do
  files <- liftEffect $ fold <$> glob `traverse` globs
  ruleSet <- mapViolation (merge { muted: false }) $ readConfig factories configFile
  checkFile ruleSet `traverse_` files

-- | Given a parsed PS module, runs all the rules on it. The rules report
-- | violations via Writer side-effects.
runRules :: ∀ m e. Monad m => RangeOf e => RuleSet (WriterT (Violations ()) m) -> Module e -> WriterT (Violations (WithRule + ())) m Unit
runRules rs mdl = void do
  onModule mdl
  traverseModule visitor mdl
  where
    visitor =
      { onBinder: \x -> allRules _.onBinder x *> traverseBinder visitor x
      , onExpr: \x -> allRules _.onExpr x *> traverseExpr visitor x
      , onDecl: \x -> allRules _.onDecl x *> traverseDecl visitor x
      , onType: \x -> allRules _.onType x *> traverseType visitor x
      }

    allRules :: ∀ x. (_ -> Handle x _) -> x e -> _
    allRules f x =
      forWithIndex_ rs \rid { rule } ->
        let (Handle h) = f rule
        in h x # mapViolation (merge { rule: rid })

    onModule :: Module e -> _
    onModule m@(Module { header: ModuleHeader header }) = do
      allRules _.onModule m
      for_ header.imports \imp ->
        allRules _.onModuleImport imp
      for_ header.exports \(Wrapped { value: Separated { head, tail } }) -> do
        allRules _.onModuleExport head
        for_ tail \(_ /\ exp) -> allRules _.onModuleExport exp

-- | Given a file path, reads the file, parses it, and runs all the rules on it,
-- | then amends the rules with the `muted :: Boolean` flag based on the muting
-- | directives in the file itself as well as include/exclude globs in each
-- | rule's config. If reading the file or parsing it fails, those conditions
-- | are reported as linter violations, rather than a big loud crash.
checkFile :: forall m. MonadEffect m => RuleSet (WriterT (Violations ()) m) -> FilePath -> WriterT (Violations (WithRule + WithMuted + WithFile + ())) m Unit
checkFile rules path = do
  eText <- liftEffect $ try $ readTextFile UTF8 path
  case eText of
    Left err ->
      tell
        [ { message: "Failed to read the file: " <> Err.message err
          , source: Nothing
          , muted: false
          , rule: ""
          , file: { path, lines: Nothing }
          }
        ]
    Right text -> do
      mapViolation (addMutedAndFile text)
        case parseModule text of
          ParseFailed _ ->
            tell [{ rule: "", source: Nothing, message: "Failed to parse the file" }]
          ParseSucceededWithErrors m _ ->
            runRules rules m
          ParseSucceeded m ->
            runRules rules m

  where
    addMutedAndFile text = \v -> v # merge
      { file
      , muted: isMutedByDirective v || isExcludedByPath v
      }
      where
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
          let
            rule = Map.lookup v.rule rules
            included = rule >>= _.globs.include # maybe true \i -> Glob.test (NEA.toArray i) path
            excluded = rule >>= _.globs.exclude # maybe false \e -> Glob.test (NEA.toArray e) path
          in
            excluded || not included
