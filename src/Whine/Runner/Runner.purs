module Whine.Runner where

import Whine.Runner.Prelude

import Data.Array.NonEmpty as NEA
import Effect.Class.Console as Console
import Node.Process (exit')
import Record (merge)
import Whine (checkFile)
import Whine.Print (printViolation)
import Whine.Runner.Cli as Cli
import Whine.Runner.Config (readConfig)
import Whine.Runner.Glob (glob)
import Whine.Runner.LanguageServer (startLanguageServer)
import Whine.Types (RuleFactories, Violations, WithFile, WithMuted, WithRule, mapViolation)

-- | The main entry point into the linter. It takes some basic parameters and
-- | runs the whole thing: reads the config, parses it, instantiates the rules,
-- | globs the input files, parses them, and runs the rules through every file.
runWhineAndPrintResultsAndExit :: RuleFactories (WriterT (Violations ()) RunnerM) -> Effect Unit
runWhineAndPrintResultsAndExit factories = launchAff_ do
  args <- Cli.parseCliArgs

  let env = { logLevel: Cli.determineLogLevel args }
      main = case args.command of
        Cli.JustWhine { globs } -> do
          results <- runWhine { factories, globs, configFile: "whine.yaml" }
          unless args.quiet $
            Console.log `traverse_` (printViolation `mapMaybe` results)
          liftEffect $ exit' if results # any (not _.muted) then 1 else 0

        Cli.LanguageServer { checkWhen } ->
          startLanguageServer { factories, configFile: "whine.yaml", checkWhen }

  runReaderT main env

-- | The main entry point into the linter. It takes some basic parameters and
-- | runs the whole thing: reads the config, parses it, instantiates the rules,
-- | globs the input files, parses them, and runs the rules through every file.
runWhine :: ∀ m. MonadEffect m =>
  { factories :: RuleFactories (WriterT (Violations ()) m)
  , globs :: Maybe (NonEmptyArray NonEmptyString)
  , configFile :: FilePath
  }
  -> m (Violations (WithRule + WithMuted + WithFile + ()))
runWhine { factories, globs, configFile } = execWriterT do
  config <- mapViolation (merge { muted: false }) $ readConfig factories configFile
  files <- liftEffect $ glob (globs' config)
  checkFile config.rules `traverse_` files
  where
    globs' config =
      globs
      <#> NEA.toArray
      <#> { include: _, exclude: [] }
      # fromMaybe config.files
