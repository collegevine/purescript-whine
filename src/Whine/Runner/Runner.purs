module Whine.Runner where

import Whine.Runner.Prelude

import Data.Array.NonEmpty as NEA
import Data.String.NonEmpty as NES
import Effect.Class.Console as Console
import Node.FS.Stats as Stats
import Node.FS.Sync as NodeFS
import Node.Process (exit')
import Whine (checkFile)
import Whine.Print (printViolation)
import Whine.Runner.Cli as Cli
import Whine.Runner.Config (readConfig)
import Whine.Runner.Glob (glob)
import Whine.Runner.LanguageServer (startLanguageServer)
import Whine.Types (RuleFactories, Violations, WithFile, WithMuted, WithRule)
import WhineM (Env, runWhineM)

-- | The main entry point into the linter. It takes some basic parameters and
-- | runs the whole thing: reads the config, parses it, instantiates the rules,
-- | globs the input files, parses them, and runs the rules through every file.
runWhineAndPrintResultsAndExit :: RuleFactories -> Effect Unit
runWhineAndPrintResultsAndExit factories = launchAff_ do
  args <- Cli.parseCliArgs

  let env = { logLevel: Cli.determineLogLevel args }

  case args.command of
    Cli.JustWhine { globs } -> do
      results <- runWhine { factories, globs, configFile: "whine.yaml", env }
      let output = printViolation `mapMaybe` results
      unless args.quiet do
        Console.log `traverse_` output
        when (null output) $ Console.log "No violations found."
      liftEffect $ exit' if null output then 0 else 1

    Cli.LanguageServer { checkWhen } ->
      void $ runWhineM env $
        startLanguageServer { factories, configFile: "whine.yaml", checkWhen }

-- | The main entry point into the linter. It takes some basic parameters and
-- | runs the whole thing: reads the config, parses it, instantiates the rules,
-- | globs the input files, parses them, and runs the rules through every file.
runWhine :: ∀ m. MonadEffect m =>
  { factories :: RuleFactories
  , globs :: Maybe (NonEmptyArray NonEmptyString)
  , configFile :: FilePath
  , env :: Env
  }
  -> m (Violations (WithRule + WithMuted + WithFile + ()))
runWhine { factories, globs, configFile, env } = snd <$> runWhineM env do
  config <- readConfig factories configFile
  files <- gatherFiles (globs' config)
  checkFile config.rules `traverse_` files
  where
    globs' config =
      globs
      <#> NEA.toArray
      <#> { include: _, exclude: [] }
      # fromMaybe config.files

    gatherFiles globses = liftEffect do
      globbed <- glob globses
      { yes: directories, no: files } <-
        globbed
        # traverse (\f -> (f /\ _) <$> NodeFS.stat f)
        <#> partition (snd >>> Stats.isDirectory)
      withinDirectories <- glob
        { include: directories <#> fst <#> (_ <> "/**/*.purs") # mapMaybe NES.fromString
        , exclude: []
        }
      pure $ nub $ (fst <$> files) <> withinDirectories
