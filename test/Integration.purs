module Test.Integration where

import Test.Prelude

import Data.String as String
import Data.String.Regex (replace) as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Debug (traceM)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.ChildProcess.Types as IO
import Node.Library.Execa as Execa
import Test.FS (newTempDir)
import Test.FS as FS
import Test.Spec (Spec, afterAll, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Reads the contents of `/integration-tests/cases` and turns each
-- | subdirectory into a test case. See `/integration-tests/cases/README` for
-- | more details.
integrationSpecs :: { debug :: Boolean, accept :: Boolean } -> Aff (Spec Unit)
integrationSpecs { debug, accept } = do
  { copyTree, runWhine, cleanupEnvironment, patchProjectPathIn, testDir } <- liftEffect $ prepareEnvironment { debug }

  let runTest { caseDir, whineArgs, expectedOutput } = do
        let caseDir' = "integration-tests" // "cases" // caseDir
        copyTree caseDir'
        runWhineAndCheckOutput (caseDir' // expectedOutput) whineArgs

      runWhineAndCheckOutput expectedOutputPath whineArgs = do
        void $ runWhine whineArgs -- Run once to initialize
        actualOutput <- runWhine whineArgs -- Run a second time to capture output
        checkOutput actualOutput expectedOutputPath

      checkOutput actualOutput expectedOutputPath = do
        traceM { actualOutput }
        if accept then do
          Console.log $ "Accepting the output for " <> expectedOutputPath
          FS.writeTextFile expectedOutputPath actualOutput
        else do
          goldenOutput <- FS.readTextFile expectedOutputPath
          String.trim actualOutput `shouldEqual` String.trim goldenOutput

  pure $ afterAll (\_ -> cleanupEnvironment) $
    describe "Integration tests" do

      it "Runs normally" do
        runTest { caseDir: "0-normal", whineArgs: [], expectedOutput: "baseline.txt" }

      it "Respects output option" do
        runTest { caseDir: "0-normal", whineArgs: ["--output=long"], expectedOutput: "baseline.txt" }
        runTest { caseDir: "0-normal", whineArgs: ["--output=short"], expectedOutput: "output-short.txt" }

      it "Respects quiet option" do
        runTest { caseDir: "0-normal", whineArgs: ["--quiet"], expectedOutput: "quiet.txt" }

      it "Respects provided globs" do
        runTest { caseDir: "0-normal", whineArgs: ["**/*/A.purs"], expectedOutput: "glob-only-a.txt" }
        runTest { caseDir: "0-normal", whineArgs: ["src/nested/**/*.purs"], expectedOutput: "glob-only-nested.txt" }
        runTest { caseDir: "0-normal", whineArgs: ["src/nested/doubly-nested/**/*.purs"], expectedOutput: "glob-only-doubly-nested.txt" }
        runTest { caseDir: "0-normal", whineArgs: ["src/nested/**/*.purs", "**/*/B.purs"], expectedOutput: "glob-nested-and-b.txt" }

      it "Picks up and runs local rules" do
        let caseDir = "integration-tests/cases/1-local-rules"
        dir <- testDir
        copyTree caseDir
        patchProjectPathIn "spago.yaml"
        runWhineAndCheckOutput (caseDir // "baseline.txt") []

        FS.unlink (dir // "whine.yaml")
        FS.copyFile (caseDir // "whine-with-local-rule-config.yaml") (dir // "whine.yaml")
        patchProjectPathIn "whine.yaml"
        runWhineAndCheckOutput (caseDir // "with-local-rule-config.txt") []

        -- Run multiple times, make sure there is the same input
        for_ [1,2,3] \_ -> do
          output1 <- runWhine ["--debug"]
          checkOutput (patchTime $ patchLatestDependency output1) (caseDir // "with-local-rule-config-debug.txt")

        -- Now touch the local rules source, see if Whine detects the timestamp
        -- change and rebuilds the cached bundle
        FS.readTextFile (dir // "src/WhineRules.purs")
          <#> String.replace (Pattern "Local rule") (Replacement "Local rule changed")
          >>= FS.writeTextFile (dir // "src/WhineRules.purs")
        output2 <- runWhine ["--debug"]
        checkOutput (patchTime output2) (caseDir // "with-local-rule-changed.txt")

  where
    patchTime = Regex.replace timeRegex "<TIMESTAMP>"
    timeRegex = Regex.unsafeRegex
      "(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{1,3})|(\\[\\d{2}:\\d{2}:\\d{2}\\.\\d{3}\\])"
      RegexFlags.global

    patchLatestDependency = Regex.replace latestDependencyRegex "latest dependency <SNIP> time is"
    latestDependencyRegex = Regex.unsafeRegex "latest dependency [^ ]+ time is" RegexFlags.global

prepareEnvironment :: { debug :: Boolean } -> Effect
  { copyTree :: FilePath -> Aff Unit
  , runWhine :: Array String -> Aff String
  , cleanupEnvironment :: Aff Unit
  , patchProjectPathIn :: FilePath -> Aff Unit
  , testDir :: Aff FilePath
  }
prepareEnvironment { debug } =
  Ref.new Nothing <#> \envDirVar ->
    { copyTree: \sourceDir -> do
        dir <- ensureEnvironmentInitialized envDirVar
        whenM (FS.exists $ dir // "src") $ FS.rmdir (dir // "src")
        FS.copyTree sourceDir dir
        patchProjectPathIn (dir // "whine.yaml")

    , runWhine: \args -> do
        dir <- ensureEnvironmentInitialized envDirVar
        here <- FS.cwd
        run dir "node" $ [here // "dist/npm/index.js"] <> args

    , cleanupEnvironment:
        liftEffect (Ref.read envDirVar) >>= case _ of
          Just dir | debug -> do
            traceLog "Skipping environment cleanup due to debug=true flag"
            traceLog $ "Environment at: " <> dir
          Just dir ->
            FS.rmdir dir
          Nothing ->
            pure unit

    , patchProjectPathIn: \file -> do
        dir <- ensureEnvironmentInitialized envDirVar
        patchProjectPathIn (dir // file)

    , testDir: ensureEnvironmentInitialized envDirVar
    }
  where
    traceLog
      | debug = Console.log
      | otherwise = const $ pure unit

    ensureEnvironmentInitialized envDirVar =
      liftEffect (Ref.read envDirVar) >>= case _ of
        Just d ->
          pure d
        Nothing -> do
          dir <- newTempDir
          liftEffect $ Ref.write (Just dir) envDirVar
          traceLog $ "Preparing environment in: " <> dir
          FS.copyTree "integration-tests/env-template" dir
          pure dir

    run cwd cmd args = do
      traceLog $ "Running: " <> String.joinWith " " ([cmd] <> args)
      res <- _.getResult =<< Execa.execa cmd args _ { cwd = Just cwd, stdout = Just IO.pipe, stderr = Just IO.pipe }
      case res.exitCode of
        Just 0 -> do
          pure res.stdout
        Just 1 -> do -- Exit code 1 is when violations are found
          pure res.stdout
        _ -> do
          Console.error res.stderr
          pure ""

    patchProjectPathIn file =
      whenM (FS.exists file) do
        cwd <- FS.cwd
        config <- FS.readTextFile file
        FS.writeTextFile file $ String.replace (Pattern "PROJECT_PATH") (Replacement cwd) config


pathConcat :: String -> String -> String
pathConcat a b = a <> "/" <> b

infixl 5 pathConcat as //
