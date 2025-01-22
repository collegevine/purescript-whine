module Test.Integration where

import Test.Prelude

import Data.String as String
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
  { copyTree, runWhine, cleanupEnvironment } <- liftEffect $ prepareEnvironment { debug }

  let runTest { caseDir, whineArgs, expectedOutput } = do
        let caseDir' = "integration-tests" // "cases" // caseDir
            expectedOutputDir = caseDir' // expectedOutput
        copyTree caseDir'
        actualOutput <- runWhine whineArgs
        if accept then do
          Console.log $ "Accepting the output for " <> expectedOutputDir
          FS.writeTextFile expectedOutputDir actualOutput
        else do
          goldenOutput <- FS.readTextFile expectedOutputDir
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

prepareEnvironment :: { debug :: Boolean } -> Effect
  { copyTree :: FilePath -> Aff Unit
  , runWhine :: Array String -> Aff String
  , cleanupEnvironment :: Aff Unit
  }
prepareEnvironment { debug } =
  Ref.new Nothing <#> \envDirVar ->
    { copyTree: \sourceDir -> do
        dir <- ensureEnvironmentInitialized envDirVar
        whenM (FS.exists $ dir // "src") $ FS.rmdir (dir // "src")
        FS.copyTree sourceDir dir

        cwd <- FS.cwd
        config <- FS.readTextFile (dir // "whine.yaml")
        FS.writeTextFile (dir // "whine.yaml") $ String.replace (Pattern "PROJECT_PATH") (Replacement cwd) config

    , runWhine: \args -> do
        dir <- ensureEnvironmentInitialized envDirVar
        here <- FS.cwd
        let r = run dir "node" $ [here // "dist/npm/index.js"] <> args
        void r -- Run once to initialize
        r -- Run another time to capture output

    , cleanupEnvironment:
        liftEffect (Ref.read envDirVar) >>= case _ of
          Just dir | debug -> do
            traceLog "Skipping environment cleanup due to debug=true flag"
            traceLog $ "Environment at: " <> dir
          Just dir ->
            FS.rmdir dir
          Nothing ->
            pure unit
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

pathConcat :: String -> String -> String
pathConcat a b = a <> "/" <> b

infixl 5 pathConcat as //
