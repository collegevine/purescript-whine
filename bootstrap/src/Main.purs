module Whine.Bootstrap.Main
  ( main
  )
  where

import Whine.Runner.Prelude

import Codec.JSON.DecodeError as DecodeError
import Data.Map as Map
import Data.Tuple (uncurry)
import Effect.Class.Console as Console
import Node.ChildProcess.Types as StdIO
import Node.Process (argv)
import Node.Process as Node
import Spago.Generated.BuildInfo as BuildInfo
import Whine.Bootstrap.Cache (cacheDir, hashConfig, rebuildCache, whineCorePackage)
import Whine.Bootstrap.Execa (execa)
import Whine.Bootstrap.JsonCodecs as J
import Whine.Runner.Cli as Cli
import Whine.Runner.Config as Config
import Whine.Runner.FS as FS
import Whine.Runner.Yaml as Yaml
import WhineM (runWhineM)

type Env = { logLevel :: LogSeverity }

main :: Effect Unit
main = launchAff_ do
  args <- Cli.parseCliArgs

  when args.version do
    Console.log BuildInfo.packages."whine-core"
    liftEffect $ Node.exit' 0

  let env = { logLevel: Cli.determineLogLevel args }

  void $ runWhineM env entryPoint

entryPoint :: RunnerM Unit
entryPoint = do
  configText <- FS.readFile "whine.yaml"
  config <- configText # Yaml.parseYaml # lmap DecodeError.basic >>= J.decode Config.configCodec # rightOrDie

  let rulePackages =
        -- If whine-core is specified in config, but without a version, assume the "current" version.
        case Map.lookup (fst whineCorePackage) config.rulePackages of
          Just Config.JustPackage ->
            config.rulePackages # uncurry Map.insert whineCorePackage
          _ ->
            config.rulePackages

  let configHash = hashConfig { rulePackages }
      bundleFile = "bundle-" <> configHash <> ".mjs"
      bundlePath = cacheDir <> "/" <> bundleFile

  unlessM (FS.exists bundlePath) $
    rebuildCache { rulePackages, bundleFile }

  unlessM (FS.exists bundlePath) $
    die "Failed to rebuild Whine cache"

  args <- liftEffect argv
  whineProc <- execa bundlePath (drop 2 args) _
    { stdin = Just StdIO.inherit
    , stdout = Just StdIO.inherit
    , stderr = Just StdIO.inherit
    }

  whineResult <- liftAff whineProc.getResult

  unless (whineResult.exitCode == Just 0) do
    logDebug whineResult.stdout
    logError whineResult.stderr

  exit $ fromMaybe 1 whineResult.exitCode
