module Whine.Bootstrap.Main
  ( main
  )
  where

import Whine.Bootstrap.Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Map as Map
import Effect.Class.Console as Console
import Node.ChildProcess.Types as StdIO
import Node.Process (argv)
import Spago.Generated.BuildInfo as BuildInfo
import Whine.Bootstrap.Cache (cacheDir, hashConfig, rebuildCache)
import Whine.Bootstrap.FS as FS
import Whine.Bootstrap.JsonCodecs as J
import Whine.Cli as Cli
import Whine.Config as Config
import Whine.Yaml as Yaml

type Env = { logLevel :: LogSeverity }

main :: Effect Unit
main = launchAff_ do
  args <- Cli.parseCliArgs

  when args.version do
    Console.log BuildInfo.packages."whine-core"
    exit 0

  let env = { logLevel: determineLogLevel args }

  runReaderT entryPoint env

  where
    determineLogLevel args =
      if args.debug then LogDebug
      else if args.quiet then LogError
      else LogInfo

entryPoint :: ReaderT Env Aff Unit
entryPoint = do
  text <- FS.readFile "whine.yaml"
  config <- text # Yaml.parseYaml # lmap DecodeError.basic >>= J.decode Config.configCodec # rightOrDie

  let rulePackages = Map.union config.rulePackages $ Map.singleton { package: "whine-core" } Config.JustPackage
      configHash = hashConfig { rulePackages }
      bundleFile = "bundle-" <> configHash <> ".mjs"
      bundlePath = cacheDir <> "/" <> bundleFile

  unlessM (FS.exists bundlePath) $
    rebuildCache { rulePackages, bundleFile }

  unlessM (FS.exists bundlePath) $
    die "Failed to rebuild Whine cache"

  args <- liftEffect argv
  whineResult <- liftAff $ _.getResult =<<
    execa bundlePath (drop 2 args) _
      { stdout = Just StdIO.inherit
      , stderr = Just StdIO.inherit
      }

  exit $ fromMaybe 1 whineResult.exitCode
