module Whine.Bootstrap.Main
  ( main
  )
  where

import Whine.Bootstrap.Prelude

import Codec.JSON.DecodeError as DecodeError
import Data.Map as Map
import Node.ChildProcess.Types as StdIO
import Node.Process (exit')
import Whine.Bootstrap.Cache (cacheDir, hashConfig, rebuildCache)
import Whine.Bootstrap.FS as FS
import Whine.Bootstrap.JsonCodecs as J
import Whine.Config as Config
import Whine.Yaml as Yaml

main :: Effect Unit
main = launchAff_ do
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

  whineResult <- _.getResult =<<
    execa bundlePath [] _
      { stdout = Just StdIO.inherit
      , stderr = Just StdIO.inherit
      }

  liftEffect $ exit' $ fromMaybe 1 whineResult.exitCode
