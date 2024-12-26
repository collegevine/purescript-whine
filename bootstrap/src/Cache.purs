module Whine.Bootstrap.Cache
  ( cacheDir
  , hashConfig
  , rebuildCache
  )
  where

import Whine.Bootstrap.Prelude

import Codec.JSON.DecodeError as DecodeError
import Data.Map as Map
import Data.String as String
import JSON as JSON
import Node.ChildProcess.Types as StdIO
import Spago.Generated.BuildInfo as BuildInfo
import Whine.Bootstrap.FS as FS
import Whine.Bootstrap.JsonCodecs as J
import Whine.Config (PackageSpec(..))
import Whine.Yaml as Yaml

cacheDir = ".whine" :: String

rebuildCache :: { rulePackages :: Map { package :: String } PackageSpec, bundleFile :: FilePath } -> Aff Unit
rebuildCache { rulePackages, bundleFile } = do
  let mainModule = "Main"
      packageName = "whine-cached-bootstrap"

  FS.mkDirP (cacheDir <> "/src")
  FS.writeFile (cacheDir <> "/package.json") "{}"
  FS.writeFile (cacheDir <> "/spago.yaml") $ Yaml.stringify $ J.encode spagoYamlCodec
    { package:
      { name: packageName
      , dependencies:
          (Map.toUnfoldable rulePackages :: Array _) <#> \({ package } /\ spec) ->
            Map.singleton package $ printPackageSpec spec
      , bundle:
        { module: mainModule
        , platform: "node"
        , outfile: bundleFile
        }
      }
    , workspace:
      { extraPackages: Map.fromFoldable do
          { package } /\ spec <- Map.toUnfoldable rulePackages
          case spec of
            LocalPackage p -> [package /\ { path: FS.relative cacheDir p.path }]
            _ -> []
      }
    }

  when (rulePackages # any isLocalPackage) $
    whenM (FS.exists $ cacheDir <> "/spago.lock") $
      FS.unlink (cacheDir <> "/spago.lock") # tryOrDie

  execSuccessOrDie_ "npm install" =<<
    execa "npm" ["install", "spago@0.93", "micromatch", "glob"] _
      { cwd = Just cacheDir
      , stdout = Just StdIO.pipe
      , stderr = Just StdIO.pipe
      }

  moduleGraphJson <- execSuccessOrDie "spago graph modules" =<<
    execa "npx" ["spago", "graph", "modules", "--json"] _
      { cwd = Just cacheDir
      , stdout = Just StdIO.pipe
      , stderr = Just StdIO.pipe
      }

  moduleGraph <- moduleGraphJson.stdout # JSON.parse # lmap DecodeError.basic >>= J.decode moduleGraphCodec # rightOrDie

  let candidateModules = Map.fromFoldable do
        modul /\ { package } <- Map.toUnfoldable moduleGraph :: Array _
        guard $ isJust $ String.stripSuffix (Pattern ".WhineRules") modul
        pure $ package /\ modul

      ruleModules = catMaybes do
        { package } /\ spec <- Map.toUnfoldable rulePackages
        let specifiedModule = case spec of
              LocalPackage p -> p.module
              _ -> Nothing
            inferredModule = Map.lookup package candidateModules
        pure $ specifiedModule <|> inferredModule

  FS.writeFile (cacheDir <> "/src/Main.purs") $
    cachedBundleMainModule { moduleName: mainModule, ruleModules }

  execSuccessOrDie_ "spago bundle" =<<
    execa "npx" ["spago", "bundle"] _
      { cwd = Just cacheDir
      , stdout = Just StdIO.pipe
      , stderr = Just StdIO.pipe
      }

  where
    isLocalPackage = case _ of
      LocalPackage _ -> true
      _ -> false

printPackageSpec ∷ PackageSpec -> String
printPackageSpec = case _ of
  JustPackage -> "*"
  PackageVersion v -> v
  LocalPackage _ -> "*"

cachedBundleMainModule :: { moduleName :: String, ruleModules :: Array String } -> String
cachedBundleMainModule { moduleName, ruleModules } = String.joinWith "\n"
  [ "module " <> moduleName <> " where"
  , "import Whine.Prelude"
  , "import Whine.Types (class MonadRules, RuleFactories)"
  , "import Whine.Run (runWhineAndPrintResultsAndExit)"
  , String.joinWith "\n" ruleModuleImports
  , "main :: Effect Unit"
  , "main = runWhineAndPrintResultsAndExit $ fold"
  , "  [ " <> String.joinWith "\n  ," ruleModuleAccess
  , "  ]"
  ]
  where
    ruleModuleImports = ruleModules <#> \modul -> "import " <> modul <> " as " <> modul
    ruleModuleAccess = ruleModules <#> \modul -> modul <> ".rules :: ∀ m. MonadRules m => RuleFactories m"

hashConfig :: { rulePackages :: Map { package :: String } PackageSpec } -> String
hashConfig { rulePackages } = hashString $ fold
  [ BuildInfo.packages."bootstrap"
  , rulePackages
      # Map.toUnfoldable
      <#> (\({ package } /\ spec) -> package <> ":" <> printPackageSpec spec)
      # String.joinWith ","
  ]

type SpagoYaml =
  { package ::
    { name :: String
    , dependencies :: Array (Map String String)
    , bundle :: { module :: String, platform :: String, outfile :: String }
    }
  , workspace ::
    { extraPackages :: Map String { path :: String }
    }
  }

spagoYamlCodec :: J.Codec SpagoYaml
spagoYamlCodec = J.object
  { package: J.object
    { name: J.string
    , dependencies: J.array (J.strMap J.string)
    , bundle: J.object
      { module: J.string
      , platform: J.string
      , outfile: J.string
      }
    }
  , workspace: J.object
    { extraPackages: J.strMap $ J.object { path: J.string }
    }
  }

moduleGraphCodec :: J.Codec (Map String { package :: String })
moduleGraphCodec = J.strMap $ J.object
  { package: J.string
  }
