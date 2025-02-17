module Whine.Bootstrap.Cache
  ( cacheDir
  , dependenciesChanged
  , hashConfig
  , rebuildCache
  , whineCorePackage
  )
  where

import Whine.Runner.Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Reader (asks)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as Fmt
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.String as String
import Data.Tuple (uncurry)
import Data.UUID as UUID
import JSON as JSON
import Node.ChildProcess.Types as StdIO
import Node.Path as NodePath
import Partial.Unsafe (unsafePartial)
import Spago.Generated.BuildInfo as BuildInfo
import Whine.Bootstrap.Execa (execResultSuccessOrDie, execSuccessOrDie_, execa)
import Whine.Bootstrap.Hash (hashString)
import Whine.Bootstrap.JsonCodecs as J
import Whine.Runner.Config (PackageSpec(..))
import Whine.Runner.FS as FS
import Whine.Runner.PackageVersion (formatVersion, formatVersionRange, parseVersion, versionToRange)
import Whine.Runner.Yaml as Yaml

cacheDir = ".whine" :: String

rebuildCache ::
  { rulePackages :: Map { package :: String } PackageSpec
  , bundleFile :: FilePath
  }
  -> RunnerM Unit
rebuildCache { rulePackages, bundleFile } = do
  logInfo "Please hold on, preparing to whine..."
  logInfo "Applying artificial tears..."

  unique <- liftEffect $ String.replaceAll (Pattern "-") (Replacement "") <$> UUID.toString <$> UUID.genUUID

  let mainModule = "Main" <> unique
      packageName = "whine-cached-bootstrap"
      dependencies = Map.union rulePackages (uncurry Map.singleton whineCorePackage)

  FS.mkDirP (cacheDir <> "/src")
  FS.writeFile (cacheDir <> "/package.json") "{}"
  FS.writeFile (cacheDir <> "/spago.yaml") $ Yaml.stringify $ J.encode spagoYamlCodec
    { package:
      { name: packageName
      , dependencies:
          (Map.toUnfoldable dependencies :: Array _) <#> \({ package } /\ spec) ->
            Map.singleton package case spec of
              JustPackage -> "*"
              PackageVersion v -> formatVersionRange $ versionToRange v
              LocalPackage _ -> "*"
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

  logSameLine "Coming up with excuses..."
  execSuccessOrDie_ "npm install" =<<
    execa "npm"
      [ "install"
      , "spago@" <> BuildInfo.spagoVersion
      , "purescript@" <> BuildInfo.pursVersion
      , "micromatch"
      , "glob"
      , "vscode-languageserver"
      , "vscode-languageserver-textdocument"
      ]
      _
        { cwd = Just cacheDir
        , stdout = Just StdIO.pipe
        , stderr = Just StdIO.pipe
        }

  logSameLine "Making a pitiful face..."
  moduleGraphJson <- spagoGraphModules
  moduleGraph <- moduleGraphJson # JSON.parse # lmap DecodeError.basic >>= J.decode moduleGraphCodec # rightOrDie

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

  logSameLine "Revisiting complaints..."
  execSuccessOrDie_ "spago bundle" =<<
    execa "npx" ["spago", "bundle", "--source-maps"] _
      { cwd = Just cacheDir
      , stdout = Just StdIO.pipe
      , stderr = Just StdIO.pipe
      }

  logSameLine "Done, ready to whine."
  logInfo ""
  where
    isLocalPackage = case _ of
      LocalPackage _ -> true
      _ -> false

    logSameLine msg = do
      logLevel <- asks _.logLevel
      let prefix = if logLevel == LogDebug then "" else "\x1B[1A\x1B[K"
      logInfo $ prefix <> msg

    spagoGraphModules = do
      proc <-
        execa "npx" ["spago", "graph", "modules", "--json"] _
          { cwd = Just cacheDir
          , stdout = Just StdIO.pipe
          , stderr = Just StdIO.pipe
          }

      res <- liftAff proc.getResult

      unless (res.exitCode == Just 0) do
        logDebug "'spago graph modules' failed. Trying to build to see what the error was..."
        FS.writeFile (cacheDir <> "/src/Main.purs") "module M where\nx = 42 :: Int"
        execSuccessOrDie_ "spago build" =<<
          execa "npx" ["spago", "build"] _
            { cwd = Just cacheDir
            , stdout = Just StdIO.pipe
            , stderr = Just StdIO.pipe
            }

        logDebug "Building succeeded, but 'spago graph modules' still failed."
        execResultSuccessOrDie "spago graph modules" res

      pure res.stdout

whineCorePackage :: { package :: String } /\ PackageSpec
whineCorePackage = { package: "whine-core" } /\ PackageVersion version
  where
    version = unsafePartial $ fromJust $ hush $
      parseVersion BuildInfo.packages."whine-core"

cachedBundleMainModule :: { moduleName :: String, ruleModules :: Array String } -> String
cachedBundleMainModule { moduleName, ruleModules } = String.joinWith "\n"
  [ "module " <> moduleName <> " where"
  , "import Whine.Prelude"
  , "import Whine.Types (class MonadRules, RuleFactories)"
  , "import Whine.Runner (runWhineAndPrintResultsAndExit)"
  , String.joinWith "\n" ruleModuleImports
  , "main :: Effect Unit"
  , "main = runWhineAndPrintResultsAndExit $ fold"
  , "  [ " <> String.joinWith "\n  ," ruleModuleAccess
  , "  ]"
  ]
  where
    ruleModuleImports = ruleModules <#> \modul -> "import " <> modul <> " as " <> modul
    ruleModuleAccess = ruleModules <#> \modul -> modul <> ".rules :: RuleFactories"

hashConfig :: { rulePackages :: Map { package :: String } PackageSpec } -> String
hashConfig { rulePackages } = hashString $ fold
  [ BuildInfo.packages."whine-core"
  , rulePackages
      # Map.toUnfoldable
      <#> (\({ package } /\ spec) -> package <> ":" <> printPackageSpec spec)
      # String.joinWith "\n"
  ]
  where
    printPackageSpec = case _ of
      JustPackage -> "*"
      PackageVersion v -> formatVersion v
      LocalPackage p -> p.path <> ":" <> fromMaybe "" p.module

dependenciesChanged :: FilePath -> FilePath -> RunnerM Boolean
dependenciesChanged cwd mapFile = do
  logDebug $ "Checking dependency timestamps based on " <> mapFile
  readMapFile >>= case _ of
    Nothing -> do
      logDebug $ mapFile <> " doesn't exist. Assuming dependencies have changed."
      pure true
    Just { sources } -> do
      let cleanSources = sources # filter (not ignored)
      { modifiedTime: mapFileTime } <- FS.stat mapFilePath

      fileStats <- for cleanSources \source -> FS.stat (cwd <> "/" <> source) <#> _.modifiedTime <#> (_ /\ source)
      let mLatestSource = maximum fileStats

      logDebug $ fold
        [ mapFile, " last modified at ", formatTime mapFileTime
        , ", latest dependency ", maybe "<none>" snd mLatestSource
        , " time is ", maybe "<none>" (formatTime <<< fst) mLatestSource
        ]
      pure $ mLatestSource # maybe true \(t /\ _) -> mapFileTime < t
  where
    mapFilePath = cwd <> "/" <> mapFile

    readMapFile =
      ifM (FS.exists mapFilePath)
        (Just <$> (decodeMapFile =<< FS.readFile mapFilePath))
        (pure Nothing)

    decodeMapFile content = rightOrDie do
      json <- JSON.parse content
      J.decode sourceMapCodec json # lmap DecodeError.print

    ignored file =
      head (String.split (Pattern NodePath.sep) file) # maybe false (_ `elem` ignore)

    ignore =
      ["node_modules", "<stdin>", ".spago"]

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

sourceMapCodec :: J.Codec { sources :: Array String }
sourceMapCodec = J.object { sources: J.array J.string }

formatTime :: DateTime -> String
formatTime = Fmt.format $ List.fromFoldable
  [ Fmt.YearFull
  , Fmt.Placeholder "-"
  , Fmt.MonthTwoDigits
  , Fmt.Placeholder "-"
  , Fmt.DayOfMonthTwoDigits
  , Fmt.Placeholder "T"
  , Fmt.Hours24
  , Fmt.Placeholder ":"
  , Fmt.MinutesTwoDigits
  , Fmt.Placeholder ":"
  , Fmt.SecondsTwoDigits
  , Fmt.Placeholder "."
  , Fmt.Milliseconds
  ]