module Whine.Bootstrap.Main
  ( main
  )
  where

import Whine.Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Error.Class (class MonadError)
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Map as Map
import Data.String as String
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import Effect.Exception as Err
import JSON as JSON
import Node.ChildProcess.Types as StdIO
import Node.FS.Aff as NodeFS
import Node.FS.Perms (permsReadWrite)
import Node.FS.Sync as NodeFSSync
import Node.Library.Execa (ExecaProcess, ExecaResult, execa)
import Node.Path as Path
import Node.Process (exit', setExitCode)
import Whine.Bootstrap.Hash (hashString)
import Whine.Config (PackageSpec(..))
import Whine.Config as Config
import Whine.Yaml as Yaml

main :: Effect Unit
main = launchAff_ do
  text <- readFile configFile
  config <- text # Yaml.parseYaml # lmap DecodeError.basic >>= CJ.decode Config.configCodec # rightOrDie

  let configHash = hashString $ Yaml.stringify $ CJ.encode Config.configCodec config
      mainModule = "Main.V" <> configHash
      packageName = "whine-bootstrap-" <> configHash
      bundleFile = "bundle-" <> configHash <> ".mjs"
      rulePackages = Map.union config.rulePackages $ Map.singleton { package: "whine-core" } Config.JustPackage

  mkDirP ".whine/src"
  writeFile ".whine/package.json" "{}"
  writeFile ".whine/spago.yaml" $ Yaml.stringify $ CJ.encode spagoYamlCodec
    { package:
      { name: packageName
      , dependencies:
          (Map.toUnfoldable rulePackages :: Array _) <#> \({ package } /\ spec) ->
            case spec of
              JustPackage -> Map.singleton package "*"
              PackageVersion v -> Map.singleton package v
              LocalPackage _ -> Map.singleton package "*"
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
            LocalPackage p -> [package /\ { path: Path.relative ".whine" p.path }]
            _ -> []
      }
    }

  when (rulePackages # any isLocalPackage) $
    whenM (liftEffect $ NodeFSSync.exists ".whine/spago.lock") $
      NodeFS.unlink ".whine/spago.lock" # tryOrDie

  execSuccessOrDie_ "npm install" =<<
    execa "npm" ["install", "spago@0.93", "micromatch", "glob"] _
      { cwd = Just ".whine"
      , stdout = Just StdIO.pipe
      , stderr = Just StdIO.pipe
      }

  moduleGraphJson <- execSuccessOrDie "spago graph modules" =<<
    execa "npx" ["spago", "graph", "modules", "--json"] _
      { cwd = Just ".whine"
      , stdout = Just StdIO.pipe
      , stderr = Just StdIO.pipe
      }

  moduleGraph <- moduleGraphJson.stdout # JSON.parse # lmap DecodeError.basic >>= CJ.decode moduleGraphCodec # rightOrDie

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

      ruleModuleImports = ruleModules <#> \modul -> "import " <> modul <> " as " <> modul
      ruleModuleAccess = ruleModules <#> \modul -> modul <> ".rules :: ∀ m. MonadRules m => RuleFactories m"

  writeFile ".whine/src/Main.purs" $ """
    module """ <> mainModule <> """ where

    import Whine.Prelude
    import Whine.Types (class MonadRules, RuleFactories)
    import Whine.Run (runWhineAndPrintResultsAndExit)
    """ <> String.joinWith "\n" ruleModuleImports <> """

    main :: Effect Unit
    main = runWhineAndPrintResultsAndExit $ fold
      [ """ <> String.joinWith "\n      ," ruleModuleAccess <> """
      ]
  """

  execSuccessOrDie_ "spago bundle" =<<
    execa "npx" ["spago", "bundle"] _
      { cwd = Just ".whine"
      , stdout = Just StdIO.pipe
      , stderr = Just StdIO.pipe
      }

  whineResult <- _.getResult =<<
    execa (".whine/" <> bundleFile) [] _
      { stdout = Just StdIO.inherit
      , stderr = Just StdIO.inherit
      }

  liftEffect $ setExitCode $ fromMaybe 1 whineResult.exitCode

  where
    isLocalPackage = case _ of
      LocalPackage _ -> true
      _ -> false

configFile = "whine.yaml" :: String

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

spagoYamlCodec :: CJ.Codec SpagoYaml
spagoYamlCodec = CJ.Record.object
  { package: CJ.Record.object
    { name: CJ.string
    , dependencies: CJ.array (CJ.Common.strMap CJ.string)
    , bundle: CJ.Record.object
      { module: CJ.string
      , platform: CJ.string
      , outfile: CJ.string
      }
    }
  , workspace: CJ.Record.object
    { extraPackages: CJ.Common.strMap $ CJ.Record.object { path: CJ.string }
    }
  }

moduleGraphCodec :: CJ.Codec (Map String { package :: String })
moduleGraphCodec = CJ.Common.strMap $ CJ.Record.object
  { package: CJ.string
  }

die :: ∀ m err a. MonadEffect m => Loggable err => err -> m a
die message = do
  Console.error $ toDoc message
  liftEffect $ exit' 1

rightOrDie :: ∀ m err a. MonadEffect m => Loggable err => Either err a -> m a
rightOrDie = either die pure

tryOrDie :: ∀ m err a. MonadEffect m => MonadError err m => Loggable err => m a -> m a
tryOrDie = try >=> rightOrDie

execSuccessOrDie :: ∀ m. MonadAff m => String -> ExecaProcess -> m ExecaResult
execSuccessOrDie cmd proc = do
  res <- liftAff $ proc.getResult
  when (res.exitCode /= Just 0) do
    die $ "Error running '" <> cmd <> "':\n" <> show res.stderr
  pure res

execSuccessOrDie_ :: ∀ m. MonadAff m => String -> ExecaProcess -> m Unit
execSuccessOrDie_ cmd res = void $ execSuccessOrDie cmd res

readFile :: ∀ m. MonadAff m => FilePath -> m String
readFile path = liftAff do
  NodeFS.readTextFile UTF8 path # tryOrDie

writeFile :: ∀ m. MonadAff m => FilePath -> String -> m Unit
writeFile path text = liftAff do
  NodeFS.writeTextFile UTF8 path text # tryOrDie

mkDirP :: ∀ m. MonadAff m => FilePath -> m Unit
mkDirP path = liftAff do
  NodeFS.mkdir' path { recursive: true, mode: permsReadWrite } # tryOrDie

class Loggable a where
  toDoc :: a -> String

instance Loggable String where
  toDoc = identity

instance Loggable Err.Error where
  toDoc = Err.message

instance Loggable DecodeError.DecodeError where
  toDoc = DecodeError.print
