module Whine.Runner.Client.Main where

import Whine.Prelude

import Data.Map as Map
import Data.String as String
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Node.FS.Sync as NodeFS
import Untagged.Castable (cast)
import Vscode.Client as Client
import Vscode.Client.Executable (transportKind)
import Vscode.Client.Configuration as Config
import Vscode.Context (ExtensionContext)
import Vscode.Events (on)
import Vscode.Client.Workspace as Workspace

activate :: EffectFn1 ExtensionContext Unit
activate = mkEffectFn1 \_ctx -> do
  clients <- Ref.new Map.empty
  settings <- getSettings

  let ensureClient doc =
        when (doc.uri.scheme == "file") do
          trace_ $ "Ensuring client for " <> doc.uri.fsPath
          maybeFolder <- Workspace.getWorkspaceFolder doc.uri
          for_ maybeFolder \folder -> do
            let folderPath = folder.uri.fsPath
            trace_ $ "Found folder " <> folderPath
            cc <- Ref.read clients
            unless (Map.member folderPath cc) do
              trace_ $ "No client for " <> folderPath
              hasConfig <- NodeFS.exists $ folderPath <> "/whine.yaml"
              if hasConfig then do
                trace_ $ "Found whine.yaml in " <> folderPath
                client <- startLanguageClient settings folder
                clients # Ref.modify_ (Map.insert folderPath $ Just client)
              else
                clients # Ref.modify_ (Map.insert folderPath Nothing)

  Workspace.workspace # on Workspace.didOpenTextDocument ensureClient

  Workspace.workspace # on Workspace.didChangeWorkspaceFolders \{ removed } -> do
    for_ removed \folder -> do
      cc <- clients # Ref.read
      Client.stop `traverse_` join (Map.lookup folder.uri.fsPath cc)
      clients # Ref.modify_ (Map.delete folder.uri.fsPath)

  where
    startLanguageClient { command, args } folder = do
      client <- Client.createLanguageClient $ cast
        { id: "purescript-whine"
        , name: "Whine at PureScript"
        , clientOptions:
          { documentSelector:
              [ cast { scheme: "file", language: "purescript", pattern: folder.uri.fsPath <> "/**/*.purs" } :: Client.DocumentSelector
              ]
          , synchronize: {}
          , workspaceFolder: folder
          }
        , serverOptions:
          { run: { command, args, transport: transportKind.stdio }
          }
        }

      Client.start client
      pure client

    getSettings = liftEffect do
      section <- Workspace.workspace # Workspace.getConfiguration "whine"

      cmd <- section # Config.get "languageServerStartCommand" <#> fromMaybe ""
      checkOn <- section # Config.get "checkOn" <#> fromMaybe "save"

      let command /\ args =
            case cmd # String.split (Pattern " ") # uncons of
              Just u -> u.head /\ u.tail
              Nothing -> "npx" /\ ["-y", "whine", "language-server", "--quiet"]

      pure { command, args: args <> ["--check-on=" <> checkOn] }

foreign import trace_ :: String -> Effect Unit
