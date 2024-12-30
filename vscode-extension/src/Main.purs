module Whine.Runner.Client.Main where

import Prelude

import Data.Foldable (for_, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Node.FS.Sync as NodeFS
import Untagged.Castable (cast)
import Vscode.Client (DocumentSelector, createLanguageClient, start, stop)
import Vscode.Client.Executable (transportKind)
import Vscode.Context (ExtensionContext)
import Vscode.Events (on)
import Vscode.Workspace as Workspace

activate :: EffectFn1 ExtensionContext Unit
activate = mkEffectFn1 \_ctx -> do
  clients <- Ref.new Map.empty

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
                client <- createClient folder
                clients # Ref.modify_ (Map.insert folderPath $ Just client)
              else
                clients # Ref.modify_ (Map.insert folderPath Nothing)

  Workspace.workspace # on Workspace.didOpenTextDocument ensureClient

  Workspace.workspace # on Workspace.didChangeWorkspaceFolders \{ removed } -> do
    for_ removed \folder -> do
      cc <- clients # Ref.read
      stop `traverse_` join (Map.lookup folder.uri.fsPath cc)
      clients # Ref.modify_ (Map.delete folder.uri.fsPath)

  where
    createClient folder = do
      client <- createLanguageClient $ cast
        { id: "purescript-whine"
        , name: "Whine at PureScript"
        , clientOptions:
          { documentSelector:
              [ cast { scheme: "file", language: "purescript", pattern: folder.uri.fsPath <> "/**/*.purs" } :: DocumentSelector
              ]
          , synchronize: {}
          , workspaceFolder: folder
          }
        , serverOptions:
          { run:
            { command: "npx"
            , transport: transportKind.stdio
            , args: ["-y", "whine", "language-server", "--quiet"]
            }
          }
        }

      start client
      pure client

foreign import trace_ :: String -> Effect Unit
