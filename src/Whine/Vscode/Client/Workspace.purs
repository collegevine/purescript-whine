module Vscode.Workspace
  ( Uri
  , Workspace
  , WorkspaceFolder
  , didChangeWorkspaceFolders
  , didOpenTextDocument
  , getWorkspaceFolder
  , textDocuments
  , workspace
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Vscode.Events (EventHandle, eventHandle)
import Vscode.Server.TextDocuments (TextDocument)

type Uri = { fsPath :: String, scheme :: String }

type WorkspaceFolder = { uri :: Uri }

data Workspace :: Type
data Workspace

didOpenTextDocument :: EventHandle Workspace { uri :: Uri } Unit
didOpenTextDocument = eventHandle "DidOpenTextDocument"

didChangeWorkspaceFolders :: EventHandle Workspace { added :: Array WorkspaceFolder, removed :: Array WorkspaceFolder } Unit
didChangeWorkspaceFolders = eventHandle "DidChangeWorkspaceFolders"

foreign import workspace :: Workspace

getWorkspaceFolder :: Uri -> Effect (Maybe WorkspaceFolder)
getWorkspaceFolder uri = Nullable.toMaybe <$> runEffectFn1 getWorkspaceFolder_ uri

textDocuments :: Workspace -> Effect (Array TextDocument)
textDocuments = runEffectFn1 textDocuments_

foreign import getWorkspaceFolder_ :: EffectFn1 Uri (Nullable WorkspaceFolder)

foreign import textDocuments_ :: EffectFn1 Workspace (Array TextDocument)
