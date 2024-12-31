module Vscode.Client.Workspace
  ( Uri
  , Workspace
  , WorkspaceFolder
  , didChangeWorkspaceFolders
  , didOpenTextDocument
  , getConfiguration
  , getWorkspaceFolder
  , workspace
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Vscode.Client.Configuration (Configuration)
import Vscode.Events (EventHandle, eventHandle)

type Uri = { fsPath :: String, scheme :: String }

type WorkspaceFolder = { uri :: Uri }

data Workspace :: Type
data Workspace

didOpenTextDocument :: EventHandle Workspace { uri :: Uri } Unit
didOpenTextDocument = eventHandle "DidOpenTextDocument"

didChangeWorkspaceFolders :: EventHandle Workspace { added :: Array WorkspaceFolder, removed :: Array WorkspaceFolder } Unit
didChangeWorkspaceFolders = eventHandle "DidChangeWorkspaceFolders"

getWorkspaceFolder :: Uri -> Effect (Maybe WorkspaceFolder)
getWorkspaceFolder uri = Nullable.toMaybe <$> runEffectFn1 getWorkspaceFolder_ uri

getConfiguration :: String -> Workspace -> Effect Configuration
getConfiguration = runEffectFn2 getConfiguration_

foreign import getWorkspaceFolder_ :: EffectFn1 Uri (Nullable WorkspaceFolder)

foreign import getConfiguration_ :: EffectFn2 String Workspace Configuration

foreign import workspace :: Workspace
