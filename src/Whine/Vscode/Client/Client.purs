module Vscode.Client
  ( ClientOptions
  , DocumentSelector
  , LanguageClient
  , ServerOptions
  , createLanguageClient
  , start
  , stop
  )
  where

import Prelude

import Effect (Effect)
import Untagged.Union (UndefinedOr)
import Vscode.Client.Executable (Executable)
import Vscode.Client.FileSystemWatcher (FileSystemWatcher)
import Vscode.Workspace (WorkspaceFolder)

data LanguageClient :: Type
data LanguageClient

type ClientOptions =
  { documentSelector :: Array DocumentSelector
  , synchronize :: { fileEvents :: UndefinedOr FileSystemWatcher }
  , workspaceFolder :: WorkspaceFolder
  }

type DocumentSelector =
  { language :: UndefinedOr String
  , scheme :: UndefinedOr String
  , pattern :: UndefinedOr String
  }

type ServerOptions = { run :: Executable }

foreign import createLanguageClient ::
  { id :: String
  , name :: String
  , serverOptions :: ServerOptions
  , clientOptions :: ClientOptions
  }
  -> Effect LanguageClient

foreign import start :: LanguageClient -> Effect Unit

foreign import stop :: LanguageClient -> Effect Unit
