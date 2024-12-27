module Vscode.Client
  ( ClientOptions
  , DocumentSelector
  , LanguageClient
  , ServerOptions
  , createLanguageClient
  , start
  )
  where

import Prelude

import Effect (Effect)
import Untagged.Union (UndefinedOr)
import Vscode.Client.Executable (Executable)
import Vscode.Client.FileSystemWatcher (FileSystemWatcher)

data LanguageClient :: Type
data LanguageClient

type ClientOptions =
  { documentSelector :: Array DocumentSelector
  , synchronize :: { fileEvents :: UndefinedOr FileSystemWatcher }
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
