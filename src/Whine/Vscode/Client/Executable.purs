module Vscode.Client.Executable where

import Foreign.Object as FO
import Untagged.Union (UndefinedOr)

type Executable =
  { command :: String
  , transport :: TransportKind
  , args :: UndefinedOr (Array String)
  , options :: UndefinedOr ExecutableOptions
  }

type ExecutableOptions =
  { cwd :: UndefinedOr String
  , env :: UndefinedOr (FO.Object String)
  , detached :: UndefinedOr Boolean
  , shell :: UndefinedOr Boolean
  }

newtype TransportKind = TransportKind Int

transportKind ::
  { stdio :: TransportKind
  , ipc :: TransportKind
  , pipe ::  TransportKind
  , socket ::  TransportKind
  }
transportKind =
  { stdio: TransportKind 0
  , ipc: TransportKind 1
  , pipe: TransportKind 2
  , socket: TransportKind 3
  }
