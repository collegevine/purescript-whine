module Vscode.Server.Connection
  ( Connection
  , createConnection
  , initialize
  , listen
  , sendDiagnostics
  )
  where

import Prelude

import Control.Promise (Promise)
import Effect (Effect)
import Vscode.Server.Capabilities (ServerCapabilities)
import Vscode.Server.Diagnostic (Diagnostic)
import Vscode.Server.Events (EventHandle, eventHandle)

data Connection :: Type
data Connection

foreign import createConnection :: Effect Connection

foreign import sendDiagnostics :: { uri :: String, diagnostics :: Array Diagnostic } -> Connection -> Promise Unit

foreign import listen :: Connection -> Effect Unit

initialize :: EventHandle Connection {} { capabilities :: ServerCapabilities }
initialize = eventHandle "Initialize"
