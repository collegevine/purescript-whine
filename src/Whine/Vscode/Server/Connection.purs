module Vscode.Server.Connection
  ( Connection
  , createConnection
  , initialize
  , listen
  , sendDiagnostics
  )
  where

import Whine.Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect.Aff.Class (class MonadAff, liftAff)
import Vscode.Server.Capabilities (ServerCapabilities)
import Vscode.Server.Diagnostic (Diagnostic)
import Vscode.Events (EventHandle, eventHandle)

data Connection :: Type
data Connection

createConnection :: ∀ m. MonadEffect m => m Connection
createConnection = liftEffect createConnection_

sendDiagnostics :: ∀ m. MonadAff m => { uri :: String, diagnostics :: Array Diagnostic } -> Connection -> m Unit
sendDiagnostics args = liftAff <<< Promise.toAff <<< sendDiagnostics_ args

listen :: ∀ m. MonadEffect m => Connection -> m Unit
listen = liftEffect <<< listen_

foreign import createConnection_ :: Effect Connection

foreign import sendDiagnostics_ :: { uri :: String, diagnostics :: Array Diagnostic } -> Connection -> Promise Unit

foreign import listen_ :: Connection -> Effect Unit

initialize :: EventHandle Connection {} { capabilities :: ServerCapabilities }
initialize = eventHandle "Initialize"
