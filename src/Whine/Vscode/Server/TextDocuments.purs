module Vscode.Server.TextDocuments
  ( TextDocument
  , TextDocuments
  , create
  , didChangeContent
  , didSave
  , listen
  , textDocumentUri
  )
  where

import Whine.Prelude

import Elmish.Foreign (class CanReceiveFromJavaScript, Foreign, ValidationResult(..))
import Vscode.Server.Connection (Connection)
import Vscode.Server.Events (EventHandle, eventHandle)

data TextDocuments :: Type
data TextDocuments

data TextDocument :: Type
data TextDocument

instance CanReceiveFromJavaScript TextDocument where
  validateForeignType x
    | isTextDocument x = Valid
    | otherwise = Invalid { path: "", expected: "TextDocuments", got: x }

didChangeContent :: EventHandle TextDocuments { document :: TextDocument } Unit
didChangeContent = eventHandle "DidChangeContent"

didSave :: EventHandle TextDocuments { document :: TextDocument } Unit
didSave = eventHandle "DidSave"

create :: forall m. MonadEffect m => m TextDocuments
create = liftEffect create_

listen :: forall m. MonadEffect m => TextDocuments -> Connection -> m Unit
listen d c = liftEffect $ listen_ d c

foreign import create_ :: Effect TextDocuments

foreign import listen_ :: TextDocuments -> Connection -> Effect Unit

foreign import textDocumentUri :: TextDocument -> String

foreign import isTextDocument :: Foreign -> Boolean
