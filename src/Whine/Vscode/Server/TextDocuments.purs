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

import Prelude

import Effect (Effect)
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

foreign import create :: Effect TextDocuments

foreign import listen :: TextDocuments -> Connection -> Effect Unit

foreign import textDocumentUri :: TextDocument -> String

foreign import isTextDocument :: Foreign -> Boolean
