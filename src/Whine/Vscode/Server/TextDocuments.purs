module Vscode.Server.TextDocuments
  ( TextDocument
  , TextDocuments
  , create
  , didChangeContent
  , didOpen
  , didSave
  , getText
  , listen
  , uri
  )
  where

import Whine.Prelude

import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Elmish.Foreign (class CanReceiveFromJavaScript, Foreign, ValidationResult(..))
import Vscode.Server.Connection (Connection)
import Vscode.Events (EventHandle, eventHandle)

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

didOpen :: EventHandle TextDocuments { document :: TextDocument } Unit
didOpen = eventHandle "DidOpen"

create :: ∀ m. MonadEffect m => m TextDocuments
create = liftEffect create_

listen :: ∀ m. MonadEffect m => TextDocuments -> Connection -> m Unit
listen d c = liftEffect $ runEffectFn2 listen_ d c

getText :: ∀ m. MonadEffect m => TextDocument -> m String
getText = liftEffect <<< runEffectFn1 getText_

foreign import create_ :: Effect TextDocuments

foreign import listen_ :: EffectFn2 TextDocuments Connection Unit

foreign import uri :: TextDocument -> String

foreign import getText_ :: EffectFn1 TextDocument String

foreign import isTextDocument :: Foreign -> Boolean
