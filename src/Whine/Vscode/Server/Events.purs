module Vscode.Server.Events
  ( EventHandle
  , class EventResult
  , eventHandle
  , on
  )
  where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Effect (Effect)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn3)
import Elmish.Foreign (class CanPassToJavaScript, class CanReceiveFromJavaScript, Foreign, readForeign')
import Foreign (unsafeToForeign)

newtype EventHandle :: Type -> Type -> Type -> Type
newtype EventHandle target a result = EventHandle String

eventHandle :: ∀ @target @a @result. CanReceiveFromJavaScript a => EventResult result => String -> EventHandle target a result
eventHandle = EventHandle

on :: ∀ target a result. CanReceiveFromJavaScript a => EventResult result =>
  EventHandle target a result -> (a -> Effect result) -> target -> Effect Unit
on (EventHandle event) f =
  runEffectFn3 unsafeOn event $
    mkEffectFn1 $ readForeign' >>> either (throwError <<< error) (map unsafeToForeign <<< f)

foreign import unsafeOn :: ∀ target. EffectFn3 String (EffectFn1 Foreign Foreign) target Unit

class EventResult (a :: Type)
instance EventResult Unit
else instance CanPassToJavaScript a => EventResult a
