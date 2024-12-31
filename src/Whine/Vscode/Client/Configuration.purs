module Vscode.Client.Configuration
  ( Configuration
  , get
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Elmish.Foreign (class CanReceiveFromJavaScript, Foreign, readForeign)

data Configuration :: Type
data Configuration

get :: ∀ a. CanReceiveFromJavaScript a => String -> Configuration -> Effect (Maybe a)
get section c = runEffectFn2 get_ section c <#> readForeign

foreign import get_ :: EffectFn2 String Configuration Foreign
