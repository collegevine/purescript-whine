module Whine.Glob (glob, test) where

import Whine.Prelude
import Effect.Uncurried (EffectFn1, runEffectFn1)

glob :: String -> Effect (Array String)
glob = runEffectFn1 globSync

foreign import globSync :: EffectFn1 String (Array String)

foreign import test :: Array String -> FilePath -> Boolean
