module Whine.Yaml (parseYaml) where

import Whine.Prelude
import Data.Function.Uncurried (Fn3, runFn3)

parseYaml :: String -> Either String Foreign
parseYaml = runFn3 parseYaml_ Left Right

foreign import parseYaml_ :: ∀ r. Fn3 (String -> r) (Foreign -> r) String r
