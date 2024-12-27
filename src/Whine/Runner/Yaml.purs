module Whine.Runner.Yaml
  ( parseYaml
  , stringify
  )
  where

import Whine.Runner.Prelude

import Data.Function.Uncurried (Fn3, runFn3)

parseYaml :: String -> Either String JSON
parseYaml = runFn3 parseYaml_ Left Right

foreign import parseYaml_ :: ∀ r. Fn3 (String -> r) (JSON -> r) String r

foreign import stringify :: JSON -> String
