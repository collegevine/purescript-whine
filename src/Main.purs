module Main
  ( main
  )
  where

import Prelude

import Data.Array (drop, mapMaybe)
import Data.Foldable (any, traverse_)
import Effect (Effect)
import Effect.Console as Console
import Node.Process (argv, setExitCode)
import Whine.Print (printViolation)
import Whine.Rules.BadModules (badModules)
import Whine.Rules.CaseBranchIndentation (caseBranchIndentation)
import Whine.Rules.CommaFirstArrays (commaFirstArrays)
import Whine.Run (runLint)
import Whine.Types ((=:>))

main :: Effect Unit
main = do

  let factories =
        [ "BadModules" =:> badModules
        , "CommaFirstArrays" =:> commaFirstArrays
        , "CaseBranchIndentation" =:> caseBranchIndentation
        ]

  globs <- drop 2 <$> argv
  res <- runLint { factories, globs, configFile: "whine.yaml" }

  Console.log `traverse_` (printViolation `mapMaybe` res)

  setExitCode if res # any (not _.muted) then 1 else 0
