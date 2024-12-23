module Whine.Core.WhineRules
  ( rules
  )
  where

import Data.Codec.JSON as CJ
import Whine.Core.BadModules as BadModules
import Whine.Core.CaseBranchIndentation as CaseBranchIndentation
import Whine.Core.CommaFirstArrays as CommaFirstArrays
import Whine.Types (class MonadRules, RuleFactories, ruleFactory)

rules :: ∀ m. MonadRules m => RuleFactories m
rules =
  [ ruleFactory "BadModules" BadModules.codec BadModules.rule
  , ruleFactory "CommaFirstArrays" CJ.json CommaFirstArrays.rule
  , ruleFactory "CaseBranchIndentation" CJ.json CaseBranchIndentation.rule
  ]
