module Whine.Core.WhineRules
  ( rules
  )
  where

import Data.Codec.JSON as CJ
import Whine.Core.UndesirableModules as UndesirableModules
import Whine.Core.CaseBranchIndentation as CaseBranchIndentation
import Whine.Core.CommaFirstArrays as CommaFirstArrays
import Whine.Types (class MonadRules, RuleFactories, ruleFactory)

rules :: ∀ m. MonadRules m => RuleFactories m
rules =
  [ ruleFactory "UndesirableModules" UndesirableModules.codec UndesirableModules.rule
  , ruleFactory "CommaFirstArrays" CJ.json CommaFirstArrays.rule
  , ruleFactory "CaseBranchIndentation" CJ.json CaseBranchIndentation.rule
  ]
