module Local.WhineRules where

import Prelude

import Data.Codec.JSON as CJ
import JSON (JSON)
import JSON as JSON
import PureScript.CST.Types (Module(..), ModuleHeader(..), ModuleName(..), Name(..))
import Whine.Log (logInfo)
import Whine.Types (Handle(..), Rule, RuleFactories, currentModule, emptyRule, ruleFactory)

rules :: RuleFactories
rules = [ruleFactory "LocalRule" CJ.json localRule]

localRule :: JSON -> Rule
localRule config = emptyRule { onModule = onModule }
  where
    onModule :: Handle Module
    onModule = Handle \_ ->
      currentModule \(Module { header: ModuleHeader { name: Name { name: ModuleName n } } }) -> do
        logInfo $ "Local rule configured with '" <> JSON.print config <> "' is running on " <> n
        pure unit