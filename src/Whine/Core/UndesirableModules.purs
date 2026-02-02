-- | Takes a list of modules that "shouldn't" be used in code (because they're
-- | bad/dangerous somehow), with an optional message to e.g. explain why the
-- | module shouldn't be used or propose alternatives.
-- |
-- | Configuration should look like this:
-- |
-- |     {
-- |       "Unsafe.Module": "Don't use this module, it's unsafe",
-- |       "Another.Module": "Use Better.Alternative instead",
-- |       ...
-- |     }
-- |
-- | There is no "default" configuration. If this rule is not configured, it
-- | will not emit any warnings.
-- |
module Whine.Core.UndesirableModules where

import Whine.Prelude

import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJR
import Data.Map as Map
import PureScript.CST.Types (ImportDecl(..), Module(..), ModuleHeader(..), ModuleName, Name(..))
import Whine.Types (Rule(..), reportViolation)

rule :: Map ModuleName String -> Rule
rule badModules = Rule \(Module { header: ModuleHeader { imports }}) ->
  for_ imports \(ImportDecl { module: Name m }) ->
    Map.lookup m.name badModules # traverse_ \message ->
      reportViolation { source: Just m.token.range, message }

codec :: CJ.Codec (Map ModuleName String)
codec =
  dimap (overMap unwrap >>> { modules: _ }) (_.modules >>> overMap wrap) $
    CJR.object { modules: CJ.Common.strMap CJ.string }

  where
    overMap :: ∀ a b. Ord b => (a -> b) -> Map a String -> Map b String
    overMap f a = Map.fromFoldable $ lmap f <$> (Map.toUnfoldable a :: Array _)
