-- | Takes a list of modules that "shouldn't" be used in code (because they're
-- | bad/dangerous somehow), every such "bad" module mapped to a "good" module
-- | that should be used instead.
-- |
-- | Configuration should look like this:
-- |
-- |     {
-- |       "Unsafe.Module": "Data.Array",  // Will emit a message saying "don't use Unsafe.Module, use Data.Array instead"
-- |       "Another.Module": "Use.This.One.Instead",
-- |       ...
-- |     }
-- |
module Whine.Core.BadModules where

import Whine.Prelude

import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJR
import Data.Map as Map
import PureScript.CST.Types (ImportDecl(..), ModuleName, Name(..))
import Whine.Types (class MonadRules, Handle(..), Rule, emptyRule)

rule :: ∀ m. MonadRules m => Map ModuleName ModuleName -> Rule m
rule badToGoodMap = emptyRule
  { onModuleImport = Handle \(ImportDecl { module: Name m }) -> do
      Map.lookup m.name badToGoodMap # traverse_ \useInstead ->
        tell
        [ { source: Just m.token.range
          , message: "Do not use " <> unwrap m.name <> ", use " <> unwrap useInstead <> " instead."
          }
        ]
  }

codec :: CJ.Codec (Map ModuleName ModuleName)
codec =
  dimap (overMap unwrap >>> { modules: _ }) (_.modules >>> overMap wrap) $
    CJR.object { modules: CJ.Common.strMap CJ.string }

  where
    overMap :: ∀ a b. Ord b => (a -> b) -> Map a a -> Map b b
    overMap f a = Map.fromFoldable $ bimap f f <$> (Map.toUnfoldable a :: Array _)
