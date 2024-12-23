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
module Whine.Rules.BadModules where

import Whine.Prelude

import Data.Map as Map
import Foreign.Object as FO
import PureScript.CST.Types (ImportDecl(..), ModuleName(..), Name(..))
import Whine.Types (class MonadRules, Handle(..), Rule, emptyRule)

badModules :: ∀ m. MonadRules m => { modules :: FO.Object String } -> Rule m
badModules { modules } = emptyRule
  { onModuleImport = Handle \(ImportDecl { module: Name m }) -> do
      Map.lookup m.name badToGoodMap # traverse_ \useInstead ->
        tell
        [ { source: Just m.token.range
          , message: "Do not use " <> unwrap m.name <> ", use " <> unwrap useInstead <> " instead."
          }
        ]
  }
  where
    badToGoodMap = (Map.fromFoldable :: Array _ -> _) $ bimap ModuleName ModuleName <$> FO.toUnfoldable modules
