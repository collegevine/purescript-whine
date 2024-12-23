-- | Checks that in a case expression all branches are either single-line or
-- | multi-line, not a mix.
-- |
-- |     -- Good:
-- |     case x of
-- |       Just y -> y
-- |       Nothing -> 0
-- |
-- |     -- Good:
-- |     case x of
-- |       Just y ->
-- |         y
-- |       Nothing ->
--           0
-- |
-- |     -- Bad:
-- |     case x of
-- |       Just y -> y
-- |       Nothing ->
-- |         0
-- |
module Whine.Rules.CaseBranchIndentation where

import Whine.Prelude

import Data.Array.NonEmpty as NEA
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Expr(..), Separated(..))
import Whine.Types (class MonadRules, Handle(..), Rule, emptyRule)

caseBranchIndentation :: ∀ m. MonadRules m => {} -> Rule m
caseBranchIndentation _ = emptyRule { onExpr = onExpr }
  where
    onExpr :: Handle Expr m
    onExpr = Handle case _ of
      e@(ExprCase { branches }) -> do
        let { yes, no } = NEA.partition isOneLine branches
            consistentIndent = null yes || null no
        unless consistentIndent $
          tell
          [ { source: Just $ rangeOf e
            , message: "Inconsistent indentation in case branches: keep either all single-line or all multi-line"
            }
          ]
        where
          isOneLine (Separated { head } /\ body) = (rangeOf head).start.line == (rangeOf body).end.line

      _ ->
        pure unit

