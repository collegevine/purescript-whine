-- | Checks that array literals are written comma-first style, with elements
-- | aligned vertically, one space to the right of the commas.
-- |
-- | Single-line arrays are ok.
-- |
-- | Several elements on the same line are ok, as long as the first one of them
-- | is aligned correctly.
-- |
-- |     -- Good:
-- |     { x: 'foo'
-- |     , y: 42
-- |     , z: true
-- |     }
-- |
-- |     -- Good:
-- |     { x: 'foo', y: 42, z: true }
-- |
-- |     -- Good:
-- |     { x: 'foo', y: 42
-- |     , z: true
-- |     , w: false
-- |     , v: 0, u: 1
-- |     }
-- |
-- |     -- Bad:
-- |     { x: 'foo'
-- |      , y: 42
-- |     , z: true
-- |     }
-- |
-- |     -- Bad:
-- |     {
-- |       x: 'foo',
-- |       y: 42,
-- |       z: true
-- |     }
-- |
-- |     -- Bad:
-- |     { x: 'foo', y: 42,
-- |       z: true }
-- |
module Whine.Core.CommaFirstRecords where

import Whine.Prelude

import PureScript.CST.Range (class RangeOf, rangeOf)
import PureScript.CST.Types (Expr(..), RecordLabeled(..))
import Whine.Core.CommaFirst (commaFirstRule)
import Whine.Types (Handle(..), Rule, emptyRule)

rule :: JSON -> Rule
rule _ = emptyRule { onExpr = onExpr }
  where
    onExpr :: Handle Expr
    onExpr = Handle case _ of
      ExprRecord r ->
        commaFirstRule r rangeOfRecordLabeled "Format record literals comma-first, align fields vertically"
      _ ->
        pure unit

    rangeOfRecordLabeled :: ∀ e. RangeOf e => RecordLabeled (Expr e) -> SourceRange
    rangeOfRecordLabeled = case _ of
      RecordPun name -> rangeOf name
      RecordField label _ (expr :: Expr e) -> unionRanges (rangeOf label) (rangeOf expr)
