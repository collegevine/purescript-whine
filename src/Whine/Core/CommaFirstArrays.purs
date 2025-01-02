-- | Checks that array literals are written comma-first style, with elements
-- | aligned vertically, one space to the right of the commas.
-- |
-- | Single-line arrays are ok.
-- |
-- | Several elements on the same line are ok, as long as the first one of them
-- | is aligned correctly.
-- |
-- |     -- Good:
-- |     [ 1
-- |     , 2
-- |     , 3
-- |     ]
-- |
-- |     -- Good:
-- |     [ 1, 2, 3 ]
-- |
-- |     -- Good:
-- |     [ 1, 2
-- |     , 3
-- |     , 4, 5
-- |     , 6
-- |     ]
-- |
-- |     -- Bad:
-- |     [ 1
-- |     ,  2
-- |     , 3
-- |     ]
-- |
-- |     -- Bad:
-- |     [ 1
-- |      , 2
-- |     , 3
-- |     ]
-- |
-- |     -- Bad:
-- |     [
-- |       1,
-- |       2,
-- |       3
-- |     ]
-- |
-- |     -- Bad:
-- |     [ 1, 2,
-- |      3, 4 ]
-- |
module Whine.Core.CommaFirstArrays where

import Whine.Prelude

import Data.Foldable (or)
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Expr(..), Separated(..), Wrapped(..))
import Whine.Types (class MonadRules, Handle(..), Rule, emptyRule)

rule :: ∀ m. MonadRules m => JSON -> Rule m
rule _ = emptyRule { onExpr = onExpr }
  where
    onExpr :: Handle Expr m
    onExpr = Handle case _ of
      ExprArray (Wrapped { open, value: Just (Separated items), close }) -> do
        when (multiLine && (anyMisalignedPrefixes || not correctCloseBracket)) $
          tell
          [ { source: Just { start: open.range.start, end: close.range.end }
            , message: "Prefer comma-first style in array literals, items aligned vertically"
            }
          ]
        where
          multiLine = open.range.start.line /= close.range.end.line
          lastItem = last items.tail <#> snd # fromMaybe items.head
          correctCloseBracket = close.range.start.line > (rangeOf lastItem).end.line

          firstItem = open /\ items.head
          newLineStartingItems = firstItem : do
            (_ /\ prevItem) /\ (nextComma /\ nextItem) <- zip (firstItem : items.tail) items.tail
            guard $ (rangeOf prevItem).end.line < (rangeOf nextItem).start.line
            pure $ nextComma /\ nextItem

          anyMisalignedPrefixes =
            newLineStartingItems # any \(prefix /\ item) ->
              or
              [ prefix.range.end.line /= (rangeOf item).start.line
              , prefix.range.end.column /= (rangeOf item).start.column - 1
              , prefix.range.start.column /= open.range.start.column
              , open.range.start.column /= close.range.start.column
              ]

      _ ->
        pure unit
