module Whine.Core.CommaFirst where

import Whine.Prelude

import Data.Foldable (or)
import PureScript.CST.Types (Separated(..), Wrapped(..), Delimited)
import Whine.Types (class MonadRules, reportViolation)

commaFirstRule :: ∀ item m. MonadRules () m => Delimited item -> (item -> SourceRange) -> String -> m Unit
commaFirstRule (Wrapped { value: Nothing }) _ _ = pure unit
commaFirstRule (Wrapped { open, close, value: Just (Separated items) }) rangeOfItem message = do
  for_ (unionManyRanges violations) \range ->
    reportViolation { source: Just range, message }
  where
    multiLine = open.range.start.line /= close.range.end.line
    lastItem = last items.tail <#> snd # fromMaybe items.head

    firstItem = open /\ items.head
    newLineStartingItems = firstItem : do
      (_ /\ prevItem) /\ (nextComma /\ nextItem) <- zip (firstItem : items.tail) items.tail
      guard $ (rangeOfItem prevItem).end.line < (rangeOfItem nextItem).start.line
      pure $ nextComma /\ nextItem

    violations
      | multiLine = misalignedPrefixes <> closeBracket
      | otherwise = []

    closeBracket = do
      guard $ or
        [ close.range.start.line /= (rangeOfItem lastItem).end.line + 1
        , close.range.start.column /= open.range.start.column
        ]
      pure
        { start: (rangeOfItem lastItem).end
        , end: close.range.end
        }

    misalignedPrefixes = do
      prefix /\ item <- newLineStartingItems
      guard $ or
        [ prefix.range.end.line /= (rangeOfItem item).start.line
        , prefix.range.end.column /= (rangeOfItem item).start.column - 1
        , prefix.range.start.column /= open.range.start.column
        ]
      pure
        { start: prefix.range.start
        , end: (rangeOfItem item).end
        }
