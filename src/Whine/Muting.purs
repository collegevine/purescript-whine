module Whine.Muting
  ( MutedRange(..)
  , mutedRanges
  )
  where

import Whine.Prelude

import Data.Map as Map
import Data.String as String
import Whine.Types (RuleId)

data MutedRange = MutedRange { start :: Int, end :: Int } | MutedLine Int

data LineKind = Enable | Disable | DisableSingleLine

-- | Given lines of a code file, finds ranges and/or lines marked with
-- | #disable/#enable directives, and returns those ranges grouped by rule.
-- |
-- | For example:
-- |
-- |     -- #disable SomeRule
-- |     foo -- #disable AnotherRule
-- |     bar
-- |     -- #enable SomeRule
-- |     baz
-- |     qux -- #disable SomeRule
-- |
-- | Would return:
-- |
-- |     {
-- |       SomeRule: [MutedRange 0 3, MutedLine 5],
-- |       AnotherRule: [MutedLine 1]
-- |     }
-- |
--
-- The implementation goes like this:
--
--    1. Scan all content lines in order (with `foldlWithIndex trackLine`)
--    2. As we go, accumulate "open" ranges and "closed" ranges.
--    3. For every #disable directive alone on the line, open a range.
--    4. For every #enable directive, close the range for that rule (if open).
--       This means removing this range from the "open" set and adding it to the
--       "closed" set.
--    5. For every #disable directive tucked on the end of a code line,
--       immediately add a closed range consisting of that one line.
--    6. After we're done scanning the lines, we might be left with some "open"
--       ranges (in case there was a #disable directive without a corresponding
--       #enable one). Such ranges we assume to be auto-closed at the end of the
--       file and force-close them (via `forceCloseOpenRanges`).
--
mutedRanges :: { lines :: Array String } -> Map RuleId (Array MutedRange)
mutedRanges { lines } = trackedRanges.closed # Map.unionWith (<>) forceCloseOpenRanges
  where
    forceCloseOpenRanges = trackedRanges.open <#> \start -> [MutedRange { start, end: length lines }]

    trackedRanges = foldlWithIndex trackLine { open: Map.empty, closed: Map.empty } lines

    trackLine idx { open, closed } line =
      case parseLine line of
        Just (DisableSingleLine /\ rule) ->
          -- Single-line disabling => immediately add a closed range consisting of this one line.
          { open, closed: Map.insertWith (<>) rule [MutedLine idx] closed }

        Just (Enable /\ rule)
          | Just start <- Map.lookup rule open ->
              -- Enable directive that matches a previously opened range => close the range.
              { open: Map.delete rule open
              , closed: Map.insertWith (<>) rule [MutedRange { start, end: idx }] closed
              }
          | otherwise ->
              -- Unmatched Enable directive => just ignore it.
              { open, closed }

        Just (Disable /\ rule)
          | Map.member rule open ->
              -- Disable directive for a rule that has already been disabled before => ignore it.
              { open, closed }
          | otherwise ->
              -- Disable directive for new rule => add an open range.
              { open: Map.insert rule idx open
              , closed
              }

        Nothing ->
          { open, closed }

    parseLine l = do
      commentStart <- String.lastIndexOf (Pattern "-- #") l

      let left = l # String.take commentStart # String.trim
          comment = l # String.drop (commentStart + 4)
          onPrefix p r = comment # String.stripPrefix (Pattern p) <#> String.trim <#> (r /\ _)

      if left /= "" then
        onPrefix "disable " DisableSingleLine
      else
        (onPrefix "enable " Enable) <|> (onPrefix "disable " Disable)

