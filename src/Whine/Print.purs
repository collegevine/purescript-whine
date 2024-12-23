module Whine.Print where

import Whine.Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Data.String as String
import Data.String.Utils (padStart')
import Whine.Types (Violation, WithFile, WithMuted, WithRule)

printViolation :: Violation (WithRule + WithMuted + WithFile + ()) -> Maybe String
printViolation { muted: true } = Nothing
printViolation { source, message, rule, file } = Just text
  where
    text = fold
      [ withGraphics (foreground Cyan) file.path, ":"
      , locationText
      , withGraphics (foreground Red) rule, ": "
      , message, "\n"
      , withGraphics (foreground BrightYellow) sourceText
      ]

    indent = "  "

    locationText = fromMaybe " " $ source <#> \r ->
      fold [show (r.start.line + 1), ":", show (r.start.column + 1), ": "]

    sourceText = fromMaybe "" do
      loc <- source
      lines <- file.lines

      let locLines = lines # drop loc.start.line # take (loc.end.line - loc.start.line + 1)
          maxLineNumWidth = (loc.end.line + 1) # show # String.length
          snippet = do
            lineText /\ num <- zip locLines (loc.start.line..loc.end.line)
            [indent, indent, padStart' maxLineNumWidth (show $ num + 1), ": ", lineText, "\n"]

      pure $ fold $ "\n" : snippet
