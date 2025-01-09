module Whine.Print where

import Whine.Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Data.String as String
import Data.String.Utils (padStart')
import Whine.Runner.Cli (OutputFormat(..))
import Whine.Types (Violation, WithFile, WithMuted, WithRule)

type Args r = { outputFormat :: OutputFormat | r }

printViolation :: ∀ r. Args r -> Violation (WithRule + WithMuted + WithFile + ()) -> Maybe String
printViolation _ { muted: true } = Nothing
printViolation args { source, message, rule, file } = Just text
  where
    text =
      case args.outputFormat of
        Short -> fileLocation <> " " <> ruleAndMessage
        Long -> fileLocation <> " " <> ruleAndMessage <> "\n" <> sourceCode
      where
        fileLocation = fold [withGraphics (foreground Cyan) file.path, ":", locationText]
        ruleAndMessage = fold [withGraphics (foreground Red) rule, ": ", message]
        sourceCode = withGraphics (foreground BrightYellow) sourceText

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
