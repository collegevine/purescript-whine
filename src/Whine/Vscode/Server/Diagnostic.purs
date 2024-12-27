module Vscode.Server.Diagnostic where

import Untagged.Union (UndefinedOr)

type Diagnostic =
  -- The range at which the message applies
  { range :: Range

  -- The diagnostic's severity. Can be omitted. If omitted it is up to the
  -- client to interpret diagnostics as error, warning, info or hint.
  , severity :: UndefinedOr DiagnosticSeverity

  -- The diagnostic's code, which usually appear in the user interface.
  , code :: String

  -- A human-readable string describing the source of this
  -- diagnostic, e.g. 'typescript' or 'super lint'. It usually
  -- appears in the user interface.
  , source :: String

  -- The diagnostic's message. It usually appears in the user interface
  , message :: String

  -- tags?: DiagnosticTag[];
  -- relatedInformation?: DiagnosticRelatedInformation[];
  -- data?: LSPAny;
  }

type Range =
  { start :: Position
  , end :: Position
  }

type Position =
  { line :: Int
  , character :: Int
  }

newtype DiagnosticSeverity = DiagnosticSeverity Int

diagnosticSeverity ::
  { error :: DiagnosticSeverity
  , warning :: DiagnosticSeverity
  , information :: DiagnosticSeverity
  , hint :: DiagnosticSeverity
  }
diagnosticSeverity =
  { error: DiagnosticSeverity 1
  , warning: DiagnosticSeverity 2
  , information: DiagnosticSeverity 3
  , hint: DiagnosticSeverity 4
  }
