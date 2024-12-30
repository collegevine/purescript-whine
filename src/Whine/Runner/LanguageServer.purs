module Whine.Runner.LanguageServer where

import Whine.Runner.Prelude

import Control.Monad.Writer (runWriterT)
import Data.String as String
import Node.Path as NodePath
import Record (merge)
import Untagged.Castable (cast)
import Vscode.Server.Capabilities (textDocumentSyncKind)
import Vscode.Server.Connection as Conn
import Vscode.Server.Diagnostic (diagnosticSeverity)
import Vscode.Events (on)
import Vscode.Server.TextDocuments as Doc
import Whine (checkModule)
import Whine.Runner.Config (readConfig)
import Whine.Types (RuleFactories, Violations, mapViolation)

startLanguageServer ::
  { factories :: RuleFactories (WriterT (Violations ()) RunnerM)
  , configFile :: FilePath
  }
  -> RunnerM Unit
startLanguageServer { factories, configFile } = do
  conn <- Conn.createConnection

  conn # on Conn.initialize \{} ->
    pure { capabilities: { textDocumentSync: textDocumentSyncKind.incremental } }

  -- TODO: report config errors
  -- TODO: reload config on change
  ruleSet /\ _configErrors <-
    readConfig factories configFile
    # mapViolation (merge { muted: false })
    # runWriterT

  onDidSave <- unliftRunnerM \document -> do
    let uri = Doc.uri document
        path = uri # String.stripPrefix (Pattern "file://") <#> NodePath.relative (NodePath.dirname configFile) # fromMaybe ""

    logDebug $ "Checking file: URI=" <> Doc.uri document <> ", path=" <> path

    text <- Doc.getText document
    violations <- execWriterT $ checkModule ruleSet { path, text }
    let liveViolations = violations # filter (not _.muted)

    logDebug $ fold
      [ "Found violations in ", path, ": "
      , show (length liveViolations), " live, "
      , show (length violations - length liveViolations), " muted"
      ]

    unless (null liveViolations) do
      conn # Conn.sendDiagnostics
        { uri
        , diagnostics: liveViolations <#> \v -> cast
            { range: fromMaybe firstLineRange $ v.source <#> \r -> { start: vsCodePos r.start, end: vsCodePos r.end }
            , code: v.rule
            , source: "purescript-whine"
            , message: v.message
            , severity: diagnosticSeverity.information
            }
        }

  docs <- Doc.create
  docs # on Doc.didSave \{ document } -> launchAff_ $ onDidSave document
  docs # on Doc.didOpen \{ document } -> launchAff_ $ onDidSave document

  Doc.listen docs conn
  Conn.listen conn

  logDebug "Language Server started"

  where
    firstLineRange = { start: { line: 0, character: 0 }, end: { line: 1, character: 0 } }
    vsCodePos p = { line: p.line, character: p.column }
