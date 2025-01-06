module Whine.Runner.LanguageServer where

import Whine.Runner.Prelude

import Data.String as String
import Node.Path as NodePath
import Record (merge)
import Untagged.Castable (cast)
import Vscode.Events (on)
import Vscode.Server.Capabilities (textDocumentSyncKind)
import Vscode.Server.Connection as Conn
import Vscode.Server.Diagnostic (diagnosticSeverity)
import Vscode.Server.TextDocuments as Doc
import Whine (checkModule)
import Whine.Runner.Cli (CheckFileWhen(..))
import Whine.Runner.Config (readConfig)
import Whine.Runner.Glob as Glob
import Whine.Types (RuleFactories)
import WhineM (mapViolations, runWhineM, unliftWhineM)

startLanguageServer ::
  { factories :: RuleFactories
  , configFile :: FilePath
  , checkWhen :: CheckFileWhen
  }
  -> RunnerM Unit
startLanguageServer { factories, configFile, checkWhen } = do
  env <- ask
  conn <- Conn.createConnection

  conn # on Conn.initialize \{} ->
    pure { capabilities: { textDocumentSync: textDocumentSyncKind.incremental } }

  -- TODO: report config errors
  -- TODO: reload config on change
  config /\ _configErrors <-
    readConfig factories configFile
    # mapViolations (merge { muted: false })
    # runWhineM env

  checkDocument <- unliftWhineM \document -> do
    let uri = Doc.uri document
        path = uri # String.stripPrefix (Pattern "file://") <#> NodePath.relative (NodePath.dirname configFile) # fromMaybe ""

    if Glob.test config.files path then do
      logDebug $ "Checking file: URI=" <> Doc.uri document <> ", path=" <> path

      text <- Doc.getText document
      _ /\ violations <- runWhineM env $ checkModule config.rules { path, text }
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
    else
      logDebug $ "Skipping " <> path <> " because it doesn't match globs in the config"

  docs <- Doc.create
  docs # on Doc.didOpen \{ document } -> launchAff_ $ checkDocument document
  docs # on checkEvent \{ document } -> launchAff_ $ checkDocument document

  Doc.listen docs conn
  Conn.listen conn

  logDebug "Language Server started"

  where
    firstLineRange = { start: { line: 0, character: 0 }, end: { line: 1, character: 0 } }
    vsCodePos p = { line: p.line, character: p.column }

    checkEvent = case checkWhen of
      CheckOnSave -> Doc.didSave
      CheckOnChange -> Doc.didChangeContent
