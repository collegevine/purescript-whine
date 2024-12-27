module Whine.Runner.LanguageServer where

import Whine.Prelude

import Control.Promise (toAff)
import Effect.Aff (launchAff_)
import Untagged.Castable (cast)
import Vscode.Server.Capabilities (textDocumentSyncKind)
import Vscode.Server.Connection as Conn
import Vscode.Server.Events (on)
import Vscode.Server.TextDocuments (textDocumentUri)
import Vscode.Server.TextDocuments as Doc
import Whine.Types (RuleFactories, Violations)

startLanguageServer ::
  { factories :: RuleFactories (WriterT (Violations ()) Effect)
  , configFile :: FilePath
  }
  -> Effect Unit
startLanguageServer _ = do
  conn <- Conn.createConnection

  conn # on Conn.initialize \{} ->
    pure { capabilities: { textDocumentSync: textDocumentSyncKind.incremental } }

  docs <- Doc.create
  docs # on Doc.didSave \{ document } -> launchAff_ do
    let diag = cast
          { range: { start: { line: 2, character: 0 }, end: { line: 2, character: 20 } }
          , code: "0"
          , source: "whine"
          , message: "This is a diagnostic message"
          }
    toAff $ conn # Conn.sendDiagnostics { uri: textDocumentUri document, diagnostics: [diag] }

  Doc.listen docs conn
  Conn.listen conn
