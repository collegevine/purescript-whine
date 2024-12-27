module Vscode.Server.Capabilities where

import Elmish.Foreign (class CanPassToJavaScript)

type ServerCapabilities =
  { textDocumentSync :: TextDocumentSyncKind
  }

newtype TextDocumentSyncKind = TextDocumentSyncKind Int
derive newtype instance CanPassToJavaScript TextDocumentSyncKind

textDocumentSyncKind ::
  { none :: TextDocumentSyncKind
  , full :: TextDocumentSyncKind
  , incremental :: TextDocumentSyncKind
  }
textDocumentSyncKind =
  { none: TextDocumentSyncKind 0
  , full: TextDocumentSyncKind 1
  , incremental: TextDocumentSyncKind 2
  }
