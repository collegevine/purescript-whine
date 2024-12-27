module Whine.Runner.Client.Main where

import Prelude

import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Untagged.Castable (cast)
import Vscode.Client (DocumentSelector, createLanguageClient, start)
import Vscode.Client.Executable (transportKind)
import Vscode.Context (ExtensionContext)

activate :: EffectFn1 ExtensionContext Unit
activate = mkEffectFn1 \ctx -> do
  client <- createLanguageClient $ cast
    { id: "whine"
    , name: "Whine"
    , clientOptions:
      { documentSelector: [ cast { scheme: "file", language: "purescript" } :: DocumentSelector ]
      , synchronize: {}
      }
    , serverOptions:
      { run:
        { command: "npx"
        , transport: transportKind.stdio
        , args: [ "-y", "whine", "language-server" ]
        }
      }
    }

  start client
