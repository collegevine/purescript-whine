module Whine.Cli where

import Whine.Prelude

import Data.Newtype (over)
import Options.Applicative as O

type Args =
  { version :: Boolean
  , debug :: Boolean
  , quiet :: Boolean
  , languageServer :: Boolean
  }

parseCliArgs :: ∀ m. MonadEffect m => m Args
parseCliArgs = liftEffect $
  O.customExecParser
    (O.defaultPrefs # over O.ParserPrefs _
      { prefShowHelpOnError = true
      }
    )
    (O.info
      (O.helper <*> argsParser)
      (O.progDesc "PureScript linter, extensible, with configurable rules, and one-off escape hatches")
    )

argsParser :: O.Parser Args
argsParser =
  ado
    version <- versionFlag
    debug <- debugFlag
    quiet <- quietFlag
    languageServer <- languageServerFlag
  in
    { version, debug, quiet, languageServer }

versionFlag :: O.Parser Boolean
versionFlag =
  O.switch $ fold
    [ O.long "version"
    , O.short 'v'
    , O.help "Print Whine version"
    ]

debugFlag :: O.Parser Boolean
debugFlag =
  O.switch $ fold
    [ O.long "debug"
    , O.help "Print debug output"
    ]

quietFlag :: O.Parser Boolean
quietFlag =
  O.switch $ fold
    [ O.long "quiet"
    , O.short 'q'
    , O.help "Print no output"
    ]

languageServerFlag :: O.Parser Boolean
languageServerFlag =
  O.switch $ fold
    [ O.long "language-server"
    , O.help "Start Whine as a language server"
    ]
