module Whine.Runner.Cli where

import Whine.Runner.Prelude

import Options.Applicative as O
import Options.Applicative.Types as OT

type Args =
  { command :: Command
  , version :: Boolean
  , debug :: Boolean
  , quiet :: Boolean
  }

data Command = JustWhine | LanguageServer

parseCliArgs :: ∀ m. MonadEffect m => m Args
parseCliArgs = liftEffect $
  O.customExecParser
    (O.prefs $ O.showHelpOnError <> O.subparserInline)
    (O.info
      (O.helper <*> argsParser)
      (O.progDesc "PureScript linter, extensible, with configurable rules, and one-off escape hatches")
    )

commandParser :: O.Parser Command
commandParser =
  O.subparser $ O.command "language-server" $
    O.info (ignoreLspOptions $> LanguageServer) (O.progDesc "Start Whine in Language Server mode")

ignoreLspOptions :: O.Parser Unit
ignoreLspOptions = ado
  _ <- O.switch $ O.long "stdio"
  _ <- O.switch $ O.long "node-ipc"
  _ <- OT.optional $ O.strOption $ O.long "socket"
  in unit

argsParser :: O.Parser Args
argsParser =
  ado
    command <- commandParser <|> pure JustWhine
    version <- versionFlag
    debug <- debugFlag
    quiet <- quietFlag
  in
    { command, version, debug, quiet }

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

determineLogLevel :: Args -> LogSeverity
determineLogLevel args =
  if args.debug then LogDebug
  else if args.quiet then LogError
  else LogInfo
