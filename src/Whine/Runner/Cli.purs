module Whine.Runner.Cli where

import Whine.Runner.Prelude

import Data.Array.NonEmpty as NEA
import Data.List as List
import Data.String.NonEmpty.Internal as NES
import Options.Applicative as O
import Options.Applicative.Types as OT

type Args =
  { command :: Command
  , version :: Boolean
  , debug :: Boolean
  , quiet :: Boolean
  }

data Command
  = JustWhine JustWhineArgs
  | LanguageServer LanguagServerArgs

type JustWhineArgs =
  { globs :: Maybe (NonEmptyArray NonEmptyString)
  , outputFormat :: OutputFormat
  }

type LanguagServerArgs = { checkWhen :: CheckFileWhen }

data CheckFileWhen = CheckOnSave | CheckOnChange

data OutputFormat = Short | Long

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
    O.info
      (LanguageServer <$> languageServerArgsParser)
      (O.progDesc "Start Whine in Language Server mode")

justWhineArgsParser :: O.Parser JustWhineArgs
justWhineArgsParser = ado
  args <- OT.many $ O.strArgument $ fold
    [ O.metavar "GLOB"
    , O.help "Glob patterns to match files to lint. When empty, all files are linted."
    ]
  outputFormat <- outputFormatOption
  in
    { globs: NEA.fromFoldable $ List.mapMaybe NES.fromString args
    , outputFormat: fromMaybe Long outputFormat
    }

languageServerArgsParser :: O.Parser LanguagServerArgs
languageServerArgsParser = ado
  _ <- O.switch $ O.long "stdio"
  _ <- O.switch $ O.long "node-ipc"
  _ <- OT.optional $ O.strOption $ O.long "socket"
  checkWhen <- checkWhenOption
  in { checkWhen: checkWhen # fromMaybe CheckOnSave }

argsParser :: O.Parser Args
argsParser =
  ado
    command <- commandParser <|> (JustWhine <$> justWhineArgsParser)
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

outputFormatOption :: O.Parser (Maybe OutputFormat)
outputFormatOption =
  OT.optional $ O.option parseOutputFormat $ fold
    [ O.long "output"
    , O.short 'o'
    , O.help "Output format. Possible values are 'short' and 'long'. Default is 'long'."
    ]
  where
    parseOutputFormat = O.eitherReader \s -> do
      if s == "short" then Right Short
      else if s == "long" then Right Long
      else Left $ "Invalid output format: " <> s

determineLogLevel :: Args -> LogSeverity
determineLogLevel args =
  if args.debug then LogDebug
  else if args.quiet then LogError
  else LogInfo

checkWhenOption :: O.Parser (Maybe CheckFileWhen)
checkWhenOption =
  OT.optional $ O.option checkWhen $ fold
    [ O.long "check-on"
    , O.help $ fold
      [ "When to check files for violations. Possible values are "
      , "'", saveOption, "' to check when a file is saved or "
      , "'", changeOption, "' to check on every change. "
      , "Default is '", saveOption, "'."
      ]
    ]
  where
    saveOption = "save"
    changeOption = "change"

    checkWhen = O.eitherReader \s -> do
      if s == saveOption then Right CheckOnSave
      else if s == changeOption then Right CheckOnChange
      else Left $ "Invalid value: " <> s
