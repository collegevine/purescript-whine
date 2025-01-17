module Test.Main where

import Test.Prelude

import Effect.Aff (launchAff_)
import Options.Applicative as Opt
import Record (merge)
import Test.Integration (integrationSpecs)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config as Config

main :: Effect Unit
main = launchAff_ do
  config <- liftEffect $
    Config.fromCommandLine' defaultConfig (Config.commandLineOptionParsers <> [integrationOptions])
  integration <-
    integrationSpecs { debug: config.debug, accept: config.accept }
  liftEffect $
    runSpecAndExitProcess'
      { defaultConfig: config
      , parseCLIOptions: false
      }
      [consoleReporter] $
      integration

type Config = Config.TestRunConfig' (debug :: Boolean, accept :: Boolean)

defaultConfig :: Config
defaultConfig = Config.defaultConfig { timeout = Nothing } `merge` { debug: false, accept: false }

integrationOptions :: Config.OptionParser Config
integrationOptions = ado
  d <- Opt.switch $ fold
    [ Opt.long "debug"
    , Opt.help "Do not destroy temporary directory used for integration tests."
    ]

  a <- Opt.switch $ fold
    [ Opt.long "accept"
    , Opt.help "Accept integration test output."
    ]

  in _ { debug = d, accept = a }
