module Whine.Bootstrap.Execa where

import Whine.Runner.Prelude

import Node.Library.Execa (ExecaOptions, ExecaProcess, ExecaResult)
import Node.Library.Execa as Execa

execa :: String -> Array String -> (ExecaOptions -> ExecaOptions) -> RunnerM ExecaProcess
execa cmd args opts = do
  res <- liftAff $ try $ Execa.execa cmd args opts
  case res of
    Left err -> do
      logDebug err
      die $ "Error running '" <> cmd <> "'"
    Right r ->
      pure r

execResultSuccessOrDie :: String -> ExecaResult -> RunnerM Unit
execResultSuccessOrDie cmd res =
  when (res.exitCode /= Just 0) do
    logDebug $ "STDOUT:\n" <> res.stdout <> "\n\n"
    logDebug $ "STDERR:\n" <> res.stderr <> "\n\n"
    die $ "Error running '" <> cmd <> "'"

execSuccessOrDie :: String -> ExecaProcess -> RunnerM ExecaResult
execSuccessOrDie cmd proc = do
  res <- liftAff proc.getResult
  execResultSuccessOrDie cmd res
  pure res

execSuccessOrDie_ :: String -> ExecaProcess -> RunnerM Unit
execSuccessOrDie_ cmd res = void $ execSuccessOrDie cmd res
