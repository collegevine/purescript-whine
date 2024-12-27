module Whine.Bootstrap.Prelude
  ( module Whine.Prelude
  , module R
  , module Whine.Bootstrap.Prelude
  )
  where

import Whine.Prelude

import Control.Monad.Error.Class (class MonadError) as R
import Effect.Aff (Aff, launchAff_) as R
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff, liftAff) as R
import Node.Library.Execa (ExecaProcess, ExecaResult, ExecaOptions) as R
import Node.Library.Execa (execa) as Execa
import Node.Process (exit')
import Whine.Bootstrap.Hash (hashString) as R
import Whine.Bootstrap.Log (class Loggable, class MonadLog, LogSeverity(..), log, logDebug, logError, logInfo, logWarning, toDoc) as R

die :: ∀ m err a. R.MonadLog m => R.Loggable err => err -> m a
die message = do
  R.logError message
  exit 1

rightOrDie :: ∀ m err a. R.MonadLog m => R.Loggable err => Either err a -> m a
rightOrDie = either die pure

tryOrDie :: ∀ m a. R.MonadLog m => R.MonadError Error m => m a -> m a
tryOrDie = try >=> rightOrDie

execSuccessOrDie :: ∀ m. R.MonadAff m => R.MonadLog m => String -> R.ExecaProcess -> m R.ExecaResult
execSuccessOrDie cmd proc = do
  res <- R.liftAff proc.getResult
  when (res.exitCode /= Just 0) do
    die $ "Error running '" <> cmd <> "':\n" <> show res.stderr
  pure res

execSuccessOrDie_ :: ∀ m. R.MonadAff m => R.MonadLog m => String -> R.ExecaProcess -> m Unit
execSuccessOrDie_ cmd res = void $ execSuccessOrDie cmd res

exit :: ∀ m a. MonadEffect m => Int -> m a
exit = liftEffect <<< exit'

execa :: ∀ m. R.MonadAff m => String -> Array String -> (R.ExecaOptions -> R.ExecaOptions) -> m R.ExecaProcess
execa cmd args opts = R.liftAff $ Execa.execa cmd args opts
