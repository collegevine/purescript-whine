module Whine.Bootstrap.Prelude
  ( module Whine.Prelude
  , module R
  , module Whine.Bootstrap.Prelude
  )
  where

import Whine.Prelude

import Control.Monad.Error.Class (class MonadError) as R
import Effect.Aff (Aff, launchAff_) as R
import Effect.Aff.Class (class MonadAff, liftAff) as R
import Effect.Class.Console as Console
import Node.Library.Execa (ExecaProcess, ExecaResult, execa) as R
import Node.Process (exit')
import Whine.Bootstrap.Hash (hashString) as R
import Whine.Bootstrap.Log (class Loggable, toDoc) as R

die :: ∀ m err a. MonadEffect m => R.Loggable err => err -> m a
die message = do
  Console.error $ R.toDoc message
  liftEffect $ exit' 1

rightOrDie :: ∀ m err a. MonadEffect m => R.Loggable err => Either err a -> m a
rightOrDie = either die pure

tryOrDie :: ∀ m err a. MonadEffect m => R.MonadError err m => R.Loggable err => m a -> m a
tryOrDie = try >=> rightOrDie

execSuccessOrDie :: ∀ m. R.MonadAff m => String -> R.ExecaProcess -> m R.ExecaResult
execSuccessOrDie cmd proc = do
  res <- R.liftAff proc.getResult
  when (res.exitCode /= Just 0) do
    die $ "Error running '" <> cmd <> "':\n" <> show res.stderr
  pure res

execSuccessOrDie_ :: ∀ m. R.MonadAff m => String -> R.ExecaProcess -> m Unit
execSuccessOrDie_ cmd res = void $ execSuccessOrDie cmd res
