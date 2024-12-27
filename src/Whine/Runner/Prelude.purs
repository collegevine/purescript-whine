module Whine.Runner.Prelude
  ( module Whine.Prelude
  , module R
  , module Whine.Runner.Prelude
  )
  where

import Whine.Prelude

import Control.Monad.Error.Class (class MonadError) as R
import Control.Monad.Reader (ReaderT) as R
import Effect.Aff (Aff, launchAff_) as R
import Effect.Aff.Class (class MonadAff, liftAff) as R
import Node.Process (exit')
import Whine.Runner.Log (class Loggable, class MonadLog, LogSeverity(..), log, logDebug, logError, logInfo, logWarning, toDoc) as R

type Env = { logLevel :: R.LogSeverity }

type RunnerM = R.ReaderT Env R.Aff

die :: ∀ err a. R.Loggable err => err -> RunnerM a
die message = do
  R.logError message
  exit 1

rightOrDie :: ∀ err a. R.Loggable err => Either err a -> RunnerM a
rightOrDie = either die pure

tryOrDie :: ∀ a. RunnerM a -> RunnerM a
tryOrDie = try >=> rightOrDie

exit :: ∀ a. Int -> RunnerM a
exit = liftEffect <<< exit'