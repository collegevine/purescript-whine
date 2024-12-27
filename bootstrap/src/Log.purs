module Whine.Bootstrap.Log where

import Whine.Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Reader (class MonadReader, asks)
import Effect.Class.Console as Console
import Effect.Exception as Err

class Loggable a where
  toDoc :: a -> String

instance Loggable String where
  toDoc = identity

instance Loggable Err.Error where
  toDoc = Err.message

instance Loggable DecodeError.DecodeError where
  toDoc = DecodeError.print

data LogSeverity = LogDebug | LogInfo | LogWarning | LogError
derive instance Eq LogSeverity
derive instance Ord LogSeverity

class MonadEffect m <= MonadLog m where
  log :: ∀ a. Loggable a => LogSeverity -> a -> m Unit

instance (MonadEffect m, MonadReader { logLevel :: LogSeverity | r } m) => MonadLog m where
  log severity message = do
    level <- asks _.logLevel
    when (severity >= level) $
      printFn $ toDoc message
    where
      printFn = case severity of
        LogDebug -> Console.debug
        LogInfo -> Console.info
        LogWarning -> Console.warn
        LogError -> Console.error

logDebug :: ∀ m a. MonadLog m => Loggable a => a -> m Unit
logDebug = log LogDebug

logInfo :: ∀ m a. MonadLog m => Loggable a => a -> m Unit
logInfo = log LogInfo

logWarning :: ∀ m a. MonadLog m => Loggable a => a -> m Unit
logWarning = log LogWarning

logError :: ∀ m a. MonadLog m => Loggable a => a -> m Unit
logError = log LogError
