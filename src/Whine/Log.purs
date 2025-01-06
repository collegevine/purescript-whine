module Whine.Log where

import Whine.Prelude

import Codec.JSON.DecodeError as DecodeError
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as Format
import Data.List as List
import Effect.Class.Console as Console
import Effect.Exception as Err
import Effect.Now (nowDateTime)

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

class MonadLog m where
  log :: ∀ a. Loggable a => LogSeverity -> a -> m Unit

logDefault :: ∀ m a. MonadEffect m => Loggable a => { level :: LogSeverity, severity :: LogSeverity } -> a -> m Unit
logDefault { level, severity } message = do
  when (severity >= level) $
    printFn $ toDoc message
  where
    printFn = case severity of
      LogInfo ->
        Console.info
      LogWarning ->
        Console.warn
      LogError ->
        Console.error
      LogDebug -> \s -> do
        now <- liftEffect nowDateTime
        Console.debug $ fold ["[", formatDebugTime now, "]: ", s]

logDebug :: ∀ m a. MonadLog m => Loggable a => a -> m Unit
logDebug = log LogDebug

logInfo :: ∀ m a. MonadLog m => Loggable a => a -> m Unit
logInfo = log LogInfo

logWarning :: ∀ m a. MonadLog m => Loggable a => a -> m Unit
logWarning = log LogWarning

logError :: ∀ m a. MonadLog m => Loggable a => a -> m Unit
logError = log LogError

formatDebugTime :: DateTime -> String
formatDebugTime = Format.format $ List.fromFoldable
  [ Format.Hours24
  , Format.Placeholder ":"
  , Format.MinutesTwoDigits
  , Format.Placeholder ":"
  , Format.SecondsTwoDigits
  , Format.Placeholder "."
  , Format.Milliseconds
  ]
