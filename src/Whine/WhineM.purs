module WhineM where

import Whine.Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadTrans, ReaderT, ask, asks, lift, runReaderT)
import Control.Monad.Writer (class MonadTell, runWriterT)
import Data.Array as Array
import Effect.Aff.Class (class MonadAff)
import Whine.Log (class MonadLog, LogSeverity, logDefault)
import Whine.Types (class MonadRules, Violation)

type Env = { logLevel :: LogSeverity }

newtype WhineM v m a = WhineM (WriterT (Array (Violation v)) (ReaderT Env m) a)
derive newtype instance Functor m => Functor (WhineM v m)
derive newtype instance Apply m => Apply (WhineM v m)
derive newtype instance Bind m => Bind (WhineM v m)
derive newtype instance Applicative m => Applicative (WhineM v m)
derive newtype instance Monad m => Monad (WhineM v m)
derive newtype instance Monad m => MonadAsk Env (WhineM v m)
derive newtype instance Monad m => MonadTell (Array (Violation v)) (WhineM v m)
derive newtype instance MonadEffect m => MonadEffect (WhineM v m)
derive newtype instance MonadAff m => MonadAff (WhineM v m)
derive newtype instance MonadThrow e m => MonadThrow e (WhineM v m)
derive newtype instance MonadError e m => MonadError e (WhineM v m)

instance MonadTrans (WhineM v) where
  lift = WhineM <<< lift <<< lift

instance MonadEffect m => MonadRules v (WhineM v m) where
  reportViolation = Array.singleton >>> tell

instance MonadEffect m => MonadLog (WhineM v m) where
  log severity message = do
    level <- asks _.logLevel
    logDefault { level, severity } message

runWhineM :: ∀ v m a. MonadEffect m => Env -> WhineM v m a -> m (a /\ Array (Violation v))
runWhineM env (WhineM m) = runReaderT (runWriterT m) env

mapViolations :: ∀ v w m a. Monad m => (Violation v -> Violation w) -> WhineM v m a -> WhineM w m a
mapViolations f (WhineM m) = WhineM $ m # mapWriterT \x -> do
  r /\ violations <- x
  pure $ r /\ (f <$> violations)

unliftWhineM :: ∀ @m v a x. MonadEffect m => (x -> WhineM v m a) -> WhineM v m (x -> m a)
unliftWhineM f = do
  env <- ask
  pure \x -> fst <$> runWhineM env (f x)
