module WhineM where

import Whine.Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadTrans, ReaderT, ask, lift, runReaderT, withReaderT)
import Control.Monad.Writer (class MonadTell, runWriterT)
import Data.Array as Array
import Effect.Aff.Class (class MonadAff)
import PureScript.CST.Types (Module)
import Safe.Coerce (coerce)
import Type.Equality (class TypeEquals)
import Whine.Log (class MonadLog, LogSeverity, logDefault)
import Whine.Types (class MonadContext, class MonadReport, class MonadRules, Violation)

newtype CurrentModule = CurrentModule (∀ r. (∀ e. Module e -> r) -> r)

newtype WhineM v env m a = WhineM (WriterT (Array (Violation v)) (ReaderT env m) a)
derive newtype instance Functor m => Functor (WhineM v env m)
derive newtype instance Apply m => Apply (WhineM v env m)
derive newtype instance Bind m => Bind (WhineM v env m)
derive newtype instance Applicative m => Applicative (WhineM v env m)
derive newtype instance Monad m => Monad (WhineM v env m)
derive newtype instance Monad m => MonadAsk env (WhineM v env m)
derive newtype instance Monad m => MonadTell (Array (Violation v)) (WhineM v env m)
derive newtype instance MonadEffect m => MonadEffect (WhineM v env m)
derive newtype instance MonadAff m => MonadAff (WhineM v env m)
derive newtype instance MonadThrow e m => MonadThrow e (WhineM v env m)
derive newtype instance MonadError e m => MonadError e (WhineM v env m)

instance MonadTrans (WhineM v env) where
  lift = WhineM <<< lift <<< lift

instance Monad m => MonadReport v (WhineM v env m) where
  reportViolation =
    Array.singleton >>> tell

instance (Monad m, TypeEquals env { currentModule :: CurrentModule | r }) => MonadContext (WhineM v env m) where
  currentModule f = do
    env <- ask
    case coerce env :: { currentModule :: CurrentModule | r } of
      { currentModule: CurrentModule h } -> h f

instance (MonadEffect m, TypeEquals env { logLevel :: LogSeverity | r }) => MonadLog (WhineM v env m) where
  log severity message = do
    level <- ask <#> (coerce :: env -> { logLevel :: LogSeverity | r }) <#> _.logLevel
    logDefault { level, severity } message

instance (MonadEffect m, TypeEquals env { logLevel :: LogSeverity, currentModule :: CurrentModule | r }) => MonadRules v (WhineM v env m)

runWhineM :: ∀ v env m a. MonadEffect m => env -> WhineM v env m a -> m (a /\ Array (Violation v))
runWhineM env (WhineM m) = runReaderT (runWriterT m) env

mapEnv :: ∀ v env env' m a. (env -> env') -> WhineM v env' m a -> WhineM v env m a
mapEnv f (WhineM m) = WhineM $ mapWriterT (withReaderT f) m

mapViolations :: ∀ v w env m a. Monad m => (Violation v -> Violation w) -> WhineM v env m a -> WhineM w env m a
mapViolations f (WhineM m) = WhineM $ m # mapWriterT \x -> do
  r /\ violations <- x
  pure $ r /\ (f <$> violations)

unliftWhineM :: ∀ @m env v a x. MonadEffect m => (x -> WhineM v env m a) -> WhineM v env m (x -> m a)
unliftWhineM f = do
  env <- ask
  pure \x -> fst <$> runWhineM env (f x)
