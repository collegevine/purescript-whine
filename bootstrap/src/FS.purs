module Whine.Bootstrap.FS
  ( module R
  , module Whine.Bootstrap.FS
  ) where

import Whine.Bootstrap.Prelude

import Effect.Exception (Error)
import Node.FS.Aff as NodeFS
import Node.FS.Perms (permsReadWrite)
import Node.FS.Sync as NodeFSSync
import Node.Path (relative) as R

readFile :: ∀ m. MonadAff m => MonadLog m => MonadError Error m => FilePath -> m String
readFile path = tryOrDie $ liftAff $
  NodeFS.readTextFile UTF8 path

writeFile :: ∀ m. MonadAff m => MonadLog m => MonadError Error m => FilePath -> String -> m Unit
writeFile path text = tryOrDie $ liftAff $
  NodeFS.writeTextFile UTF8 path text

mkDirP :: ∀ m. MonadAff m => MonadLog m => MonadError Error m => FilePath -> m Unit
mkDirP path = tryOrDie $ liftAff $
  NodeFS.mkdir' path { recursive: true, mode: permsReadWrite }

exists :: ∀ m. MonadAff m => FilePath -> m Boolean
exists path = liftEffect $
  NodeFSSync.exists path

unlink :: ∀ m. MonadAff m => MonadLog m => MonadError Error m => FilePath -> m Unit
unlink path = tryOrDie $ liftAff $
  NodeFS.unlink path
