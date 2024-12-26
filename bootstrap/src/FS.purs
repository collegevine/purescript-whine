module Whine.Bootstrap.FS
  ( module R
  , module Whine.Bootstrap.FS
  ) where

import Whine.Bootstrap.Prelude

import Node.FS.Aff as NodeFS
import Node.FS.Perms (permsReadWrite)
import Node.FS.Sync as NodeFSSync
import Node.Path (relative) as R

readFile :: ∀ m. MonadAff m => FilePath -> m String
readFile path = liftAff $
  NodeFS.readTextFile UTF8 path # tryOrDie

writeFile :: ∀ m. MonadAff m => FilePath -> String -> m Unit
writeFile path text = liftAff $
  NodeFS.writeTextFile UTF8 path text # tryOrDie

mkDirP :: ∀ m. MonadAff m => FilePath -> m Unit
mkDirP path = liftAff $
  NodeFS.mkdir' path { recursive: true, mode: permsReadWrite } # tryOrDie

exists :: ∀ m. MonadAff m => FilePath -> m Boolean
exists path = liftEffect $
  NodeFSSync.exists path

unlink :: ∀ m. MonadAff m => FilePath -> m Unit
unlink path = liftAff $
  NodeFS.unlink path # tryOrDie
