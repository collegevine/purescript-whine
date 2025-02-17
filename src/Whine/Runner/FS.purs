module Whine.Runner.FS
  ( module R
  , module Whine.Runner.FS
  ) where

import Whine.Runner.Prelude

import Data.DateTime (DateTime)
import Node.FS.Aff as NodeFS
import Node.FS.Perms (permsReadWrite)
import Node.FS.Stats as NodeStats
import Node.FS.Sync as NodeFSSync
import Node.Path (relative) as R

readFile :: FilePath -> RunnerM String
readFile path = tryOrDie $ liftAff $
  NodeFS.readTextFile UTF8 path

writeFile :: FilePath -> String -> RunnerM Unit
writeFile path text = tryOrDie $ liftAff $
  NodeFS.writeTextFile UTF8 path text

mkDirP :: FilePath -> RunnerM Unit
mkDirP path = tryOrDie $ liftAff $
  NodeFS.mkdir' path { recursive: true, mode: permsReadWrite }

exists :: FilePath -> RunnerM Boolean
exists path = liftEffect $
  NodeFSSync.exists path

unlink :: FilePath -> RunnerM Unit
unlink path = tryOrDie $ liftAff $
  NodeFS.unlink path

stat :: FilePath -> RunnerM { modifiedTime :: DateTime }
stat path = do
  stats <- liftAff $ NodeFS.stat path
  pure { modifiedTime: NodeStats.modifiedTime stats }
