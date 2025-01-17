module Test.FS where

import Test.Prelude

import Data.UUID as UUID
import Node.FS.Aff as FS
import Node.FS.Perms as Perms
import Node.FS.Sync as SyncFS
import Node.OS as NodeOS
import Node.Process as Node

cwd :: ∀ m. MonadAff m => m String
cwd = liftEffect Node.cwd

chdir :: ∀ m. MonadAff m => String -> m Unit
chdir = liftEffect <<< Node.chdir

exists :: ∀ m. MonadAff m => FilePath -> m Boolean
exists = liftEffect <<< SyncFS.exists

newTempDir :: ∀ m. MonadAff m => m String
newTempDir = do
  tmpdir <- liftEffect $ (\t u -> t <> "/" <> UUID.toString u) <$> NodeOS.tmpdir <*> UUID.genUUID
  liftAff $ FS.mkdir' tmpdir { recursive: true, mode: Perms.permsAll }
  pure tmpdir

writeTextFile :: ∀ m. MonadAff m => FilePath -> String -> m Unit
writeTextFile path text = liftAff $ FS.writeTextFile UTF8 path text

readTextFile :: ∀ m. MonadAff m => FilePath -> m String
readTextFile = liftAff <<< FS.readTextFile UTF8

rmdir :: ∀ m. MonadAff m => FilePath -> m Unit
rmdir path = liftAff $ FS.rm' path { force: true, recursive: true, maxRetries: 3, retryDelay: 100 }

readdir :: ∀ m. MonadAff m => FilePath -> m (Array FilePath)
readdir = liftAff <<< FS.readdir

copyTree :: ∀ m. MonadAff m => FilePath -> FilePath -> m Unit
copyTree src dest = liftEffect $ copyTree_ src dest

foreign import copyTree_ :: FilePath -> FilePath -> Effect Unit
