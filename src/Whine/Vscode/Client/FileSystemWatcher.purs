module Vscode.Client.FileSystemWatcher
  ( FileSystemWatcher
  , createFileSystemWatcher
  )
  where

import Effect (Effect)

data FileSystemWatcher :: Type
data FileSystemWatcher

foreign import createFileSystemWatcher :: { glob :: String } -> Effect FileSystemWatcher
