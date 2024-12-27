module Vscode.Context
  ( ExtensionContext
  , asAbsolutePath
  )
  where


data ExtensionContext :: Type
data ExtensionContext

foreign import asAbsolutePath :: String -> ExtensionContext -> String
