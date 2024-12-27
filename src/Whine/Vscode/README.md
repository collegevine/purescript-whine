This whole directory is just FFI bindings for VSCode API, and in truth it should
be a separate subproject. But for now it cannot be, because then the
`whine-core` library couldn't be published to PureScript Registry, because it
would be referencing a library that is not itself in the registry, and the
`vscode` library itself couldn't be published either, because the Registry
doesn't support monorepos yet.
