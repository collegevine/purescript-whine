import LC from 'vscode-languageclient/node'

export const createLanguageClient = args => () =>
    new LC.LanguageClient(
      args.id, args.name, args.serverOptions, args.clientOptions
    )

export const start = client => () => { client.start(); return }
