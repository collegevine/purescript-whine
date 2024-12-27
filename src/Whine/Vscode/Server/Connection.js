import LS from 'vscode-languageserver/node'

export const createConnection = () => LS.createConnection(LS.ProposedFeatures.all)

export const sendDiagnostics = params => conn => conn.sendDiagnostics(params)

export const listen = conn => () => conn.listen()
