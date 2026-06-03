import LS from 'vscode-languageserver/node'

export const createConnection_ = () => LS.createConnection(LS.ProposedFeatures.all)

export const sendDiagnostics_ = params => conn => conn.sendDiagnostics(params)

export const listen_ = conn => () => conn.listen()
