import LS from 'vscode-languageserver/node.js'
import { TextDocument } from 'vscode-languageserver-textdocument'

export const create_ = () => new LS.TextDocuments(TextDocument)
export const listen_ = (docs, connection) => docs.listen(connection)
export const isTextDocument = d => !!(d && d.uri && d.languageId && d.version)

export const uri = d => d.uri
export const getText_ = d => d.getText()
