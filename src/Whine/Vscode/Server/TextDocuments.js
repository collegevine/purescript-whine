import LS from 'vscode-languageserver/node'
import { TextDocument } from 'vscode-languageserver-textdocument'

export const create = () => {
  const d = new LS.TextDocuments(TextDocument)
  d.onDidChangeContent(x => console.log('Got change', x))
  return d
}
export const listen = docs => connection => () => docs.listen(connection)
export const isTextDocument = d => !!(d && d.uri && d.languageId && d.version)

export const textDocumentUri = d => d.uri
