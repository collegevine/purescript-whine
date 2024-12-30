import VSC from 'vscode'

export const workspace = VSC.workspace

export const getWorkspaceFolder_ = uri => workspace.getWorkspaceFolder(uri)

export const textDocuments_ = ws => ws.textDocuments
