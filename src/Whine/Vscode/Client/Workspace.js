import VSC from 'vscode'

export const workspace = VSC.workspace

export const getWorkspaceFolder_ = uri => VSC.workspace.getWorkspaceFolder(uri)

export const getConfiguration_ = (section, ws) => ws.getConfiguration(section)
