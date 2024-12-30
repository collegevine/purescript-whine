import VSC from 'vscode'

const outputChannel = VSC.window.createOutputChannel('whine-trace');

export const trace_ = msg => () => outputChannel.appendLine(msg)
