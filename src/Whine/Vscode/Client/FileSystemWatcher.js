import { workspace } from 'vscode';

export const createFileSystemWatcher = ({ glob }) => () => workspace.createFileSystemWatcher(glob);
