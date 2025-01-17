import fs from 'fs-extra';

export const copyTree_ = (source) => (dest) => () => fs.copySync(source, dest);
