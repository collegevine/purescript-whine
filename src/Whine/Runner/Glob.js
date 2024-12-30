import mm from 'micromatch'
export { globSync } from 'glob'

export const test = patterns => file => mm.isMatch(file, patterns)
