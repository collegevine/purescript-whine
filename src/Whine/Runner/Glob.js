import mm from 'micromatch'
export { globSync } from 'glob'

export const test = patterns => file => {
  const res = mm.isMatch(file, patterns)
  console.log('test', file, patterns, res)
  return res
}
