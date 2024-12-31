import mm from 'micromatch'
import { globSync } from 'glob'

export const test = ({ include, exclude }) => file => mm.isMatch(file, [...include, ...exclude.map(e => `!${e}`)])

export const glob = ({ include, exclude }) => () => globSync(include, { ignore: exclude })
