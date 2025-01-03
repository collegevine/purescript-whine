import mm from 'micromatch'
import { globSync } from 'glob'

export const test = ({ include, exclude }) => file => {
  return (
    (include.length == 0 || mm.isMatch(file, include)) &&
    (exclude.length == 0 || !mm.isMatch(file, exclude))
  )
}

export const glob = ({ include, exclude }) => () => globSync(include, { ignore: exclude })
