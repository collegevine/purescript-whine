import mm from 'micromatch'
import { globSync } from 'glob'

export const test = ({ include, exclude }) => file => {
  console.log(file, {include, exclude})
  console.log(mm.isMatch(file, include))
  console.log(mm.isMatch(file, exclude))
  return (
    (include.length == 0 || mm.isMatch(file, include)) &&
    (exclude.length == 0 || !mm.isMatch(file, exclude))
  )
}

export const glob = ({ include, exclude }) => () => globSync(include, { ignore: exclude })
