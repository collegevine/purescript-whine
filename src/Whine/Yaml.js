import Yaml from 'yaml'

export const parseYaml_ = (left, right, text) => {
  try { return right(Yaml.parse(text)) }
  catch (error) { return left(error.message) }
}

export const stringify = Yaml.stringify
