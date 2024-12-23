import crypto from 'crypto'

export const hashString = (str) => {
  const h = crypto.createHash('sha256')
  h.update(str)
  return h.digest('hex')
}
