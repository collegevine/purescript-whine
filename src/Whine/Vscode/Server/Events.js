export const unsafeOn = (event, handler, target) => target[`on${event}`](handler)
