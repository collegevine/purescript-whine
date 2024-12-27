export const unsafeOn = (event, handler, target) => target[`on${event}`](x => { console.log(`Got ${event}`); handler(x); })
