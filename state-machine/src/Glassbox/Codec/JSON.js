// Two spaces, stable key order as the encoder emitted them. The point of
// showing this in the UI is that it is the artifact as the DECODER understood
// it, re-encoded — not the bytes that arrived. If it differs from the file on
// disk, the codec lost something, and you can see that with your eyes.
export const stringifyPrettyImpl = (json) => JSON.stringify(json, null, 2)
