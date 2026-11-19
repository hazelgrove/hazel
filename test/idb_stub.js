// Minimal stubs for browser globals needed by Ezjs_idb module initialization.
// The test suite runs in Node.js which lacks these browser APIs.
if (typeof window === "undefined") {
  globalThis.window = globalThis;
}
// Ensure parent === window so PatchworkComm.is_in_iframe() returns false
// in the test environment (Node.js has no parent property).
if (typeof parent === "undefined") {
  globalThis.parent = globalThis;
}
if (typeof IDBKeyRange === "undefined") {
  globalThis.IDBKeyRange = {
    only: () => ({}),
    bound: () => ({}),
    lowerBound: () => ({}),
    upperBound: () => ({}),
  };
}
if (typeof indexedDB === "undefined") {
  globalThis.indexedDB = {open: () => ({result: null})};
}
