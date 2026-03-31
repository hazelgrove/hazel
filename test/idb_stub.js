// Minimal stubs for browser globals needed by Ezjs_idb module initialization.
// The test suite runs in Node.js which lacks these browser APIs.
if (typeof window === "undefined") {
  globalThis.window = globalThis;
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
