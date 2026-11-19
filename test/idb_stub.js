// The browser globals the test suite needs, which Node.js lacks: minimal
// stubs for Ezjs_idb's module initialization, and the Algebrite the page
// and the worker load, which the proof checker verifies algebra with.
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
if (typeof Algebrite === "undefined") {
  globalThis.Algebrite = require("algebrite");
}
