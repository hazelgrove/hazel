// The browser globals the test suite needs, which Node.js lacks: minimal
// stubs for Ezjs_idb's module initialization, and the Z3 the page and the
// worker load, which the proof checker verifies algebra with.
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
if (typeof initZ3 === "undefined") {
  globalThis.initZ3 = require("hazel-z3/z3-sync.js");
}
