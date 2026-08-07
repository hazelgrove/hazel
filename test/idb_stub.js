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

// Some linked web modules construct their worker client during module
// initialization even though unit tests never dispatch work through it. Node
// does not provide the browser Worker constructor, so keep test startup
// independent of module-link order with the same inert interface used by the
// CLI preload.
if (typeof Worker === "undefined") {
  globalThis.Worker = class Worker {
    constructor() {}
    postMessage() {}
    terminate() {}
    addEventListener() {}
    removeEventListener() {}
  };
}
