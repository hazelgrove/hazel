// Headless browser mocks for running compiled js_of_ocaml in Node.js.
// Used by both the test suite and the batch log replay script.

const store = {};
globalThis.localStorage = {
  getItem: (key) => (key in store ? store[key] : null),
  setItem: (key, val) => {
    store[key] = String(val);
  },
  removeItem: (key) => {
    delete store[key];
  },
  clear: () => {
    for (const key in store) delete store[key];
  },
};

const noopElem = {
  style: {},
  classList: { add: () => {}, remove: () => {} },
  getBoundingClientRect: () => ({
    top: 0,
    left: 0,
    width: 0,
    height: 0,
    right: 0,
    bottom: 0,
  }),
  appendChild: () => {},
  setAttribute: () => {},
  addEventListener: () => {},
  removeEventListener: () => {},
  querySelectorAll: () => [],
  querySelector: () => null,
  innerHTML: "",
  textContent: "",
  scrollIntoView: () => {},
  focus: () => {},
  blur: () => {},
  // Textarea properties for CoerceTo.textarea (checks tagName/nodeName)
  tagName: "TEXTAREA",
  nodeName: "TEXTAREA",
  value: "",
  selectionStart: 0,
  selectionEnd: 0,
};

globalThis.document = {
  getElementById: () => ({ ...noopElem }),
  createElement: () => ({ ...noopElem }),
  createElementNS: () => ({ ...noopElem }),
  createTextNode: () => ({ ...noopElem }),
  createDocumentFragment: () => ({
    appendChild: () => {},
    querySelectorAll: () => [],
  }),
  body: { ...noopElem },
  documentElement: { ...noopElem },
  head: { ...noopElem },
  querySelector: () => null,
  querySelectorAll: () => [],
  addEventListener: () => {},
  removeEventListener: () => {},
  createRange: () => ({
    setStart: () => {},
    setEnd: () => {},
    getBoundingClientRect: () => ({
      top: 0,
      left: 0,
      width: 0,
      height: 0,
    }),
    getClientRects: () => [],
    commonAncestorContainer: { ...noopElem },
  }),
  cookie: "",
};

function fakeRequest(resultValue) {
  const req = {
    result: resultValue !== undefined ? resultValue : undefined,
    error: null,
    addEventListener: () => {},
  };
  Object.defineProperty(req, "onsuccess", {
    set() {},
    get() {
      return null;
    },
  });
  Object.defineProperty(req, "onerror", {
    set() {},
    get() {
      return null;
    },
  });
  Object.defineProperty(req, "onupgradeneeded", {
    set() {},
    get() {
      return null;
    },
  });
  return req;
}

globalThis.indexedDB = {
  open: () => {
    const fakeStore = {
      add: () => fakeRequest(),
      put: () => fakeRequest(),
      get: () => fakeRequest(),
      getAll: () => fakeRequest(),
      clear: () => fakeRequest(),
      delete: () => fakeRequest(),
    };
    const fakeTx = {
      objectStore: () => fakeStore,
    };
    const fakeDb = {
      transaction: () => fakeTx,
      createObjectStore: () => fakeStore,
    };
    return fakeRequest(fakeDb);
  },
};

globalThis.window = {
  navigator: { platform: "", userAgent: "" },
  location: { hash: "", href: "", search: "", pathname: "/" },
  addEventListener: () => {},
  removeEventListener: () => {},
  requestAnimationFrame: (cb) => setTimeout(cb, 0),
  cancelAnimationFrame: () => {},
  getComputedStyle: () => ({}),
  scrollTo: () => {},
  innerWidth: 1024,
  innerHeight: 768,
  devicePixelRatio: 1,
  performance: { now: () => Date.now() },
  document: globalThis.document,
  localStorage: globalThis.localStorage,
  indexedDB: globalThis.indexedDB,
};

globalThis.navigator = globalThis.window.navigator;
globalThis.performance = globalThis.window.performance;
globalThis.IDBKeyRange = {
  only: () => ({}),
  lowerBound: () => ({}),
  upperBound: () => ({}),
  bound: () => ({}),
};

globalThis.MutationObserver = class {
  observe() {}
  disconnect() {}
  takeRecords() {
    return [];
  }
};

globalThis.ResizeObserver = class {
  observe() {}
  unobserve() {}
  disconnect() {}
};

globalThis.IntersectionObserver = class {
  observe() {}
  unobserve() {}
  disconnect() {}
};

globalThis.Worker = class {
  constructor() {}
  postMessage() {}
  addEventListener() {}
  removeEventListener() {}
  terminate() {}
};

globalThis.XMLHttpRequest = class {
  open() {}
  send() {}
  addEventListener() {}
  setRequestHeader() {}
};
