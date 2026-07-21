// Node.js preload for the Hazel CLI (hazel). The CLI links against the
// `web` library for access to grading logic (so we can grade submissions
// without invoking a browser build). The `web` library pulls in HazelDB
// (IndexedDB-backed persistence) and UI modules that reference browser
// globals at module load time, none of which are needed during CLI
// operation. We stub them out here so the CLI runs cleanly under node.

const noopIDBRequest = () => ({
  onsuccess: null,
  onerror: null,
  onupgradeneeded: null,
  result: null,
});
global.IDBKeyRange = {
  only: () => ({}),
  lowerBound: () => ({}),
  upperBound: () => ({}),
  bound: () => ({}),
};
const fakeIndexedDB = {
  open: noopIDBRequest,
  deleteDatabase: noopIDBRequest,
};
const fakeLocalStorage = {
  getItem: () => null,
  setItem: () => {},
  removeItem: () => {},
  clear: () => {},
  key: () => null,
  length: 0,
};
if (typeof global.window === "undefined") {
  global.window = {};
}
global.window.indexedDB = fakeIndexedDB;
global.window.localStorage = fakeLocalStorage;
global.indexedDB = fakeIndexedDB;
global.localStorage = fakeLocalStorage;

const makeFakeElement = () => {
  const el = {
    id: "",
    className: "",
    innerHTML: "",
    textContent: "",
    style: {},
    children: [],
    childNodes: [],
    shadowRoot: null,
    attachShadow: function () { this.shadowRoot = makeFakeElement(); return this.shadowRoot; },
    appendChild: function (c) { this.children.push(c); this.childNodes.push(c); return c; },
    removeChild: function (c) { return c; },
    insertBefore: function (c) { return c; },
    cloneNode: function () { return makeFakeElement(); },
    getElementById: function () { return makeFakeElement(); },
    querySelector: function () { return makeFakeElement(); },
    querySelectorAll: function () { return []; },
    addEventListener: function () {},
    removeEventListener: function () {},
    setAttribute: function () {},
    getAttribute: function () { return null; },
    removeAttribute: function () {},
    getBoundingClientRect: function () {
      return { top: 0, left: 0, right: 0, bottom: 0, width: 0, height: 0, x: 0, y: 0 };
    },
    focus: function () {},
    blur: function () {},
    click: function () {},
    dispatchEvent: function () { return true; },
  };
  return el;
};
const fakeDocument = makeFakeElement();
fakeDocument.body = makeFakeElement();
fakeDocument.head = makeFakeElement();
fakeDocument.documentElement = makeFakeElement();
fakeDocument.createElement = function () { return makeFakeElement(); };
fakeDocument.createTextNode = function () { return makeFakeElement(); };
fakeDocument.createDocumentFragment = function () { return makeFakeElement(); };
global.document = fakeDocument;
global.window.document = fakeDocument;
global.navigator = global.navigator || { userAgent: "node", clipboard: {} };
global.window.navigator = global.navigator;

// Stub Worker so that module-scope `new Worker(...)` code doesn't crash.
// The CLI does not actually use workers at runtime.
if (typeof global.Worker === "undefined") {
  global.Worker = class Worker {
    constructor() {}
    postMessage() {}
    terminate() {}
    addEventListener() {}
    removeEventListener() {}
  };
}
