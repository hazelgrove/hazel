const Worker = require("web-worker");
const path = require("path");
global.Worker = Worker;

const workerPath = path.join(process.cwd(), "_build/default/src/web/worker.bc.js");

const worker = new Worker(workerPath, { type: "module" });

// Stub browser storage APIs referenced by HazelDB / localStorage code paths
// that are transitively loaded by the grader entry point but never exercised
// at runtime during batch grading.
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

// Stub DOM APIs transitively referenced at module load time by UI modules
// that the grader entry point pulls in but doesn't actually execute.
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
