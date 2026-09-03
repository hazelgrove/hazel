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

// Synchronous XMLHttpRequest backed by curl, for CLI commands that make
// real network calls (agent-eval). jsoo's XmlHttpRequest binding targets the
// browser global; grading and friends never touch it, so this is inert for
// every other command. Synchronous on purpose: the CLI's entry point calls
// exit() as soon as the command function returns, so async work would be
// dropped — with a sync send(), response handlers (including the SSE
// streaming drain, which reads the fully-buffered responseText at DONE) run
// to completion before control returns.
if (typeof global.XMLHttpRequest === "undefined") {
  const { execFileSync } = require("child_process");
  const fsx = require("fs");
  const osx = require("os");
  const pathx = require("path");
  global.XMLHttpRequest = class XMLHttpRequest {
    constructor() {
      this.readyState = 0;
      this.status = 0;
      this.responseText = "";
      this.withCredentials = false;
      this.onreadystatechange = null;
      this.onprogress = null;
      this.onerror = null;
      this._headers = [];
    }
    open(method, url) {
      this._method = method;
      this._url = url;
      this.readyState = 1;
    }
    setRequestHeader(k, v) {
      this._headers.push("-H", `${k}: ${v}`);
    }
    abort() {}
    send(body) {
      const tmp = fsx.mkdtempSync(pathx.join(osx.tmpdir(), "hazel-xhr-"));
      const bodyFile = pathx.join(tmp, "body");
      const outFile = pathx.join(tmp, "out");
      try {
        const args = ["-sS", "-X", this._method, this._url, ...this._headers,
                      "-o", outFile, "-w", "%{http_code}",
                      "--max-time", "300"];
        if (body != null) {
          fsx.writeFileSync(bodyFile, String(body));
          args.push("--data-binary", `@${bodyFile}`);
        }
        const codeStr = execFileSync("curl", args, { encoding: "utf8" });
        this.status = parseInt(codeStr.trim(), 10) || 0;
        this.responseText = fsx.existsSync(outFile)
          ? fsx.readFileSync(outFile, "utf8")
          : "";
        this.readyState = 4;
        if (this.onprogress) this.onprogress({});
        if (this.onreadystatechange) this.onreadystatechange();
      } catch (e) {
        this.status = 0;
        this.responseText = "";
        this.readyState = 4;
        if (this.onerror) this.onerror(e);
        if (this.onreadystatechange) this.onreadystatechange();
      } finally {
        fsx.rmSync(tmp, { recursive: true, force: true });
      }
    }
  };
}
