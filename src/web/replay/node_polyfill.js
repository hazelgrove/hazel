// Node.js polyfills for Hazel log replay script.
// These must load before any OCaml/js_of_ocaml code to stub out
// browser APIs (localStorage, IndexedDB, DOM) that aren't available in Node.js.

// Minimal DOM stubs for js_of_ocaml Dom_html bindings
if (typeof globalThis.window === 'undefined') {
  var noop = function() {};
  var noopObj = function() { return {}; };

  // Minimal Element stub
  function StubElement(tag) {
    this.tagName = tag || 'DIV';
    this.childNodes = [];
    this.children = [];
    this.style = {};
    this.classList = { add: noop, remove: noop, toggle: noop, contains: function() { return false; } };
    this.dataset = {};
    this.attributes = {};
    this.parentNode = null;
    this.innerHTML = '';
    this.textContent = '';
    this.value = '';
  }
  StubElement.prototype.appendChild = function(c) { this.childNodes.push(c); return c; };
  StubElement.prototype.removeChild = function(c) { return c; };
  StubElement.prototype.insertBefore = function(n) { return n; };
  StubElement.prototype.setAttribute = function() {};
  StubElement.prototype.getAttribute = function() { return null; };
  StubElement.prototype.removeAttribute = noop;
  StubElement.prototype.addEventListener = noop;
  StubElement.prototype.removeEventListener = noop;
  StubElement.prototype.dispatchEvent = function() { return true; };
  StubElement.prototype.focus = noop;
  StubElement.prototype.blur = noop;
  StubElement.prototype.click = noop;
  StubElement.prototype.getBoundingClientRect = function() {
    return { top: 0, left: 0, bottom: 0, right: 0, width: 0, height: 0 };
  };
  StubElement.prototype.querySelector = function() { return null; };
  StubElement.prototype.querySelectorAll = function() { return []; };
  StubElement.prototype.getElementsByTagName = function() { return []; };
  StubElement.prototype.getElementsByClassName = function() { return []; };
  StubElement.prototype.cloneNode = function() { return new StubElement(this.tagName); };

  var bodyElement = new StubElement('BODY');
  var docElement = new StubElement('HTML');

  var stubDocument = {
    createElement: function(tag) { return new StubElement(tag); },
    createElementNS: function(ns, tag) { return new StubElement(tag); },
    createTextNode: function(t) { return { textContent: t, nodeType: 3 }; },
    createDocumentFragment: function() { return new StubElement('FRAGMENT'); },
    createComment: function() { return { nodeType: 8 }; },
    createEvent: function() {
      return { initEvent: noop, preventDefault: noop, stopPropagation: noop };
    },
    body: bodyElement,
    documentElement: docElement,
    head: new StubElement('HEAD'),
    getElementById: function() { return null; },
    querySelector: function() { return null; },
    querySelectorAll: function() { return []; },
    getElementsByTagName: function() { return []; },
    getElementsByClassName: function() { return []; },
    addEventListener: noop,
    removeEventListener: noop,
    cookie: '',
    readyState: 'complete',
    title: '',
    location: { href: '', protocol: 'file:', host: '', pathname: '/' },
    activeElement: bodyElement,
  };

  var stubWindow = {
    document: stubDocument,
    location: stubDocument.location,
    navigator: { userAgent: 'node', platform: 'node' },
    history: { pushState: noop, replaceState: noop, back: noop, forward: noop },
    addEventListener: noop,
    removeEventListener: noop,
    dispatchEvent: function() { return true; },
    setTimeout: setTimeout,
    clearTimeout: clearTimeout,
    setInterval: setInterval,
    clearInterval: clearInterval,
    requestAnimationFrame: function(cb) { return setTimeout(cb, 16); },
    cancelAnimationFrame: function(id) { clearTimeout(id); },
    getComputedStyle: function() { return {}; },
    matchMedia: function() { return { matches: false, addEventListener: noop }; },
    innerWidth: 1024,
    innerHeight: 768,
    scrollX: 0,
    scrollY: 0,
    pageXOffset: 0,
    pageYOffset: 0,
    screen: { width: 1024, height: 768 },
    performance: typeof performance !== 'undefined' ? performance : { now: function() { return Date.now(); } },
    getSelection: function() {
      return { rangeCount: 0, getRangeAt: noopObj, removeAllRanges: noop, addRange: noop };
    },
    MutationObserver: function() { this.observe = noop; this.disconnect = noop; },
    ResizeObserver: function() { this.observe = noop; this.disconnect = noop; },
    IntersectionObserver: function() { this.observe = noop; this.disconnect = noop; },
    CustomEvent: function(type, params) { this.type = type; this.detail = params ? params.detail : null; },
    Event: function(type) { this.type = type; },
    XMLHttpRequest: function() {
      this.open = noop; this.send = noop; this.setRequestHeader = noop;
      this.addEventListener = noop;
    },
    fetch: function() { return Promise.reject(new Error('fetch not available in Node.js replay')); },
    btoa: function(s) { return Buffer.from(s, 'binary').toString('base64'); },
    atob: function(s) { return Buffer.from(s, 'base64').toString('binary'); },
    URL: typeof URL !== 'undefined' ? URL : function(u) { this.href = u; },
    Blob: typeof Blob !== 'undefined' ? Blob : function() {},
    File: function() {},
    FileReader: function() { this.readAsText = noop; this.readAsArrayBuffer = noop; this.addEventListener = noop; },
    console: console,
  };

  globalThis.window = stubWindow;
  globalThis.document = stubDocument;
  globalThis.navigator = stubWindow.navigator;
  globalThis.Element = StubElement;
  globalThis.HTMLElement = StubElement;
  globalThis.Node = StubElement;
  globalThis.MutationObserver = stubWindow.MutationObserver;
  globalThis.ResizeObserver = stubWindow.ResizeObserver;
  globalThis.IntersectionObserver = stubWindow.IntersectionObserver;
  globalThis.CustomEvent = stubWindow.CustomEvent;
  globalThis.Event = stubWindow.Event;
  globalThis.XMLHttpRequest = stubWindow.XMLHttpRequest;
  globalThis.getComputedStyle = stubWindow.getComputedStyle;
  globalThis.requestAnimationFrame = stubWindow.requestAnimationFrame;
  globalThis.cancelAnimationFrame = stubWindow.cancelAnimationFrame;
}

// Web Worker stub
if (typeof globalThis.Worker === 'undefined') {
  globalThis.Worker = function(url) {
    this.url = url;
    this.onmessage = null;
    this.onerror = null;
  };
  globalThis.Worker.prototype.postMessage = function() {};
  globalThis.Worker.prototype.terminate = function() {};
  globalThis.Worker.prototype.addEventListener = function() {};
  globalThis.Worker.prototype.removeEventListener = function() {};
}

// localStorage polyfill
if (typeof globalThis.localStorage === 'undefined') {
  var store = {};
  globalThis.localStorage = {
    getItem: function(k) { return store.hasOwnProperty(k) ? store[k] : null; },
    setItem: function(k, v) { store[k] = String(v); },
    removeItem: function(k) { delete store[k]; },
    clear: function() { store = {}; },
    get length() { return Object.keys(store).length; },
    key: function(n) { return Object.keys(store)[n] || null; }
  };
}
// Also set on window if it exists
if (typeof globalThis.window !== 'undefined' && !globalThis.window.localStorage) {
  globalThis.window.localStorage = globalThis.localStorage;
}

// Stub IndexedDB APIs used by ezjs_idb
if (typeof globalThis.indexedDB === 'undefined') {
  globalThis.indexedDB = {
    open: function() {
      return {
        onupgradeneeded: null,
        onsuccess: null,
        onerror: null,
        result: null
      };
    }
  };
}
if (typeof globalThis.IDBKeyRange === 'undefined') {
  globalThis.IDBKeyRange = {
    only: function(v) { return v; },
    lowerBound: function(v) { return v; },
    upperBound: function(v) { return v; },
    bound: function(l, u) { return [l, u]; }
  };
}
if (typeof globalThis.IDBTransaction === 'undefined') {
  globalThis.IDBTransaction = {};
}
