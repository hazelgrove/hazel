// A minimal XMLHttpRequest implementation for Node, sufficient for the subset
// of XHR that src/util/API.re uses (see API.re:139 `request` and API.re:219
// `request_streaming`). js_of_ocaml's `XmlHttpRequest.create()` compiles to
// `new globalThis.XMLHttpRequest()` (js_of_ocaml-compiler/runtime.js:6650), so
// installing this class as a global is all that is needed to make the browser
// HTTP path work headlessly under node.
//
// What is covered, because API.re reads exactly these:
//   open/setRequestHeader/send/abort, readyState, status, responseText,
//   withCredentials, onreadystatechange, onprogress, onerror.
// `responseText` accumulates across chunks and `onprogress` fires per chunk,
// which is what API.re's `drain` relies on to parse SSE incrementally: it
// tracks a byte offset into responseText rather than reading chunk payloads.
//
// NOT covered (nothing in API.re asks for them): synchronous requests,
// responseType/response/responseXML, timeouts, upload progress, the
// EventTarget interface (addEventListener), HTTP redirects beyond what node's
// http/https client does on its own (i.e. none), and `withCredentials`, which
// is accepted and ignored since there is no cookie jar.

const http = require("http");
const https = require("https");
const { URL } = require("url");

const UNSENT = 0;
const OPENED = 1;
const HEADERS_RECEIVED = 2;
const LOADING = 3;
const DONE = 4;

class NodeXMLHttpRequest {
  constructor() {
    this.readyState = UNSENT;
    this.status = 0;
    this.statusText = "";
    this.responseText = "";
    this.withCredentials = false;
    this.onreadystatechange = null;
    this.onprogress = null;
    this.onerror = null;
    this.onload = null;
    this._headers = {};
    this._req = null;
    this._aborted = false;
  }

  _setReadyState(rs) {
    this.readyState = rs;
    if (typeof this.onreadystatechange === "function") {
      this.onreadystatechange.call(this, { type: "readystatechange" });
    }
  }

  open(method, url, _async) {
    this._method = String(method);
    this._url = String(url);
    this._setReadyState(OPENED);
  }

  setRequestHeader(key, value) {
    this._headers[String(key)] = String(value);
  }

  abort() {
    this._aborted = true;
    if (this._req) {
      try {
        this._req.destroy();
      } catch (_) {}
    }
  }

  send(body) {
    const parsed = new URL(this._url);
    const mod = parsed.protocol === "http:" ? http : https;
    // node needs an explicit content-length/encoding; the browser derives it.
    const payload =
      body === null || body === undefined ? null : Buffer.from(String(body));
    const headers = Object.assign({}, this._headers);
    if (payload && headers["Content-Length"] === undefined) {
      headers["Content-Length"] = String(payload.length);
    }
    const req = mod.request(
      {
        protocol: parsed.protocol,
        hostname: parsed.hostname,
        port: parsed.port || undefined,
        path: parsed.pathname + parsed.search,
        method: this._method,
        headers,
      },
      (res) => {
        this.status = res.statusCode || 0;
        this.statusText = res.statusMessage || "";
        this._setReadyState(HEADERS_RECEIVED);
        res.setEncoding("utf8");
        res.on("data", (chunk) => {
          if (this._aborted) return;
          this.responseText += chunk;
          if (this.readyState !== LOADING) this._setReadyState(LOADING);
          if (typeof this.onprogress === "function") {
            this.onprogress.call(this, { type: "progress" });
          }
        });
        res.on("end", () => {
          if (this._aborted) return;
          this._setReadyState(DONE);
          if (typeof this.onload === "function") {
            this.onload.call(this, { type: "load" });
          }
        });
      },
    );
    this._req = req;
    req.on("error", (_err) => {
      if (this._aborted) return;
      // Mirror the browser: on a transport error status resets to 0 and the
      // error handler runs; API.re's `fail` (API.re:273) keys off that.
      this.status = 0;
      if (typeof this.onerror === "function") {
        this.onerror.call(this, { type: "error" });
      }
      if (this.readyState !== DONE) this._setReadyState(DONE);
    });
    if (payload) req.write(payload);
    req.end();
  }
}

NodeXMLHttpRequest.UNSENT = UNSENT;
NodeXMLHttpRequest.OPENED = OPENED;
NodeXMLHttpRequest.HEADERS_RECEIVED = HEADERS_RECEIVED;
NodeXMLHttpRequest.LOADING = LOADING;
NodeXMLHttpRequest.DONE = DONE;

if (typeof globalThis.XMLHttpRequest === "undefined") {
  globalThis.XMLHttpRequest = NodeXMLHttpRequest;
}

module.exports = NodeXMLHttpRequest;
