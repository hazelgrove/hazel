#!/usr/bin/env python3
"""Static file server that sets cross-origin isolation headers.

Test input generation runs Z3 as WebAssembly (the `z3-solver` package), which
is a pthreads build: it spawns worker threads and therefore needs
SharedArrayBuffer. Browsers only expose SharedArrayBuffer on cross-origin
isolated pages, i.e. ones served with these two headers. Python's stock
`http.server` doesn't send them, so `make serve` uses this instead.

`credentialless` (rather than `require-corp`) keeps cross-origin resources
such as Google Fonts loading without requiring them to send CORP headers.

Serves the current working directory. Usage: python3 coi_serve.py [PORT] [BIND]
"""

import sys
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer


class Handler(SimpleHTTPRequestHandler):
    extensions_map = {
        **SimpleHTTPRequestHandler.extensions_map,
        ".js": "text/javascript",
        ".mjs": "text/javascript",
        ".wasm": "application/wasm",
    }

    def end_headers(self):
        self.send_header("Cross-Origin-Opener-Policy", "same-origin")
        self.send_header("Cross-Origin-Embedder-Policy", "credentialless")
        super().end_headers()


if __name__ == "__main__":
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8000
    bind = sys.argv[2] if len(sys.argv) > 2 else "0.0.0.0"
    ThreadingHTTPServer((bind, port), Handler).serve_forever()
