#!/usr/bin/env bash
# icp's build step for hazel: Hazel's release build, assembled into
# dist/. The static files come from the source www/ (dune copies them into
# _build only lazily), _headers among them, the scripts from the build. SKIP_HAZEL_BUILD=1 reuses
# the existing release build, which takes tens of minutes from cold.
set -euo pipefail
IC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$IC_DIR/.." && pwd)"
if [ -z "${SKIP_HAZEL_BUILD:-}" ]; then
  (cd "$REPO" && opam exec -- dune build src/web/www --profile release)
fi
rm -rf "$IC_DIR/dist" && mkdir "$IC_DIR/dist"
(cd "$REPO/src/web/www" && tar cf - --exclude=dune --exclude=prebundle.js .) | (cd "$IC_DIR/dist" && tar xf -)
BUILT="$REPO/_build/default/src/web/www"
cp "$BUILT/hazel.js" "$BUILT/worker.js" "$BUILT/bundled.js" "$IC_DIR/dist/"
chmod u+w "$IC_DIR/dist"/*.js
# The CSP (www/_headers, copied above) allows script only from 'self', so
# each inline <script> in index.html gets its own hash as a script source.
python3 - "$IC_DIR/dist" <<'PY2'
import base64, hashlib, re, sys
d = sys.argv[1]
html = open(d + "/index.html", encoding="utf-8").read()
inline = re.findall(r"<script>(.*?)</script>", html, re.S)
hashes = " ".join(
    "'sha256-%s'" % base64.b64encode(hashlib.sha256(js.encode("utf-8")).digest()).decode()
    for js in inline)
p = d + "/_headers"
h = open(p, encoding="utf-8").read()
assert "script-src 'self' 'unsafe-eval'" in h, "_headers has no script-src to extend"
open(p, "w", encoding="utf-8").write(
    h.replace("script-src 'self' 'unsafe-eval'", "script-src 'self' 'unsafe-eval' " + hashes))
print("_headers: %d inline script hash(es)" % len(inline))
PY2
