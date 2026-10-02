#!/usr/bin/env bash
# icp's presync step for hazel: once the canisters exist, write the
# backend's address into dist/config.js, which HazelDB reads. Through the
# gateway's raw address, since the backend's HTTP answers are not certified;
# on the page's own host and port, so the gateway's port need not be known
# here.
set -euo pipefail
IC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
: "${ICP_CLI_CID_FUMOLA:?no fumola canister id}"
cat > "$IC_DIR/dist/config.js" <<JS
// Written by ic/write-config.sh at deploy: Hazel's state lives in the
// fumola canister, ${ICP_CLI_CID_FUMOLA}.
window.hazelBackend =
  location.protocol + "//${ICP_CLI_CID_FUMOLA}.raw.localhost" +
  (location.port ? ":" + location.port : "");
JS

# The CSP's connect-src allows only 'self' and openrouter.ai; Hazel's state
# lives in the backend, so add its raw origin, on any port (the gateway's
# port is not known here).
python3 - "$IC_DIR/dist/_headers" "$ICP_CLI_CID_FUMOLA" <<'PY2'
import sys
p, cid = sys.argv[1], sys.argv[2]
h = open(p, encoding="utf-8").read()
assert "connect-src 'self'" in h, "_headers has no connect-src to extend"
origin = "http://%s.raw.localhost:*" % cid
if origin not in h:
    h = h.replace("connect-src 'self'", "connect-src 'self' " + origin)
open(p, "w", encoding="utf-8").write(h)
print("_headers: connect-src allows " + origin)
PY2
