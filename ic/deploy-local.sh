#!/usr/bin/env bash
# Deploy the canister demo to a LOCAL replica: Hazel's web build in an
# assets canister, its state in the Fumola-backed backend canister.
# Nothing here touches the IC mainnet.
#
#   ic/deploy-local.sh            build everything, start a replica if none
#                                 is running, deploy both canisters
#   SKIP_HAZEL_BUILD=1 ...        reuse the existing Hazel release build
#   FUMOLA_DIR=... ...            the Adapton/fumola checkout with
#                                 crates/fumola_canister (default
#                                 ~/fumola-canister, branch ic-canister)
set -euo pipefail
IC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$IC_DIR/.." && pwd)"
FUMOLA_DIR="${FUMOLA_DIR:-$HOME/fumola-canister}"
export PATH="$HOME/.local/share/dfx/bin:$PATH"

echo "== backend: fumola_canister (from $FUMOLA_DIR)"
(cd "$FUMOLA_DIR" && cargo build --target wasm32-unknown-unknown --release -p fumola_canister)
mkdir -p "$IC_DIR/build"
cp "$FUMOLA_DIR/target/wasm32-unknown-unknown/release/fumola_canister.wasm" "$IC_DIR/build/"
cp "$FUMOLA_DIR/crates/fumola_canister/fumola_canister.did" "$IC_DIR/build/"

if [ -z "${SKIP_HAZEL_BUILD:-}" ]; then
  echo "== frontend: Hazel release build"
  (cd "$REPO" && opam exec -- dune build src/web/www --profile release)
fi

cd "$IC_DIR"
if ! dfx ping local >/dev/null 2>&1; then
  echo "== starting a local replica"
  dfx start --background --clean
fi

echo "== deploying the backend"
dfx deploy hazel_backend
BACKEND_ID="$(dfx canister id hazel_backend)"
# raw: the backend's HTTP responses are not certified
BACKEND_URL="http://${BACKEND_ID}.raw.localhost:4943"

echo "== assembling the frontend's files"
rm -rf dist && mkdir dist
WWW="$REPO/src/web/www"
BUILT="$REPO/_build/default/src/web/www"
(cd "$WWW" && tar cf - --exclude=dune --exclude=prebundle.js .) | (cd dist && tar xf -)
cp "$BUILT/hazel.js" "$BUILT/worker.js" "$BUILT/bundled.js" dist/
cp assets.ic-assets.json5 dist/.ic-assets.json5
cat > dist/config.js <<JS
// Written by ic/deploy-local.sh: Hazel's state lives in this canister.
window.hazelBackend = "${BACKEND_URL}";
JS

echo "== deploying the frontend"
dfx deploy hazel_frontend
FRONTEND_ID="$(dfx canister id hazel_frontend)"

echo
echo "Hazel:   http://${FRONTEND_ID}.localhost:4943/"
echo "Backend: ${BACKEND_URL}/health"
