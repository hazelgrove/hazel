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
