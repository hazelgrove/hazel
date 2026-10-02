#!/usr/bin/env bash
# icp's build step for fumola: build fumola_canister from the fumola
# checkout and hand icp the wasm, with its Candid interface embedded.
set -euo pipefail
FUMOLA_DIR="${FUMOLA_DIR:-$HOME/fumola-canister}"
(cd "$FUMOLA_DIR" && cargo build --target wasm32-unknown-unknown --release -p fumola_canister)
cp "$FUMOLA_DIR/target/wasm32-unknown-unknown/release/fumola_canister.wasm" "$ICP_WASM_OUTPUT_PATH"
ic-wasm "$ICP_WASM_OUTPUT_PATH" -o "$ICP_WASM_OUTPUT_PATH" metadata candid:service \
  -f "$FUMOLA_DIR/crates/fumola_canister/fumola_canister.did" -v public --keep-name-section
