#!/usr/bin/env bash
# Build the Fumola wasm runtime that backs the Fumola livelit, and install the
# generated artifacts into Hazel's static assets.
#
# The artifacts are generated rather than checked in: the wasm binary is
# several megabytes. Without them Hazel still builds, and the Fumola livelit
# reports that its runtime is unavailable.
#
# Note: the Fumola build is slow (a few minutes from cold) because of lalrpop
# parser generation. Keep the cargo cache warm rather than running clean.
set -euo pipefail

FUMOLA_REPO="${FUMOLA_REPO:-$HOME/fumola}"
HAZEL_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="$HAZEL_ROOT/src/web/www/fumola"

if [ ! -d "$FUMOLA_REPO" ]; then
  echo "Fumola repo not found at $FUMOLA_REPO." >&2
  echo "Clone github.com/Adapton/fumola and set FUMOLA_REPO." >&2
  exit 1
fi

if ! command -v wasm-bindgen >/dev/null 2>&1; then
  echo "wasm-bindgen not found. Install the version matching the" >&2
  echo "wasm-bindgen crate, e.g. cargo install wasm-bindgen-cli --version 0.2.108" >&2
  exit 1
fi

echo "Building fumola_wasm for wasm32-unknown-unknown..."
cd "$FUMOLA_REPO"
rustup target add wasm32-unknown-unknown
cargo build --release --target wasm32-unknown-unknown -p fumola_wasm

echo "Generating JS bindings into $OUT_DIR..."
mkdir -p "$OUT_DIR"
wasm-bindgen --target web --out-dir "$OUT_DIR" \
  target/wasm32-unknown-unknown/release/fumola_wasm.wasm

echo "Done. Built:"
ls -la "$OUT_DIR"
echo
echo "These are not committed. Hazel falls back to the published runtime at"
echo "https://adapton.github.io/fumola/ when they are absent, so building"
echo "locally is only needed when working on Fumola and Hazel together."
