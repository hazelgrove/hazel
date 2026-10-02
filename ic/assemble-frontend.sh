#!/usr/bin/env bash
# icp's build step for hazel: Hazel's release build, assembled into
# dist/. The static files come from the source www/ (dune copies them into
# _build only lazily), the scripts from the build. SKIP_HAZEL_BUILD=1 reuses
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
# The certified-assets canister adds no headers of its own. The filenames
# carry no content hash, so have browsers revalidate.
cat > "$IC_DIR/dist/_headers" <<'HEADERS'
/*
  Cache-Control: max-age=0, must-revalidate
HEADERS
