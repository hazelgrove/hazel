# Hazel (collaborative) for Patchwork

A Patchwork datatype + tool (`hazel`) for editing a Hazel program together:
the program lives in an Automerge doc as items with text leaves, and the
modular-editors Hazel runs in an iframe with its own Automerge replica. See
[docs/collab-modular.md](../docs/collab-modular.md).

Try it: in Patchwork, My tools → Register a Module →
`automerge:3LHEjqPrFdnCwsLwXtJfjJGXamkd`, then Create new → Hazel.

## Build & publish

```bash
# at the repo root: a release build of Hazel (hazel.js ~6.5 MB; dev is ~90 MB)
dune build src --profile release
# the collab bundle's deps
(cd collab && npm install)

cd patchwork-tool
npm install
npm run build              # vite build + copy Hazel's build and collab.js into dist/hazel/
npx pushwork@latest init . # first time only: mints a new module URL
npx pushwork@latest sync   # publish
```

`.pushwork/` (the module URL + sync state) isn't committed, so `init` gives
you your own module; to update the shared one above, `pushwork clone` it
first.

Debugging: `localStorage["hazel-collab-debug"] = "1"` in the Hazel frame
logs the traffic between Hazel and the Automerge session.
