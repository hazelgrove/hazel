# Hazel's state in a canister (local demo)

This branch serves Hazel from a local Internet Computer replica and keeps its
state in a backend canister instead of the browser's IndexedDB. Local only:
nothing here deploys to the IC mainnet. Design and status:
https://claude.ai/code/artifact/68cbf43f-3a8e-4d39-83f0-a2789589af8c

## Pieces

| Piece | Where |
| --- | --- |
| Backend canister, `fumola_canister` | `Adapton/fumola`, branch `ic-canister`, `crates/fumola_canister` |
| Storage switch | `src/web/HazelDB.re` (`Backend`), `src/web/www/ic-backend.js` |
| Backend address | `src/web/www/config.js`: `null` here, so a normal build uses IndexedDB; the deploy writes the canister's address |
| icp-cli project | `ic/icp.yaml`, with its build and presync steps in `ic/*.sh` |
| Response headers | `src/web/www/_headers`, the ic-canister branch's CSP; the deploy adds the inline script's hash and the backend's origin |

The backend keeps each `kv` value in a cell of an Adapton DCG, inside one
Fumola interpreter state: a save is the put `` `hazel(N) := value ``, a read
forces the cell. The value is a Fumola value, not Hazel's text: Hazel saves
S-expressions, stored as `#atom("text")` and `#list([...])` and printed back
on a read, byte for byte as Hazel wrote them. A value that is not one
S-expression stays text (a slide's caret, saved as `0 0`).

For values whose type the canister knows (`crates/fumola_canister/src/
schema.rs`), the value is shaped like Hazel's own type: records as objects
with their field names, variants as `#Ctor`, options as `null` / `?x`. That is
`SETTINGS` (`Settings.Model.t` and the types it holds); a document's
program text, each item a `Base.segment` of `#Tile` / `#Grout` / `#Secondary`
/ `#Projector` / `#Splice` pieces, and the roster that orders them; a slide's
editor state (`doc:<slide name>`, a `#CodePersist` with the editor's root,
zipper and result); and the deck index (`doc:_meta`: the current slide and
the slide names). Parts with no schema yet, such as the agent chat inside a
slide's state, stay generic inside the typed record. A value is stored typed only when encoding it again
gives back exactly what Hazel saved, so a schema that drifts from Hazel's
source falls back to the generic form instead of corrupting anything.

`POST /eval` runs Fumola programs in that state. Each cell is bound as
`hazelCell<N>`; `GET /index` maps Hazel's keys to the numbers. For example,
with `MODE` in cell 1:

```
switch (@ hazelCell1) { case (#atom(t)) { t }; case _ { "?" } }
```

and, with `SETTINGS` in cell 2:

```
let s = @ hazelCell2; (s.core.format_shortcut, s.sidebar.panel)
```

## Running it

Needs `icp-cli` and `ic-wasm` (`npm install -g @icp-sdk/icp-cli
@icp-sdk/ic-wasm`, Node >= 22), the `wasm32-unknown-unknown` Rust target, and a
checkout of the fumola branch at `~/fumola-canister` (or set `FUMOLA_DIR`).

```
cd ic
icp network start -d
icp deploy                      # builds both; SKIP_HAZEL_BUILD=1 reuses Hazel's release build
icp network stop
```

`ic/deploy-local.sh` does the first two. Two canisters:

| Canister | Address | Is |
| --- | --- | --- |
| `hazel` | `http://hazel.local.localhost:8000/` | Hazel, via the `@dfinity/static-site` recipe |
| `fumola` | `http://<id>.raw.localhost:8000/` | `fumola_canister`, built by `build-backend.sh` |

The front end reaches the backend at its `raw` address, because the backend's
HTTP answers are not certified. `write-config.sh` runs at sync, once the ids
exist, and writes that address into `dist/config.js`.

The canister names have no underscore on purpose. A canister's local address
is `<name>.local.localhost`, and `js_of_ocaml`'s URL parser, which Hazel reads
`?slide=` links through, refuses an underscore in a host name: the page loads,
but every query parameter is lost.

## Not handled yet

- A write that fails is logged to the console and not retried.
- Two tabs writing at once: the last write wins.
- The cells' histories do not survive a canister upgrade; the values do.
- Typed so far: `SETTINGS`, the deck index, slides' editor states, and
  documents' items and rosters. The rest (probes, pins, a slide's view, the
  agent chat, `MODE`, the ExplainThis model) is generic S-expressions, a
  record's fields being lists of two atoms.
