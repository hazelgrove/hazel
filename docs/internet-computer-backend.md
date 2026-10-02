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
| dfx project and deploy | `ic/dfx.json`, `ic/deploy-local.sh` |

The backend keeps each `kv` value in a cell of an Adapton DCG, inside one
Fumola interpreter state: a save is the put `` `hazel(N) := value ``, a read
forces the cell. `POST /eval` runs Fumola programs in that state.

## Running it

Needs `dfx` (`~/.local/share/dfx/bin`) and a checkout of the fumola branch at
`~/fumola-canister` (or set `FUMOLA_DIR`).

```
ic/deploy-local.sh                     # builds both, starts a replica, deploys
SKIP_HAZEL_BUILD=1 ic/deploy-local.sh  # reuse the Hazel release build
```

It prints Hazel's address, `http://<frontend id>.localhost:4943/`, and the
backend's, which the front end reaches at `<backend id>.raw.localhost:4943`
because the backend's HTTP answers are not certified.

`dfx stop` stops the replica. Its state is ephemeral: `dfx start --clean`,
which the script uses when no replica is running, starts empty.

## Not handled yet

- A write that fails is logged to the console and not retried.
- Two tabs writing at once: the last write wins.
- The cells' histories do not survive a canister upgrade; the text does.
