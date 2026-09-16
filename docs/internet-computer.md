# Hazel as an Internet Computer canister

Hazel's web build is a static bundle with no backend, so it can be served
directly from an Internet Computer canister as certified assets: no origin
server, no CDN, and the served bytes are signature-checked by the boundary
node rather than trusted.

This is a *second* deploy target. It does not replace the branch previews at
`hazel.org/build/<branch>/`, which keep working exactly as before
(`.github/workflows/deploy.yml`).

## What is in the repo

| File | Role |
| --- | --- |
| `icp.yaml` | Declares one canister, `hazel`, built by the `@dfinity/static-site` recipe from `_build/default/src/web/www`. |
| `src/web/www/_headers` | Every response header the browser sees, CSP included. Dune copies it into the build directory. |

## Prerequisites

`icp-cli` needs Node >= 22 (this repo already requires Node for esbuild):

```
npm install -g @icp-sdk/icp-cli @icp-sdk/ic-wasm
```

On a minimal Linux install you may also need `libdbus-1-3 libssl3
ca-certificates`.

## Deploying locally

```
icp network start -d
icp deploy
icp network stop
```

`icp deploy` runs the release build first, which takes tens of minutes from
cold. When the build is already current, skip it:

```
make release
icp sync hazel
```

## Deploying to mainnet

Mainnet needs a named identity and cycles. The anonymous identity is funded on
the local network only.

```
icp identity new hazel-deploy
icp identity default hazel-deploy
icp identity account-id        # fund this
icp cycles balance -n ic
icp deploy -e ic hazel
```

Mainnet canister IDs land in `.icp/data/mappings/ic.ids.json`. **Commit that
directory** -- losing it breaks the name-to-canister mapping. Only
`.icp/cache/` is gitignored.

## Size and cost

Measured against the release build currently served at `hazel.org/build/dev`:

| Asset | Size |
| --- | --- |
| `hazel.js` | 5.9 MB |
| `worker.js` | 1.2 MB |
| `bundled.js` | 0.8 MB |
| `img/`, `style/` | ~2.5 MB |

Note the release profile matters enormously here: the same `hazel.js` built
with `--profile dev` is 84 MB. Deploy the release build.

Only `hazel.js` exceeds the 2 MB ingress message limit, so it uploads in
chunks; the CLI handles that. The canister also stores gzip and brotli
encodings, negotiated per request.

At roughly 10 MB of assets, storage runs about two cents a month (storage is
127,000 cycles per GiB per second on a 13-node subnet, ~$0.43 per GiB-month).
Creating the canister costs 500B cycles, about $0.65. Cycles are fixed by
protocol at 1T cycles = 1 XDR.

## Headers

The certified-assets canister adds no headers of its own, so `_headers` is the
whole story -- there is no default CSP or `Cache-Control` behind it. The
reasoning for each CSP source is recorded in the file itself. Two are worth
repeating here because they are easy to break:

- **`'unsafe-eval'` is required.** Both `hazel.js` and `bundled.js` evaluate
  strings as JavaScript. Removing it leaves the app stuck on the loading
  spinner, with `EvalError` in the console.
- **`connect-src` must allow `https://openrouter.ai`.** The agent panel calls
  the OpenRouter API directly from the browser (`src/util/OpenRouter.re`).
  Without it the agent fails while the rest of the editor looks fine.

Assets are served with `max-age=0, must-revalidate` because the filenames carry
no content hash; the canister supplies `ETag`, so repeat visits revalidate
cheaply instead of re-downloading.

## Not done here

- No CI job deploys this. Mainnet deploys need a funded identity, so they stay
  manual until someone decides where that key lives.
- Material Icons still comes from Google. Self-hosting it would let both
  `fonts.*` entries out of the CSP and make the canister genuinely
  self-contained.
