# The Fumola runtime, when it changes

## Why this document exists

The Fumola livelits in Hazel are backed by a wasm runtime built from a
*different repository* ([Adapton/fumola](https://github.com/Adapton/fumola)),
and Hazel pins no version of it. There is no submodule, no revision in a
manifest, no lockfile entry. The coupling is entirely:

> build from `$FUMOLA_REPO` if you have it, otherwise fetch whatever is
> published at `fumola.org`.

That is a deliberate trade — see [livelits.md](livelits.md) — and it has a
consequence worth writing down: **a change merged in the Fumola repo reaches
every Hazel user without any commit in this repository.** Nothing in Hazel's
CI will notice. Nothing in Hazel's test suite will fail.

This document records the first time we walked that seam deliberately, what we
learned about which parts are actually load-bearing, and the testing
methodology that follows from it.

## The dependency, precisely

Three sources, tried in order, by
[`prebundle.js`](../src/web/www/prebundle.js) (~line 107):

1. `./fumola/fumola_wasm.js` — local artifacts under
   `src/web/www/fumola/`, produced by
   [`scripts/build-fumola-wasm.sh`](../scripts/build-fumola-wasm.sh). These
   are **gitignored** (`.gitignore:86`); the wasm is ~5.6 MB.
2. `https://fumola.org` — the published runtime. Its `runtime.json` is read
   first, naming a content-addressed directory for the current build; the
   unversioned pair at the origin root is the fallback when the manifest
   cannot be reached.
3. `https://adapton.github.io/fumola` — the same, under its pre-CNAME name,
   kept because links to it were shared.

Each source gets 30 s before the loader moves on, so one stalled source cannot
hold up the others. On success the bridge sets `window.fumola` and dispatches
a `fumola-runtime-ready` DOM event, which
[`Main.re`](../src/web/Main.re) listens for so a program elaborated before the
runtime arrived is recalculated.

`window.fumola.source()` reports which source answered *and which version* —
`"fumola.org @ 290846d3aad19d38"`. **Check it before drawing any conclusion
from a browser session** — a developer with stale local artifacts and a
developer with none are running different runtimes at the same commit.

### What Hazel actually calls

This is the contract, and it is much narrower than the runtime's surface. The
published runtime exports 17 functions. The bridge calls **six**:

```
fumola_create   fumola_has     fumola_realize
fumola_eval     fumola_eval_top   fumola_ensure_mode
```

The other eleven — `fumola_modules`, `fumola_module_source`, `fumola_tokens`,
`fumola_get`, `fumola_reset`, `fumola_drop`, `fumola_mode`,
`fumola_instance_count`, `fumola_eval_scratch`, `fumola_symbol_of`,
`fumola_symbol_source` — exist for Fumola's own web-play page, not for Hazel.

Knowing this list is what turns "did that Fumola change break the livelits?"
from a worry into a two-minute check.

## Event log

### 2026-09-07 — imports can name a sibling directory (fumola#74)

**What changed in Fumola.** Import paths gained `.` and `..` resolution
(Adapton/fumola#55, merged as `8abca92`). Twenty symlinks that had stood in
for the missing `../..` were deleted, and the library's imports were rewritten
to spell where files actually are. A second bug surfaced and was fixed: the
import stack's active-path restore read the bottom of the stack instead of the
top, which the symlinks had been masking.

**Library-visible effect.** The registered library went from 37 paths to 17
modules. Paths that no longer exist: `fumola/system/hashMap`,
`fumola/collections/adapton`, `fumola/examples/mergeSort/adapton`, and
seventeen more. The `link` flag on `fumola_modules()` was removed, since the
duplicates it existed to hide are gone.

**Blast radius in Hazel: none, and here is why.** The only externally visible
API change was to `fumola_modules()`, which is in the eleven Hazel does not
call. The six it does call were untouched. The published and locally built
runtimes were confirmed to have an identical 17-function export list.

That "none" is a verified claim, not an assumption, and it held. But the check
that produced it is narrower than it looked — see the addendum below, where
the same change broke a page in the Fumola repo and this method could not have
seen it.

**Verification performed.**

| Check | Result |
| --- | --- |
| Fumola workspace tests | 32/32 groups pass |
| Fumola library suite (`./fumola-test.sh`) | 32/32, run 6× for ordering |
| Published wasm carries the merged library | `../../system/adapton` present; the three dead symlink paths absent |
| Export list, published vs local | identical, 17 functions |
| Every bridge call satisfied by published runtime | 6/6 |
| Hazel `FumolaValue` group | 54/54 |
| Hazel `FumolaSource` group | 21/21 |

**Note on the last two.** They passed, and they would have passed just as
happily against the *old* runtime, or with the wasm deleted. See the gap below.

#### Addendum: what the export-diff check does not cover

fumola#74 did break something — not in Hazel, but in Fumola's own web-play
page, and it is worth recording because **the check I used could not have found
it.**

`importsOf` in `pages/web-play/index.html` decided whether an import path was
absolute by testing for a `/`. That was correct while a bare name meant a
symlinked neighbour. Deleting the symlinks made every relative path contain a
slash, so the test silently began meaning something else, and opening an
example body failed with `ModuleFileNotFound("../system/adapton")`. Found and
fixed by the session working on that page (fumola#78).

Nothing about the API changed. The export list was identical, all six of
Hazel's calls resolved, and every one of those checks passed while the feature
was broken. What changed was **the shape of the data flowing through an
unchanged interface**.

The breakage was also invisible from the most obvious test path, which is the
part worth internalising. The default view of that page still worked:
`exampleMergeSort`'s body names no alias but `M`, and unused imports are
filtered out before they can fail. It took clicking the one body that reached
for its module's *neighbours*. So "I opened the page and it was fine" was true
and meant nothing.

So "the contract is six functions wide" is true and is not sufficient. The
export diff catches a changed *signature*; it is blind to a changed
*convention* — a path that gains a slash, an id that starts being a string, a
list that starts arriving empty rather than absent. Those are exactly the
changes a generated fixture would catch (tier 4) and a hand-written one will
not, because a hand-written fixture keeps asserting the old convention in
perfect health.

When reading a Fumola diff for blast radius, ask both questions: did any
signature change, **and** did the meaning of any value crossing the boundary
change? The second has no mechanical check today.

## The coverage gap

Stated plainly, because it is easy to mistake green tests for coverage here.

**Nothing, in either repository, tests a Fumola livelit against a live
runtime.**

- `FumolaValue` (54 cases) feeds
  [`FumolaValue.re`](../src/language/FumolaValue.re) **hand-written JSON
  literals** and a stubbed `~eval` callback. Its own header says the livelit
  "can only be exercised in a browser with the wasm runtime loaded". Real
  coverage of the translation step; zero coverage of the runtime.
- `FumolaSource` (21 cases) is the mirror image — Hazel value to Fumola source
  text, no runtime.
- `Evaluator.Livelit` **explicitly skips all four** Fumola livelits
  (`fumola_new`, `fumola_put_force`, `fumola_eval`, `fumola_with`), because
  there is no `window.fumola` under the node test runner. It raises
  `Skip_livelit` and swallows it.
- [`prebundle.js`](../src/web/www/prebundle.js), the JS↔wasm bridge — the
  three-source loader, the 30 s timeout, the ready event, the argument
  marshalling — has **no test of any kind**.
- There is no headless browser harness in the repo. No playwright, puppeteer,
  selenium or cypress; `package.json` has no `scripts` section.
- On the Fumola side, `cargo test -p fumola_wasm` exercises the Rust functions
  **natively**. `test_instances.rs` says so: "These run natively; nothing here
  needs a browser or a wasm host." So even those do not test the compiled
  module.

The JSON fixtures in `FumolaValue` are the one place the two repos' agreement
is written down, and they are *transcribed by hand*. If the runtime's reply
shape changes, those fixtures keep asserting the old shape, in perfect health,
forever.

## Testing methodology

Four tiers. Tier 1 is cheap enough to run on every Fumola change; tier 4 is
the one that does not exist yet.

### Tier 1 — the contract check (minutes, scriptable)

Run this whenever Fumola's `main` moves. It is what caught (or rather,
cleared) fumola#74.

0. **Do not trust a fetch made in the ten minutes after a deploy.** Pages
   serves `max-age=600` through an edge cache, and the edge will hand you a
   pre-deploy object inside that window. Observed directly on 2026-09-07: a
   fetch just after a successful deploy returned bytes *byte-identical to the
   pre-deploy copy*, and a fetch a minute later returned the new object with
   `X-Cache: MISS` and a matching `Last-Modified`. A deploy was nearly
   reported as half-broken on the strength of the first read.

   So when verifying a deploy, check `X-Cache` and the content, not just the
   status code, and re-fetch before concluding anything. This is the same
   failure the content-addressed URLs remove for *page* loads — but the
   unversioned URLs stay published for older clients, so verifying by fetching
   them keeps this hazard.

1. **Confirm what is published, and from which commit.** The deploy workflow
   triggers on push to `main`, and `actions-gh-pages` records the source
   commit in the gh-pages commit message:

   ```
   gh api repos/Adapton/fumola/commits/gh-pages --jq '.commit.message'
   # => deploy: 8abca927dc26b46c07994ff1f4a03cc3d8cf462b
   ```

   That is GitHub attesting the provenance. From a browser, ask the page
   instead: `window.fumola.source()` — see "Provenance" below.

2. **Diff the export list** between the published runtime and the one you
   built. Any change here is a change to the contract:

   ```
   grep -oE '^export function [a-z_0-9]+' fumola_wasm.js | sed 's/.*function //' | sort
   ```

3. **Confirm every bridge call is satisfied.** Extract the `fumola_*` symbols
   `prebundle.js` references and check each against that export list. Beware
   that a naive grep also matches the *filenames* `fumola_wasm` and
   `fumola_wasm_bg` — those are not calls.

4. **Spot-check the embedded library** if the change touched `fumola/`:
   `strings fumola_wasm_bg.wasm | grep <module path>`.

Byte-identity between published and local is **not** an expected outcome and
its absence means nothing: CI builds `--no-default-features` (dropping the
repl's rustyline, which does not compile for wasm32) on nightly, so the
published binary is legitimately smaller. Compare content and API, never
hashes.

### Tier 2 — the OCaml groups (seconds)

```
./run_tests test 'FumolaValue' -q
./run_tests test 'FumolaSource' -q
```

These verify the translation boundaries in both directions and the livelit
models' round-tripping. Necessary, and not sufficient — they cannot fail
because of a runtime change.

### Tier 3 — the browser, deliberately (manual, ~10 minutes)

The only tier that exercises a livelit end to end today.

```
make dev && make serve      # http://0.0.0.0:8000/
```

Then, in order:

1. Open the console and read `window.fumola.source()`. Know which runtime you
   are testing before you test it.
2. Exercise each of the four livelits — `fumola_new`, `fumola_put_force`,
   `fumola_eval`, `fumola_with` — and the `fumola-peek` projector.
3. **Test the fallback path on purpose.** It is otherwise never exercised:
   move `src/web/www/fumola/` aside, reload, and confirm the loader reaches
   `fumola.org` and `source()` says so. This is the path every user who has
   not built locally is on, and it is the path a Fumola deploy can break
   without any signal.
4. Restore the local artifacts.

Step 3 is the one people skip, and it is the one that matters most after a
Fumola deploy.

### Tier 4 — what should exist and does not

Two pieces, in priority order.

**A generated fixture, replacing the hand-transcribed one.** Have the Fumola
repo emit the JSON its runtime actually returns for a corpus of programs, and
have Hazel's `FumolaValue` tests read *that* file rather than string literals
typed out by a person. Then a reply-shape change in Fumola fails a Hazel test
instead of silently invalidating a fixture that keeps passing. This closes the
real gap and needs no browser.

**A headless smoke test of the bridge.** Load the wasm under node or
playwright, call the six functions the bridge calls, and assert their reply
shapes. That covers `prebundle.js`, which today has nothing. Smaller value
than the fixture work, but it is the only thing that would ever test the
loader.

## Provenance: which Fumola is this page holding?

Answered, as of the content-addressed publishing work for
[fumola#69](https://github.com/Adapton/fumola/issues/69).

The published site carries a `runtime.json` naming a content-addressed
directory for the current build, and the bridge reads it before importing
anything. So `window.fumola.source()` now returns the version as well as the
origin:

```
"fumola.org @ 290846d3aad19d38"     the current build, by content hash
"fumola.org @ stable"               the manifest was unreachable; the
                                    unversioned pair at the origin root
"local @ stable"                    your own scripts/build-fumola-wasm.sh
```

Console gets the same line at load. **Read it before drawing any conclusion
from a browser session** — it is the first question worth asking when a
livelit misbehaves for someone and not for you.

This also removes the older proposal to compile a `fumola_version()` export
into the wasm via `build.rs`. The manifest is the version stamp, it costs
nothing extra, and it is the same fact GitHub records in the gh-pages commit
message (`deploy: <sha>`) — which remains the way to check provenance without
a browser:

```
gh api repos/Adapton/fumola/commits/gh-pages --jq '.commit.message'
```

### What content addressing does and does not buy

Worth stating exactly, because it is easy to over-claim. GitHub Pages stamps
`Cache-Control: max-age=600` on every asset and offers no way to change it —
no `immutable`, no long max-age. So hashing buys **no** caching improvement.
It buys:

- **freshness by construction** — a new build has a new URL, so a browser's
  cached copy of the old one is simply never requested again. This is the
  mechanism that fixes the skew, not cache headers.
- **pinning** — `/v/<hash>/` bytes never change.

Pins are bounded: the publish workflow retains a fixed number of recent
versions (`--keep`, currently 10) because each is ~5.6 MB. A pin is good for
the near term, not forever. `versions.json` lists what is currently retained.
