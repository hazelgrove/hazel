# BigInt WebWorker Serialization Fix

This document explains the vendored zarith runtime and why it's needed.

**Vendored file:** `vendor/zarith_native_bigint_runtime.js`
**Source:** `zarith_stubs_js` v0.17.0
**Purpose:** Fixes BigInt serialization through WebWorker `postMessage`

### The Problem

Hazel uses a WebWorker for evaluation to avoid blocking the UI. Data (including
the AST with BigInt values) is sent to/from the worker via `postMessage`, which
uses the browser's [structured clone algorithm](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Structured_clone_algorithm).

The `zarith_stubs_js` package (v0.16.x) that provides BigInt support for
js_of_ocaml uses the [BigInteger.js](https://github.com/peterolson/BigInteger.js)
library internally. BigInteger.js creates custom JavaScript objects with
prototype methods like `.lt()`, `.add()`, etc. When these objects pass through
structured clone, **the prototype chain is lost** - the data survives but the
methods don't. This caused crashes when displaying evaluation results containing
large integers.

### The Solution

Starting in v0.17.0, `zarith_stubs_js` switched to using **native JavaScript
`BigInt`** instead of BigInteger.js. Native `BigInt` is a primitive type that
fully survives structured clone.

However, we cannot simply upgrade to zarith_stubs_js v0.17.0 because it requires
upgrading the entire Jane Street package ecosystem (bonsai, incr_dom, etc.) to
v0.17.0, which in turn requires `js_of_ocaml < 5.7.0`, which is incompatible
with OCaml 5.2.0. This dependency chain made a straightforward upgrade impossible
at the time of this fix (January 2026).

### Our Approach

We vendor the `runtime.js` from zarith_stubs_js v0.17.0 and copy it into the
opam switch at build time, replacing the v0.16.x version. This gives us native
BigInt support without upgrading the package.

The `Makefile` has a `setup-zarith` target that performs this copy:

```makefile
setup-zarith:
    cp vendor/zarith_native_bigint_runtime.js "$$(opam var lib)/zarith_stubs_js/runtime.js"
```

This target is automatically run before builds (`make dev`, `make release`, etc.)
and in CI.

### Alternatives Considered

1. **Full package upgrade** - Blocked by OCaml version / js_of_ocaml constraints
   as described above.

2. **O(n) serialization shim** - Our previous approach: recursively walk the
   entire message payload before/after `postMessage`, converting BigInts to
   tagged strings (`{__hazel_bigint__: "123"}`) and back. This worked but added
   overhead proportional to message size (4 traversals per worker round-trip).

3. **Different BigInt library** - Would require replacing zarith throughout the
   codebase (~23 files, ~112 usages). Zarith is deeply integrated and provides
   the `Z.t` type used in OCaml.

4. **Comlink or other RPC libraries** - These still use structured clone under
   the hood, so they don't solve the fundamental problem.

### Future Considerations

- **When Jane Street releases packages compatible with OCaml 5.2+ and
  js_of_ocaml 5.7+**, we should be able to remove this vendored file and do a
  proper upgrade. At that point:
  1. Upgrade zarith_stubs_js to v0.17.0+ via opam
  2. Remove `vendor/zarith_native_bigint_runtime.js`
  3. Remove the `setup-zarith` target from the Makefile
  4. Remove the `setup-zarith` dependencies from other Makefile targets

- **If zarith_stubs_js changes its internal representation again**, we may need
  to update the vendored file. Check the [zarith_stubs_js releases](https://github.com/janestreet/zarith_stubs_js)
  for changes.

- **The vendored runtime.js API is stable** - it implements the same `ml_z_*`
  functions as the original, just with native BigInt instead of BigInteger.js.
  There should be no compatibility issues.

### Related Files

- `vendor/zarith_native_bigint_runtime.js` - The vendored runtime file
- `src/web/util/WorkerClient.re` - Client-side worker communication
- `src/web/util/WorkerServer.re` - Worker-side message handling
- `Makefile` - `setup-zarith` target
- `.github/workflows/deploy_branches.yml` - CI setup-zarith step
