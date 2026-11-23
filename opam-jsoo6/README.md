# opam overlay for the js_of_ocaml 6 port (#1131)

A tiny opam repository that `scripts/jsoo6-switch.sh` ranks first in the
`hazel-jsoo6` switch. Each entry shadows the same package version from the
upstream repositories and adds one patch:

| package | patch | why |
|---|---|---|
| `basement.v0.18~preview.133.03+642` | `basement-mainline.patch` | (1) OCaml's `caml/misc.h` (under `CAML_INTERNALS`) defines a `fallthrough` macro that breaks `__has_attribute(fallthrough)` in the macOS SDK's `<dispatch/dispatch.h>`; undefine it around that include. (2) `Stdlib_shim.Dynamic` (behind `Basement.Dynamic`, which sexplib0, ppx_yojson_conv_lib and Core read on hot paths such as `sexp_of_float`) is built on OCaml effect handlers. js_of_ocaml raises `Failure "Effect handlers are not supported"` unless everything is compiled with `--effects=cps`/`double-translation`, and double translation broke the tail calls in Hazel's evaluator trampoline (stack overflow on `fib(19)`). On the JS/Wasm backends use a plain binding stack instead. |
| `lsp.1.26.1` | `ppx_yojson_conv_lib-v0.18.patch` | its checked-in ppx output reads `Yojson_conv.record_check_extra_fields` with `!`; in v0.18 it is a `bool Basement.Dynamic.t` (and `basement` must be a direct dependency, since lsp sets `implicit_transitive_deps false`). |
| `ocaml-lsp-server.1.26.1` | `base-v0.18.patch`, plus `stdune < 3.25` | `Base.List.compare`'s type mentions `Ppx_compare_lib`, which must now be a direct dependency; stdune 3.25 renamed `Pid.of_int` and retyped `Unix_env`, which 1.26.1 predates. |
| `ocamlformat-lib.0.29.0` | `base-v0.18-comparator.patch` | Base v0.18 made `Comparator.t` abstract; use `Comparator.sexp_of_t`. |

Not patched here but also needed:

- `src/util/basement_stubs.js` (in Hazel): JS versions of basement primitives
  that have C stubs but no JS ones (`basement_domain_self_index`,
  `caml_atomic_*_stub_loc`). Without them module initialisation throws
  `runtime.basement_domain_self_index is not a function`.

Handled by the setup script:

- `bisect_ppx`: pinned to the branch of ocaml-ppx/bisect_ppx#448 (ppxlib 0.36+).
- `incremental`: its build runs `sed` over sources with non-UTF-8 bytes, which
  macOS `sed` rejects under a UTF-8 locale; install with `LC_ALL=C`.

After editing a file here, run `opam update --switch=hazel-jsoo6 hazel-jsoo6-overlay`.
