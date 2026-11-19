# opam overlay for the js_of_ocaml 6 port (#1131)

A tiny opam repository that `scripts/jsoo6-switch.sh` ranks first in the
`hazel-jsoo6` switch. Each entry shadows the same package version from the
upstream repositories and adds one patch:

| package | patch | why |
|---|---|---|
| `basement.v0.18~preview.133.03+642` | `macos-fallthrough.patch` | OCaml's `caml/misc.h` (under `CAML_INTERNALS`) defines a `fallthrough` macro that breaks `__has_attribute(fallthrough)` in the macOS SDK's `<dispatch/dispatch.h>`; undefine it around that include. |
| `lsp.1.26.1` (ocaml-lsp-server) | `ppx_yojson_conv_lib-v0.18.patch` | its checked-in ppx output reads `Yojson_conv.record_check_extra_fields` with `!`; in v0.18 it is a `bool Basement.Dynamic.t`. |
| `ocamlformat-lib.0.29.0` | `base-v0.18-comparator.patch` | Base v0.18 made `Comparator.t` abstract; use `Comparator.sexp_of_t`. |

Not patched here but also needed (handled by the setup script):

- `bisect_ppx`: pinned to the branch of ocaml-ppx/bisect_ppx#448 (ppxlib 0.36+).
- `incremental`: its build runs `sed` over sources with non-UTF-8 bytes, which
  macOS `sed` rejects under a UTF-8 locale; install with `LC_ALL=C`.

After editing a file here, run `opam update --switch=hazel-jsoo6 hazel-jsoo6-overlay`.
