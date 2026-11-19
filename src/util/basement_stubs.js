// JavaScript implementations of primitives that Jane Street's
// basement.v0.18~preview declares (with C stubs) but whose js_of_ocaml
// runtime.js does not provide. Without them the generated JS calls dummy
// primitives that throw, e.g. "runtime.basement_domain_self_index is not a
// function" at module initialisation. See #1131 and opam-jsoo6/README.md.
// Stdlib_shim.Domain.self_index has a C stub but no JS one; there is one domain.
//Provides: basement_domain_self_index
function basement_domain_self_index(unit) {
  return 0;
}

// Subatomic.Loc's C stubs have no JS counterparts. A loc is the pair
// (block, field index); field i of a block lives at index i + 1.
//Provides: caml_atomic_set_stub_loc
function caml_atomic_set_stub_loc(loc, v) {
  loc[1][loc[2] + 1] = v;
  return 0;
}

//Provides: caml_atomic_compare_exchange_stub_loc
function caml_atomic_compare_exchange_stub_loc(loc, o, n) {
  var r = loc[1], i = loc[2] + 1;
  var old = r[i];
  if (old === o) {
    r[i] = n;
  }
  return old;
}

//Provides: caml_atomic_add_stub_loc
function caml_atomic_add_stub_loc(loc, v) {
  loc[1][loc[2] + 1] += v;
  return 0;
}

//Provides: caml_atomic_sub_stub_loc
function caml_atomic_sub_stub_loc(loc, v) {
  loc[1][loc[2] + 1] -= v;
  return 0;
}

//Provides: caml_atomic_land_stub_loc
function caml_atomic_land_stub_loc(loc, v) {
  loc[1][loc[2] + 1] &= v;
  return 0;
}

//Provides: caml_atomic_lor_stub_loc
function caml_atomic_lor_stub_loc(loc, v) {
  loc[1][loc[2] + 1] |= v;
  return 0;
}

//Provides: caml_atomic_lxor_stub_loc
function caml_atomic_lxor_stub_loc(loc, v) {
  loc[1][loc[2] + 1] ^= v;
  return 0;
}
