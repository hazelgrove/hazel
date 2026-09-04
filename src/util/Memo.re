/* SPIKE (wasm-eval-bench): a stand-in for Core.Memo.general, so that
   [language] does not depend on Jane Street Core. Core drags in a C-stub
   surface that wasm_of_ocaml cannot satisfy, and this was its only use in
   the whole library (Statics.mk).

   Semantics differ from Core's in one way worth knowing: Core evicts
   least-recently-used entries at the bound, this clears the whole table.
   For a statics cache that means an occasional cliff rather than gradual
   eviction. Keys are compared with polymorphic equality, as Core's default
   does. */

let general = (~cache_size_bound: int, f: 'a => 'b): ('a => 'b) => {
  let tbl: Hashtbl.t('a, 'b) = Hashtbl.create(16);
  x =>
    switch (Hashtbl.find_opt(tbl, x)) {
    | Some(v) => v
    | None =>
      let v = f(x);
      if (Hashtbl.length(tbl) >= cache_size_bound) {
        Hashtbl.reset(tbl);
      };
      Hashtbl.add(tbl, x, v);
      v;
    };
};
