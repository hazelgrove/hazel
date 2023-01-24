open Sexplib.Std;

module Id = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type ns =
    | Dark
    | Default
    | Elaborator
    | Evaluator
    | BuiltinElab
    | BuiltinEval
    | NextGeneration;

  let int_of_ns = (ns: ns) =>
    switch (ns) {
    | Dark => (-1)
    | Default => 0
    | Elaborator => 1
    | Evaluator => 2
    | BuiltinElab => 3
    | BuiltinEval => 4
    | NextGeneration => 10_000_000
    };

  let token_of_ns = ns =>
    switch (ns) {
    | Dark => "dark"
    | Default => "default"
    | Elaborator => "elaborator"
    | Evaluator => "evaluator"
    | BuiltinElab => "builtin_elab"
    | BuiltinEval => "builtin_eval"
    | NextGeneration => "next_generation"
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    ns,
    id: int,
  };

  let compare: (t, t) => int =
    (id1, id2) => {
      let ns1 = int_of_ns(id1.ns);
      let ns2 = int_of_ns(id2.ns);
      let id1 = id1.id;
      let id2 = id2.id;
      let ns_cmp = Int.compare(ns1, ns2);
      if (ns_cmp == 0) {
        Int.compare(id1, id2);
      } else {
        ns_cmp;
      };
    };

  let mk: (ns, int) => t = (ns, id) => {ns, id};

  let map: (int => int, t) => t =
    (f, {ns, id}) => {
      {ns, id: f(id)};
    };

  let next: t => t = map(id => id + 1);

  let init: ns => t =
    ns => {
      {ns, id: 0};
    };

  let is_bright: t => bool = t => int_of_ns(t.ns) >= 0;

  let string_of_t: t => string = id => Sexplib.Sexp.to_string(sexp_of_t(id));

  let token_of_t: t => string =
    ({ns, id}) => Printf.sprintf("%s_%d", token_of_ns(ns), id);
};

include Id;

module Map = {
  include Map.Make(Id);

  let disj_union = (m: t('a), m': t('a)): t('a) =>
    union(
      (_, _, _) =>
        raise(
          Invalid_argument(
            "IntMap.disj_union expects input maps to have disjoint key sets",
          ),
        ),
      m,
      m',
    );
};

module Uf: {
  type store('a);
  let init: unit => store(_);
  let add: (t, 'a, store('a)) => unit;
  let get: (t, store('a)) => 'a;
  let get_opt: (t, store('a)) => option('a);
  let set: (t, 'a, store('a)) => unit;
  let merge: (('a, 'a) => 'a, t, t, store('a)) => unit;
} = {
  module M = UnionFind.Make(UnionFind.StoreVector);
  type store('a) = {
    refs: ref(Map.t(M.rref('a))),
    store: M.store('a),
  };
  let init = () => {refs: ref(Map.empty), store: M.new_store()};
  let rref = (id, s) => Map.find(id, s.refs^);
  let add = (id, a, s) =>
    switch (Map.find_opt(id, s.refs^)) {
    | None =>
      let r = M.make(s.store, a);
      s.refs := Map.add(id, r, s.refs^);
    | Some(_) => ()
    };
  let get = (id, s) => M.get(s.store, M.find(s.store, rref(id, s)));
  let get_opt = (id, s) =>
    Map.find_opt(id, s.refs^) |> Option.map(_ => get(id, s));
  let set = (id, a, s) => M.set(s.store, M.find(s.store, rref(id, s)), a);

  let merge = (f, id, id', s) =>
    ignore(M.merge(s.store, f, rref(id, s), rref(id', s)));
};
