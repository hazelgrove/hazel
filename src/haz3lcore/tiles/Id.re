open Sexplib.Std;

module Id = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    base: int,
    derived: int,
  };
  let compare = (a: t, b: t): int => {
    let base_compared = Int.compare(a.base, b.base);
    if (base_compared == 0) {
      Int.compare(a.derived, b.derived);
    } else {
      base_compared;
    };
  };
  let invalid: t = {base: (-1), derived: (-1)};

  let next = (id: t): t => {base: id.base + 1, derived: 0};

  let init_base = (base: int): t => {base, derived: 0};

  let init = {base: 0, derived: 0};

  let string_of_t = (id: t): string =>
    Printf.sprintf("(%d, %d)", id.base, id.derived);
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
