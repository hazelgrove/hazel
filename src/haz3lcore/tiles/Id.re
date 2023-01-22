open Sexplib.Std;

module Id = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
  let compare = Int.compare;

  let invalid: t = (-1);

  let next = (id: t): t => id + 1;

  let init = 0;

  let string_of_t = string_of_int;
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
