open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type binding('a) = (Var.t, 'a);

type t('a) =
  | Empty
  | E({
      id: Id.t,
      binding: binding('a),
      prev_env: t('a),
      cached_search_tree:
        Core.Map.t(Var.t, 'a, Core.String.comparator_witness),
    });

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type serialized_t'('a, 'b) =
  | EmptyS
  | ES({
      id: Id.t,
      binding: (Var.t, 'a),
      prev_env: 'b,
    });

let extend = (type a, ~id=Id.mk(), env: t(a), (v: Var.t, x: a)): t(a) => {
  E({
    id,
    binding: (v, x),
    prev_env: env,
    cached_search_tree:
      Core.Map.update(
        switch (env) {
        | Empty => Core.Map.empty((module Core.String))
        | E(e) => e.cached_search_tree
        },
        v,
        ~f=_ =>
        x
      ),
  });
};

let sexp_of_serialized_t' = (f_a, f_b, x) =>
  Util.StructureShareSexp.structure_share_sexp_of_t(
    (x: serialized_t'('a, 'b)) =>
      switch (x) {
      | EmptyS => Id.invalid
      | ES(e) => e.id
      },
    sexp_of_serialized_t'(f_a, f_b),
    x,
  );

let serialized_t'_of_sexp = (f_a, f_b) =>
  Util.StructureShareSexp.structure_share_t_of_sexp(
    serialized_t'_of_sexp(f_a, f_b),
  );

[@deriving (show({with_path: false}), sexp, yojson)]
type serialized_t('a) =
  | A(serialized_t'('a, serialized_t('a)));

let rec serialized_of_t = (env: t('a)): serialized_t('a) => {
  switch (env) {
  | Empty => A(EmptyS)
  | E(e) =>
    A(
      ES({
        id: e.id,
        binding: e.binding,
        prev_env: serialized_of_t(e.prev_env),
      }),
    )
  };
};

let rec t_of_serialized = (serialized: serialized_t('a)): t('a) => {
  switch (serialized) {
  | A(EmptyS) => Empty
  | A(ES(e)) =>
    let (v, x) = e.binding;
    extend(~id=e.id, t_of_serialized(e.prev_env), (v, x));
  };
};

let pp = (a: (Format.formatter, 'a) => unit, b: Format.formatter, t: t('a)) =>
  t |> serialized_of_t |> pp_serialized_t(a, b);
let sexp_of_t = (a: 'a => Sexplib.Sexp.t, t: t('a)) =>
  t |> serialized_of_t |> sexp_of_serialized_t(a);
let t_of_sexp = (a: Sexplib.Sexp.t => 'a, sexp: Sexplib.Sexp.t) =>
  sexp |> serialized_t_of_sexp(a) |> t_of_serialized;
let yojson_of_t = (a: 'a => Yojson.Safe.t, t: t('a)) =>
  t |> serialized_of_t |> yojson_of_serialized_t(a);
let t_of_yojson = (a: Yojson.Safe.t => 'a, yojson: Yojson.Safe.t) =>
  yojson |> serialized_t_of_yojson(a) |> t_of_serialized;

let id_equal = (x: t('a), y: t('a)) =>
  switch (x, y) {
  | (Empty, Empty) => true
  | (E(e1), E(e2)) => e1.id == e2.id
  | (Empty, E(_))
  | (E(_), Empty) => false
  };

let equal = _ => id_equal;

let empty = Empty;

let add_bindings = (type a, env: t(a), bindings: list(binding(a))): t(a) =>
  List.fold_left(extend, env, bindings);

let of_bindings = (type a, bindings: list(binding(a))): t(a) =>
  add_bindings(empty, bindings);

let rec map = (f: 'a => 'b, env: t('a)): t('b) =>
  switch (env) {
  | Empty => Empty
  | E({id, binding: (name, value), prev_env, cached_search_tree: _}) =>
    extend(~id, map(f, prev_env), (name, f(value)))
  };

let rec filter = (p: (Var.t, 'a) => bool, env: t('a)): t('a) =>
  switch (env) {
  | Empty => Empty
  | E({id, binding: (name, value), prev_env, cached_search_tree: _}) =>
    let new_prev = filter(p, prev_env);
    p(name, value) ? extend(~id, new_prev, (name, value)) : new_prev;
  };

let rec fold = (f: ((Var.t, 'a), 'b) => 'b, init: 'b, env: t('a)): 'b =>
  switch (env) {
  | Empty => init
  | E({binding, prev_env, _}) => fold(f, f(binding, init), prev_env)
  };

let lookup = (env: t('a), v: Var.t): option('a) => {
  switch (env) {
  | Empty => None
  | E({cached_search_tree, _}) => Core.Map.find(cached_search_tree, v)
  };
};

let without_keys = (type a, keys: list(Var.t), env: t(a)): t(a) =>
  filter((name, _) => !List.mem(name, keys), env);

let to_bindings = (type a, env: t(a)): list(binding(a)) =>
  fold((binding, acc) => [binding, ...acc], [], env);
