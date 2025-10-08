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
type serialized_t('a) =
  | EmptyS
  | ES({
      id: Id.t,
      binding: (Var.t, 'a),
      prev_env: serialized_t('a),
    });

let extend = (type a, ~id=Id.mk(), env: t(a), v: Var.t, x: a): t(a) => {
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

let rec serialized_of_t = (env: t('a)): serialized_t('a) => {
  switch (env) {
  | Empty => EmptyS
  | E(e) =>
    ES({
      id: e.id,
      binding: e.binding,
      prev_env: serialized_of_t(e.prev_env),
    })
  };
};

let rec t_of_serialized = (serialized: serialized_t('a)): t('a) => {
  switch (serialized) {
  | EmptyS => Empty
  | ES(e) =>
    let (v, x) = e.binding;
    extend(~id=e.id, t_of_serialized(e.prev_env), v, x);
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

let add_bindings = (type a, env: t(a), bindings: list(binding(a))): t(a) => {
  List.fold_left(
    (env, (name, value)) => extend(env, name, value),
    env,
    bindings,
  );
};

let of_list = (bindings: list(binding('a))): t('a) =>
  add_bindings(empty, bindings);

let rec map = (f: 'a => 'b, env: t('a)): t('b) => {
  switch (env) {
  | Empty => Empty
  | E({id, binding: (name, value), prev_env, cached_search_tree: _}) =>
    map(f, prev_env) |> extend(~id, _, name, f(value)) /* */
  };
};

let rec filter = (p: (Var.t, 'a) => bool, env: t('a)): t('a) => {
  switch (env) {
  | Empty => Empty
  | E({id, binding: (name, value), prev_env, cached_search_tree: _}) =>
    let new_prev = filter(p, prev_env);
    if (p(name, value)) {
      new_prev |> extend(~id, _, name, value); /* */
    } else {
      new_prev;
    };
  };
};

let rec fold = (f: ((Var.t, 'a), 'b) => 'b, init: 'b, env: t('a)): 'b => {
  switch (env) {
  | Empty => init
  | E({binding: (name, value), prev_env, _}) =>
    fold(f, f((name, value), init), prev_env)
  };
};

let lookup = (env: t('a), v: Var.t): option('a) => {
  switch (env) {
  | Empty => None
  | E({id: _, binding: _, prev_env: _, cached_search_tree}) =>
    Core.Map.find(cached_search_tree, v)
  };
};

let without_keys = (type a, keys: list(Var.t), env: t(a)): t(a) => {
  filter((name, _) => !List.mem(name, keys), env);
};

let of_bindings = (type a, bindings: list(binding(a))): t(a) => {
  List.fold_left(
    (env, (name, value)) => extend(env, name, value),
    empty,
    bindings,
  );
};

// and Environment: {
//   include
//      (module type of VarBstMap.Ordered) with
//       type t_('a) = VarBstMap.Ordered.t_('a);
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t = environment_t;
//   let pp: (Format.formatter, t) => unit;
// } = {
//   include VarBstMap.Ordered;
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t = environment_t;

//   [@deriving show({with_path: false})]
//   type entries = list((Var.t, Exp.t));

//   let pp = (f, map: t) => pp_entries(f, VarBstMap.Ordered.to_listo(map));
// }

// and ClosureEnvironment: {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t = closure_environment_t;

//   let empty: t;

//   let of_environment: Environment.t => t;

//   let map_of: t => Environment.t;
//   let call_stack_of: t => Probe.call_stack;

//   let id_equal: (closure_environment_t, closure_environment_t) => bool;

//   let lookup: (t, Var.t) => option(Exp.t);
//   let update_env: (Environment.t => Environment.t, t) => t;
//   let extend_eval:
//     (~ap_id: Id.t=?, ~call_stack: Probe.call_stack, Environment.t, t) => t;

//   let to_list: t => list((Var.t, Exp.t));
// } = {
//   module Inner: {
//     [@deriving (show({with_path: false}), sexp, yojson)]
//     type t = closure_environment_t;

//     let wrap: (Id.t, Environment.t, Probe.call_stack) => t;

//     let id_of: t => Id.t;
//     let map_of: t => Environment.t;
//     let call_stack_of: t => Probe.call_stack;
//   } = {
//     [@deriving (show({with_path: false}), sexp, yojson)]
//     type t = closure_environment_t;

//     let wrap = (id, env, call_stack): t => {
//       id,
//       env,
//       call_stack,
//     };

//     let id_of = (t: t) => t.id;
//     let map_of = (t: t) => t.env;
//     let call_stack_of = (t: t) => t.call_stack;

//     let (sexp_of_t, t_of_sexp) =
//       Util.StructureShareSexp.structure_share_here(
//         id_of,
//         sexp_of_t,
//         t_of_sexp,
//       );
//   };
//   include Inner;

//   let to_list = env => env |> map_of |> Environment.to_listo;

//   let of_environment = env => wrap(Id.mk(), env, []);

//   /* Equals only needs to check environment id's (faster than structural equality
//    * checking.) */
//   let id_equal = (env1, env2) => id_of(env1) == id_of(env2);

//   let empty = Environment.empty |> of_environment;

//   let lookup = (env, x) =>
//     env |> map_of |> (map => Environment.lookup(map, x));

//   let update_env = (f, env) => env |> map_of |> f |> of_environment;

//   /* Extend the environment with new bindings. ~ap_id is an optional argument which
//    * will add an entry in a stack of function application syntax ids, used to
//    * represent and track the call stack for use by live value probes. */
//   let extend_eval =
//       (
//         ~ap_id: option(Id.t)=?,
//         ~call_stack: Probe.call_stack,
//         new_bindings: Environment.t,
//         env_to_extend: t,
//       )
//       : t => {
//     {
//       id: Id.mk(),
//       env: Environment.union(new_bindings, map_of(env_to_extend)),
//       call_stack: Option.to_list(ap_id) @ call_stack,
//     };
//   };
