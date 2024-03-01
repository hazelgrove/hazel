open Sexplib.Std;

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(UExp.t)
    | Pat(UPat.t)
    | Typ(UTyp.t)
    | TPat(TPat.t)
    | Rul(Rul.t)
    | Nul(unit)
    | Any(unit);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(UExp.t)
    | Pat(UPat.t)
    | Typ(UTyp.t)
    | TPat(TPat.t)
    | Rul(Rul.t)
    | Nul(unit)
    | Any(unit);
}
and UExp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | StaticErrorHole(Id.t, t)
    | DynamicErrorHole(t, InvalidOperationError.t)
    | FailedCast(t, Typ.t, Typ.t)
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    | Constructor(string)
    | Fun(
        UPat.t,
        t,
        [@show.opaque] option(ClosureEnvironment.t),
        option(Var.t),
      )
    | Tuple(list(t))
    | Var(Var.t)
    | Let(UPat.t, t, t)
    | FixF(UPat.t, t, [@show.opaque] option(ClosureEnvironment.t))
    | TyAlias(TPat.t, UTyp.t, t)
    | Ap(Operators.ap_direction, t, t)
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Filter(StepperFilterKind.t, t)
    | Closure([@show.opaque] ClosureEnvironment.t, t)
    | Parens(t) // (
    | Cons(t, t)
    | ListConcat(t, t)
    | UnOp(Operators.op_un, t)
    | BinOp(Operators.op_bin, t, t)
    | BuiltinFun(string)
    | Match(t, list((UPat.t, t)))
    | Cast(t, Typ.t, Typ.t)
  and t = {
    // invariant: nonempty
    ids: list(Id.t),
    copied: bool,
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | StaticErrorHole(Id.t, t)
    | DynamicErrorHole(t, InvalidOperationError.t)
    | FailedCast(t, Typ.t, Typ.t)
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t))
    | Constructor(string)
    | Fun(
        UPat.t,
        t,
        [@show.opaque] option(ClosureEnvironment.t),
        option(Var.t),
      )
    | Tuple(list(t))
    | Var(Var.t)
    | Let(UPat.t, t, t)
    | FixF(UPat.t, t, [@show.opaque] option(ClosureEnvironment.t)) // TODO[Matt]: CHECK WITH SOMEONE THAT I GOT THE STATIC SEMANTICS RIGHT
    | TyAlias(TPat.t, UTyp.t, t)
    | Ap(Operators.ap_direction, t, t) // note: function is always first then argument; even in pipe mode
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Filter(StepperFilterKind.t, t) // TODO: Change to reflect UExp
    | Closure([@show.opaque] ClosureEnvironment.t, t)
    | Parens(t)
    | Cons(t, t)
    | ListConcat(t, t)
    | UnOp(Operators.op_un, t)
    | BinOp(Operators.op_bin, t, t)
    | BuiltinFun(string) /// Doesn't currently have a distinguishable syntax...
    | Match(t, list((UPat.t, t)))
    | Cast(t, Typ.t, Typ.t)
  and t = {
    // invariant: nonempty
    ids: list(Id.t), // > DHEXP // Multiple ids?? // Add source??
    copied: bool,
    term,
  };
}
and UPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | ListLit(list(t))
    | Constructor(string)
    | Cons(t, t)
    | Var(Var.t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | TypeAnn(t, UTyp.t)
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | ListLit(list(t))
    | Constructor(string)
    | Cons(t, t)
    | Var(Var.t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | TypeAnn(t, UTyp.t)
  and t = {
    ids: list(Id.t),
    term,
  };
}
and UTyp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Int
    | Float
    | Bool
    | String
    | List(t)
    | Var(string)
    | Constructor(string)
    | Arrow(t, t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | Sum(list(variant))
  and variant =
    | Variant(Constructor.t, list(Id.t), option(t))
    | BadEntry(t)
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Int
    | Float
    | Bool
    | String
    | List(t)
    | Var(string)
    | Constructor(string)
    | Arrow(t, t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | Sum(list(variant))
  and variant =
    | Variant(Constructor.t, list(Id.t), option(t))
    | BadEntry(t)
  and t = {
    ids: list(Id.t),
    term,
  };
}
and TPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Var(TypVar.t)
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Var(TypVar.t)
  and t = {
    ids: list(Id.t),
    term,
  };
}
and Rul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | Hole(list(Any.t))
    | Rules(UExp.t, list((UPat.t, UExp.t)))
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | Hole(list(Any.t))
    | Rules(UExp.t, list((UPat.t, UExp.t)))
  and t = {
    ids: list(Id.t),
    term,
  };
}

and Environment: {
  include
     (module type of VarBstMap.Ordered) with
      type t_('a) = VarBstMap.Ordered.t_('a);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(UExp.t);
} = {
  include VarBstMap.Ordered;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(UExp.t);
}

and ClosureEnvironment: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  let wrap: (EnvironmentId.t, Environment.t) => t;

  let id_of: t => EnvironmentId.t;
  let map_of: t => Environment.t;

  let to_list: t => list((Var.t, UExp.t));

  let of_environment: Environment.t => t;

  let id_equal: (t, t) => bool;

  let empty: t;
  let is_empty: t => bool;
  let length: t => int;

  let lookup: (t, Var.t) => option(UExp.t);
  let contains: (t, Var.t) => bool;
  let update: (Environment.t => Environment.t, t) => t;
  let update_keep_id: (Environment.t => Environment.t, t) => t;
  let extend: (t, (Var.t, UExp.t)) => t;
  let extend_keep_id: (t, (Var.t, UExp.t)) => t;
  let union: (t, t) => t;
  let union_keep_id: (t, t) => t;
  let map: (((Var.t, UExp.t)) => UExp.t, t) => t;
  let map_keep_id: (((Var.t, UExp.t)) => UExp.t, t) => t;
  let filter: (((Var.t, UExp.t)) => bool, t) => t;
  let filter_keep_id: (((Var.t, UExp.t)) => bool, t) => t;
  let fold: (((Var.t, UExp.t), 'b) => 'b, 'b, t) => 'b;

  let without_keys: (list(Var.t), t) => t;

  let placeholder: t;
} = {
  module Inner: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let wrap: (EnvironmentId.t, Environment.t) => t;

    let id_of: t => EnvironmentId.t;
    let map_of: t => Environment.t;
  } = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = (EnvironmentId.t, Environment.t);

    let wrap = (ei, map): t => (ei, map);

    let id_of = ((ei, _)) => ei;
    let map_of = ((_, map)) => map;
    let (sexp_of_t, t_of_sexp) =
      StructureShareSexp.structure_share_here(id_of, sexp_of_t, t_of_sexp);
  };
  include Inner;

  let to_list = env => env |> map_of |> Environment.to_listo;

  let of_environment = map => {
    let ei = Id.mk();
    wrap(ei, map);
  };

  /* Equals only needs to check environment id's (faster than structural equality
   * checking.) */
  let id_equal = (env1, env2) => id_of(env1) == id_of(env2);

  let empty = Environment.empty |> of_environment;

  let is_empty = env => env |> map_of |> Environment.is_empty;

  let length = env => Environment.length(map_of(env));

  let lookup = (env, x) =>
    env |> map_of |> (map => Environment.lookup(map, x));

  let contains = (env, x) =>
    env |> map_of |> (map => Environment.contains(map, x));

  let update = (f, env) => env |> map_of |> f |> of_environment;

  let update_keep_id = (f, env) => env |> map_of |> f |> wrap(env |> id_of);

  let extend = (env, xr) =>
    env |> update(map => Environment.extend(map, xr));

  let extend_keep_id = (env, xr) =>
    env |> update_keep_id(map => Environment.extend(map, xr));

  let union = (env1, env2) =>
    env2 |> update(map2 => Environment.union(env1 |> map_of, map2));

  let union_keep_id = (env1, env2) =>
    env2 |> update_keep_id(map2 => Environment.union(env1 |> map_of, map2));

  let map = (f, env) => env |> update(Environment.mapo(f));

  let map_keep_id = (f, env) => env |> update_keep_id(Environment.mapo(f));

  let filter = (f, env) => env |> update(Environment.filtero(f));

  let filter_keep_id = (f, env) =>
    env |> update_keep_id(Environment.filtero(f));

  let fold = (f, init, env) => env |> map_of |> Environment.foldo(f, init);

  let placeholder = wrap(EnvironmentId.invalid, Environment.empty);

  let without_keys = keys => update(Environment.without_keys(keys));
}
and StepperFilterKind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type filter = {
    pat: UExp.t,
    act: FilterAction.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Filter(filter)
    | Residue(int, FilterAction.t);

  let map: (UExp.t => UExp.t, t) => t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type filter = {
    pat: UExp.t,
    act: FilterAction.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Filter(filter)
    | Residue(int, FilterAction.t);

  let map = (mapper, filter) => {
    switch (filter) {
    | Filter({act, pat}) => Filter({act, pat: mapper(pat)})
    | Residue(idx, act) => Residue(idx, act)
    };
  };
};
