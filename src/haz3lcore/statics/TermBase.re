open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type parse_flag =
  | Whitespace // Not really an error
  | MalformedGrout // Should never happen
  | UnrecognizedTerm // Reminder to add term to MakeTerm
  | IncompleteTile; // Remove in future

let show_parse_flag: parse_flag => string =
  fun
  | Whitespace => "Whitespace"
  | MalformedGrout => "Malformed Grout"
  | UnrecognizedTerm => "Unrecognized Term"
  | IncompleteTile => "Incomplete Tile";

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(UExp.t)
    | Pat(UPat.t)
    | Typ(UTyp.t)
    | Rul(URul.t)
    | Nul(unit)
    | Any(unit);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(UExp.t)
    | Pat(UPat.t)
    | Typ(UTyp.t)
    | Rul(URul.t)
    | Nul(unit)
    | Any(unit);
}
and UExp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_int =
    | Minus;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_bool =
    | And
    | Or;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_int =
    | Plus
    | Minus
    | Times
    | Power
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_float =
    | Plus
    | Minus
    | Times
    | Power
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_string =
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un =
    | Int(op_un_int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin =
    | Int(op_bin_int)
    | Float(op_bin_float)
    | Bool(op_bin_bool)
    | String(op_bin_string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | Error
    | Closure
    | EmptyHole
    | MultiHole
    | Hole
    | Triv
    | Bool
    | Int
    | Float
    | String
    | ListLit
    | Tag
    | FixF
    | Fun
    | Tuple
    | Var
    | Let
    | Ap
    | ApBuiltin
    | If
    | Seq
    | Test
    | Parens
    | Cons
    | Prj
    | Inj
    | UnOp(op_un)
    | BinOp(op_bin)
    | Match
    | Cast;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | Error(error)
    | Closure(ClosureEnvironment.t, t)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Hole(HoleInstance.t, hole)
    | Triv
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(
        list(t),
        option((MetaVar.t, MetaVarInst.t, ListErrStatus.t, Typ.t)),
      )
    | Tag(string)
    | FixF(Token.t, Typ.t, t)
    | Fun(UPat.t, option(Typ.t), t, option(Var.t))
    | Tuple(list(t))
    | Var(Token.t)
    | Let(UPat.t, t, t)
    // Let_pat(UPat.t, t)
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | If(t, t, t)
    | Seq(t, t)
    | Test(t, option(KeywordID.t))
    | Parens(t) // (
    | Cons(t, t)
    | Prj(t, int)
    | Inj(Typ.t, InjSide.t, t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | Match(t, list((UPat.t, t)), int)
    | Cast(t, Typ.t, Typ.t)
  and error =
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(InvalidOperationError.t, t)
  and hole =
    | Empty
    | NonEmpty(ErrStatus.HoleReason.t, t)
    | InvalidText(string)
    | FreeVar(Token.t)
    | ExpandingKeyword(ExpandingKeyword.t)
    | InconsistentBranches(t, list((UPat.t, t)), int)
  and t = {
    // invariant: nonempty
    ids: list(Id.t),
    term,
  };

  let ids_derive: (~step: int=?, list(Id.t)) => list(Id.t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un_int =
    | Minus;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_bool =
    | And
    | Or;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_int =
    | Plus
    | Minus
    | Times
    | Power
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_float =
    | Plus
    | Minus
    | Times
    | Power
    | Divide
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin_string =
    | Equals;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_un =
    | Int(op_un_int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type op_bin =
    | Int(op_bin_int)
    | Float(op_bin_float)
    | Bool(op_bin_bool)
    | String(op_bin_string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | Error
    | Closure
    | EmptyHole
    | MultiHole
    | Hole
    | Triv
    | Bool
    | Int
    | Float
    | String
    | ListLit
    | Tag
    | FixF
    | Fun
    | Tuple
    | Var
    | Let
    | Ap
    | ApBuiltin
    | If
    | Seq
    | Test
    | Parens
    | Cons
    | Prj
    | Inj
    | UnOp(op_un)
    | BinOp(op_bin)
    | Match
    | Cast;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | Error(error)
    | Closure(ClosureEnvironment.t, t)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Hole(HoleInstance.t, hole)
    | Triv
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(
        list(t),
        option((MetaVar.t, MetaVarInst.t, ListErrStatus.t, Typ.t)),
      )
    | Tag(string)
    | FixF(Token.t, Typ.t, t)
    | Fun(UPat.t, option(Typ.t), t, option(Var.t))
    | Tuple(list(t))
    | Var(Token.t)
    | Let(UPat.t, t, t)
    // Let_pat(UPat.t, t)
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | If(t, t, t)
    | Seq(t, t)
    | Test(t, option(KeywordID.t))
    | Parens(t) // (
    | Cons(t, t)
    | Prj(t, int)
    | Inj(Typ.t, InjSide.t, t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | Match(t, list((UPat.t, t)), int)
    | Cast(t, Typ.t, Typ.t)
  and error =
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(InvalidOperationError.t, t)
  and hole =
    | Empty
    | NonEmpty(ErrStatus.HoleReason.t, t)
    | InvalidText(string)
    | FreeVar(Token.t)
    | ExpandingKeyword(ExpandingKeyword.t)
    | InconsistentBranches(t, list((UPat.t, t)), int)
  and t = {
    // invariant: nonempty
    ids: list(Id.t),
    term,
  };

  let ids_derive = (~step=1, ids: list(Id.t)): list(Id.t) =>
    List.map((id: Id.t) => Id.derive(id, ~step), ids);
}
and UPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Hole(HoleInstance.t, hole)
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | Triv
    | ListLit(list(t), option(Typ.t))
    | Tag(string)
    | Cons(t, t)
    | Inj(InjSide.t, t)
    | Var(Token.t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | TypeAnn(t, UTyp.t)
  and hole =
    | Empty
    | NonEmpty(ErrStatus.HoleReason.t, t)
    | ExpandingKeyword(ExpandingKeyword.t)
    | InvalidText(string)
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Hole(HoleInstance.t, hole)
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | Triv
    | ListLit(list(t), option(Typ.t))
    | Tag(string)
    | Cons(t, t)
    | Inj(InjSide.t, t)
    | Var(Token.t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | TypeAnn(t, UTyp.t)
  and hole =
    | Empty
    | NonEmpty(ErrStatus.HoleReason.t, t)
    | ExpandingKeyword(ExpandingKeyword.t)
    | InvalidText(string)
  and t = {
    ids: list(Id.t),
    term,
  };
}
and UTyp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Int
    | Float
    | Bool
    | String
    | List(t)
    | Var(string)
    | Arrow(t, t)
    | Tuple(list(t))
    | Parens(t)
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Int
    | Float
    | Bool
    | String
    | List(t)
    | Var(string)
    | Arrow(t, t)
    | Tuple(list(t))
    | Parens(t)
  and t = {
    ids: list(Id.t),
    term,
  };
}
and URul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
    | Hole(list(Any.t))
    | Rules(UExp.t, list((UPat.t, UExp.t)))
  and t = {
    ids: list(Id.t),
    term,
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(parse_flag)
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

  let of_environment:
    (Environment.t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

  let id_equal: (t, t) => bool;

  let empty: EnvironmentIdGen.t => (t, EnvironmentIdGen.t);
  let is_empty: t => bool;
  let length: t => int;

  let lookup: (t, Var.t) => option(UExp.t);
  let contains: (t, Var.t) => bool;
  let update:
    (Environment.t => Environment.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let update_keep_id: (Environment.t => Environment.t, t) => t;
  let extend:
    (t, (Var.t, UExp.t), EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);
  let extend_keep_id: (t, (Var.t, UExp.t)) => t;
  let union: (t, t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);
  let union_keep_id: (t, t) => t;
  let map:
    (((Var.t, UExp.t)) => UExp.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let map_keep_id: (((Var.t, UExp.t)) => UExp.t, t) => t;
  let filter:
    (((Var.t, UExp.t)) => bool, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let filter_keep_id: (((Var.t, UExp.t)) => bool, t) => t;
  let fold: (((Var.t, UExp.t), 'b) => 'b, 'b, t) => 'b;

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
  };
  include Inner;

  let to_list = env => env |> map_of |> Environment.to_listo;

  let of_environment = (map, eig) => {
    let (ei, eig) = EnvironmentIdGen.next(eig);
    (wrap(ei, map), eig);
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
};
