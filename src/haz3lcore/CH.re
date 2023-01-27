open Util;
open Sexplib.Std;

// Yeah, a group of rec'd modules that represent the core data type we
// use for expression.

module Ids = {
  module Tree = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | Root(Id.t)
      | Leaf(t, Id.t);

    let rec root: t => Id.t =
      tree =>
        switch (tree) {
        | Root(id) => id
        | Leaf(parent, _) => root(parent)
        };

    let append: (t, Id.t) => t = (tree, id) => Leaf(tree, id);

    let rec token_of_t: t => string =
      tree =>
        switch (tree) {
        | Root(id) => "root_" ++ Id.token_of_t(id)
        | Leaf(parent, id) => token_of_t(parent) ++ "_" ++ Id.token_of_t(id)
        };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Tree.t);

  let rep_id: t => Id.t = ids => Tree.root(List.nth(ids, 0));

  let rep_ids: t => list(Id.t) = ids => ids |> List.map(Tree.root);

  let mk: list(Id.t) => t = List.map(id => Tree.Root(id));

  let derive: t => IdGen.t(t) =
    ids => {
      open IdGen.Syntax;
      let f = tree => {
        let+ derived = IdGen.fresh;
        Tree.append(tree, derived);
      };
      ListUtil.traverse(f, ids);
    };
};

module rec CAny: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(CExp.t)
    | Pat(CPat.t)
    | Typ(CTyp.t)
    | Rul(CRul.t)
    | Nul(unit)
    | Any(unit);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(CExp.t)
    | Pat(CPat.t)
    | Typ(CTyp.t)
    | Rul(CRul.t)
    | Nul(unit)
    | Any(unit);
}
and CExp: {
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
  type list_status = (MetaVar.t, MetaVarInst.t, ListErrStatus.t, Typ.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(option(HoleInstance.t), hole)
    | Closure(ClosureEnvironment.t, t)
    | Triv
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t), option(list_status))
    | Tag(string)
    | FixF(Token.t, Typ.t, t)
    | Fun(CPat.t, option(Typ.t), t, option(Var.t))
    | Tuple(list(t))
    | Var(Token.t)
    | Let(CPat.t, t, t)
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | If(t, t, t)
    | Seq(t, t)
    | Test(t, option(KeywordID.t))
    | Parens(t)
    | Cons(t, t)
    | Prj(t, int)
    | Inj(Typ.t, InjSide.t, t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | Match(t, list((CPat.t, t)), int)
    | Cast(t, Typ.t, Typ.t)
  and hole =
    | EmptyHole
    | MultiHole(list(CAny.t))
    | NonEmptyHole(ErrStatus.HoleReason.t, t)
    | Invalid(ParseFlag.t)
    | InvalidText(string)
    | InvalidOperation(InvalidOperationError.t, t)
    | ExpandingKeyword(ExpandingKeyword.t)
    | FreeVar(Token.t)
    | InconsistentBranches(t, list((CPat.t, t)), int)
    | FailedCast(t, Typ.t, Typ.t)
  and t = {
    // invariant: nonempty
    ids: Ids.t,
    term,
  };

  let mk: (Ids.t, term) => t;
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
  type list_status = (MetaVar.t, MetaVarInst.t, ListErrStatus.t, Typ.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(option(HoleInstance.t), hole)
    | Closure(ClosureEnvironment.t, t)
    | Triv
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t), option(list_status))
    | Tag(string)
    | FixF(Token.t, Typ.t, t)
    | Fun(CPat.t, option(Typ.t), t, option(Var.t))
    | Tuple(list(t))
    | Var(Token.t)
    | Let(CPat.t, t, t)
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | If(t, t, t)
    | Seq(t, t)
    | Test(t, option(KeywordID.t))
    | Parens(t)
    | Cons(t, t)
    | Prj(t, int)
    | Inj(Typ.t, InjSide.t, t)
    | UnOp(op_un, t)
    | BinOp(op_bin, t, t)
    | Match(t, list((CPat.t, t)), int)
    | Cast(t, Typ.t, Typ.t)
  and hole =
    | EmptyHole
    | MultiHole(list(CAny.t))
    | NonEmptyHole(ErrStatus.HoleReason.t, t)
    | Invalid(ParseFlag.t)
    | InvalidText(string)
    | InvalidOperation(InvalidOperationError.t, t)
    | ExpandingKeyword(ExpandingKeyword.t)
    | FreeVar(Token.t)
    | InconsistentBranches(t, list((CPat.t, t)), int)
    | FailedCast(t, Typ.t, Typ.t)
  and t = {
    // invariant: nonempty
    ids: Ids.t,
    term,
  };

  let mk = (ids, term) => {ids, term};
}
and CPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(option(HoleInstance.t), hole)
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
    | TypeAnn(t, CTyp.t)
  and hole =
    | EmptyHole
    | MultiHole(list(CAny.t))
    | NonEmptyHole(ErrStatus.HoleReason.t, t)
    | Invalid(ParseFlag.t)
    | InvalidText(string)
    | ExpandingKeyword(ExpandingKeyword.t)
  and t = {
    ids: Ids.t,
    term,
  };

  let mk: (Ids.t, term) => t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(option(HoleInstance.t), hole)
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
    | TypeAnn(t, CTyp.t)
  and hole =
    | EmptyHole
    | MultiHole(list(CAny.t))
    | NonEmptyHole(ErrStatus.HoleReason.t, t)
    | Invalid(ParseFlag.t)
    | InvalidText(string)
    | ExpandingKeyword(ExpandingKeyword.t)
  and t = {
    ids: Ids.t,
    term,
  };

  let mk = (ids, term) => {ids, term};
}
and CTyp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(ParseFlag.t)
    | EmptyHole
    | MultiHole(list(CAny.t))
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
    ids: Ids.t,
    term,
  };

  let mk: (Ids.t, term) => t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(ParseFlag.t)
    | EmptyHole
    | MultiHole(list(CAny.t))
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
    ids: Ids.t,
    term,
  };

  let mk = (ids, term) => {ids, term};
}
and CRul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(ParseFlag.t)
    | Hole(list(CAny.t))
    | Rules(CExp.t, list((CPat.t, CExp.t)))
  and t = {
    ids: Ids.t,
    term,
  };

  let mk: (Ids.t, term) => t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(ParseFlag.t)
    | Hole(list(CAny.t))
    | Rules(CExp.t, list((CPat.t, CExp.t)))
  and t = {
    ids: Ids.t,
    term,
  };

  let mk = (ids, term) => {ids, term};
}
and Environment: {
  include
     (module type of VarBstMap.Ordered) with
      type t_('a) = VarBstMap.Ordered.t_('a);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(CExp.t);
} = {
  include VarBstMap.Ordered;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(CExp.t);
}
and ClosureEnvironment: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  let wrap: (EnvironmentId.t, Environment.t) => t;

  let id_of: t => EnvironmentId.t;
  let map_of: t => Environment.t;

  let to_list: t => list((Var.t, CExp.t));

  let of_environment:
    (Environment.t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

  let id_equal: (t, t) => bool;

  let empty: EnvironmentIdGen.t => (t, EnvironmentIdGen.t);
  let is_empty: t => bool;
  let length: t => int;

  let lookup: (t, Var.t) => option(CExp.t);
  let contains: (t, Var.t) => bool;
  let update:
    (Environment.t => Environment.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let update_keep_id: (Environment.t => Environment.t, t) => t;
  let extend:
    (t, (Var.t, CExp.t), EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);
  let extend_keep_id: (t, (Var.t, CExp.t)) => t;
  let union: (t, t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);
  let union_keep_id: (t, t) => t;
  let map:
    (((Var.t, CExp.t)) => CExp.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let map_keep_id: (((Var.t, CExp.t)) => CExp.t, t) => t;
  let filter:
    (((Var.t, CExp.t)) => bool, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let filter_keep_id: (((Var.t, CExp.t)) => bool, t) => t;
  let fold: (((Var.t, CExp.t), 'b) => 'b, 'b, t) => 'b;

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
