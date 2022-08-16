module rec DHExp: {
  module BinBoolOp: {
    [@deriving sexp]
    type t =
      | And
      | Or;

    let of_op: UHExp.operator => option(t);

    let to_op: t => UHExp.operator;
  };

  module BinIntOp: {
    [@deriving sexp]
    type t =
      | Minus
      | Plus
      | Times
      | Divide
      | LessThan
      | GreaterThan
      | Equals;

    let of_op: UHExp.operator => option((t, HTyp.t));

    let to_op: t => UHExp.operator;
  };

  module BinFloatOp: {
    [@deriving sexp]
    type t =
      | FPlus
      | FMinus
      | FTimes
      | FDivide
      | FLessThan
      | FGreaterThan
      | FEquals;

    let of_op: UHExp.operator => option((t, HTyp.t));

    let to_op: t => UHExp.operator;
  };

  [@deriving sexp]
  type t =
    /* Hole types */
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
    /* Generalized closures */
    | Closure(ClosureEnvironment.t, t)
    /* Other expressions forms */
    | BoundVar(Var.t)
    | Let(DHPat.t, t, t)
    | FixF(Var.t, HTyp.t, t)
    | Fun(DHPat.t, HTyp.t, t)
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | BinBoolOp(BinBoolOp.t, t, t)
    | BinIntOp(BinIntOp.t, t, t)
    | BinFloatOp(BinFloatOp.t, t, t)
    | ListNil(HTyp.t)
    | Cons(t, t)
    | Inj(HTyp.t, InjSide.t, t)
    | Pair(t, t)
    | Triv
    | ConsistentCase(case)
    | Cast(t, HTyp.t, HTyp.t)
    | FailedCast(t, HTyp.t, HTyp.t)
    | InvalidOperation(t, InvalidOperationError.t)
  and case =
    | Case(t, list(rule), int)
  and rule =
    | Rule(DHPat.t, t);

  let constructor_string: t => string;

  let mk_tuple: list(t) => t;

  let cast: (t, HTyp.t, HTyp.t) => t;

  let apply_casts: (t, list((HTyp.t, HTyp.t))) => t;

  /* Used for faster structural equality checking. Structural
     checking may be slow when an expression is large,
     in particular when environments are repeated many times.
     We can optimize checking for structural equality of
     environments simply by checking equality of environment ID's.

     Note: assumes that environments with the same EnvironmentId.t
     within both expressions are equivalent. This assumption
     is true if comparing within a program evaluation (since
     EnvironmentId.t numbers don't get reused within a single program
     evaluation) or if all the environments are checked to be
     equal (see Result.fast_equal).
     */
  let fast_equal: (t, t) => bool;
}

/**
  [Environment] is a map from variables to {!DHExp.t} backed by {!VarBstMap.Ordered}.
 */
and Environment: {
  include
     (module type of VarBstMap.Ordered) with
      type t_('a) = VarBstMap.Ordered.t_('a);

  [@deriving sexp]
  type t = t_(DHExp.t);
}

/**
  [ClosureEnvironment] is an {!module:Environment} associated with an id, used
  for the environments of {!DHExp.Closure}s. {b Importantly, the bindings in
  the environment are ordered (see {!VarBstMap.Ordered}), and any fixpoint
  bindings must come after their dependencies (see also high-level dynamics
  documentation).}

  ClosureEnvironments are numbered so that operations on them (e.g., during
  hole numbering) can be memoized; the id allows for quick equality checking
  and allows environments to be comparable (e.g., so that they can be stored in
  a map).

  Both [ClosureEnvironment.t] and {!type:Environment.t} are often named sigma
  (for hole environments) or env.

  This module mimicks the {!VarBstMap} interface, but most operations require
  an extra {!EnvironmentIdGen.t} parameter, which is used to generate unique
  id's for each environment.
 */
and ClosureEnvironment: {
  [@deriving sexp]
  type t;

  /**
    [wrap ei map] is the environment with id [ei] and map [map].
   */
  let wrap: (EnvironmentId.t, Environment.t) => t;

  /**
    [id_of env] is the id of [env].
   */
  let id_of: t => EnvironmentId.t;

  /**
    [map_of env] is the {!type:map} of [env].
   */
  let map_of: t => Environment.t;

  /**
    [to_list env] is the list of bindings in [env], in insertion order.
   */
  let to_list: t => list((Var.t, DHExp.t));

  /**
    [of_environment map eig] is [(env, eig')] where [env] is the environment
    with a new id and map [map].
   */
  let of_environment:
    (Environment.t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

  /**
    [id_equal env1 env2] is [true] if and only if [env1] and [env2] have the same
    id.
   */
  let id_equal: (t, t) => bool;

  /**
    [empty eig] is [(env, eig')] where [env] is an empty environment.
   */
  let empty: EnvironmentIdGen.t => (t, EnvironmentIdGen.t);

  /**
    [is_empty env] is [true] if and only if [env] has no bindings.
   */
  let is_empty: t => bool;

  /**
    [length env] is the number of bindings in [env].
   */
  let length: t => int;

  /**
    [lookup env x] is [Some d] if [d] is bound for [x] in [env] and [None]
    otherwise.
   */
  let lookup: (t, Var.t) => option(DHExp.t);

  /**
    [contains env x] is [true] if [x] is bound in [env].
   */
  let contains: (t, Var.t) => bool;

  /**
    [update f env] is [(env', eig)], where [env] is the environment with a new
    id and map mapped by [f].
   */
  let update:
    (Environment.t => Environment.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);

  /**
    [update_keep_id] is [update], but the id of the given environment is maintained.
   */
  let update_keep_id: (Environment.t => Environment.t, t) => t;

  /**
    [extend env (x, d) eig] is [(env', eig')], where [env'] is [env] extended
    with the binding of [d] for [x].
   */
  let extend:
    (t, (Var.t, DHExp.t), EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

  /**
    [extend_keep_id] is [extend], but the id of the given environment is maintained.
   */
  let extend_keep_id: (t, (Var.t, DHExp.t)) => t;

  /**
    [union env1 env2 eig] is [(env, eig)] where [env] is [env2] extended with
    [env1]. See {!val:VarBstMap.union}.
   */
  let union: (t, t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

  /**
    [union_keep_id] is [union], but the id of the given environment is
    maintained.
   */
  let union_keep_id: (t, t) => t;

  /**
    [map f env eig] is [(env', eig')] where [env'] contains the bindings of
    [env] mapped by [f] in insertion order.
   */
  let map:
    (((Var.t, DHExp.t)) => DHExp.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);

  /**
    [map_keep_id] is [map], but the id of the given environment is maintained.
   */
  let map_keep_id: (((Var.t, DHExp.t)) => DHExp.t, t) => t;

  /**
    [filter f env eig] is [(env', eig')] where [env'] contains the bindings of
    [env] filtered by [f] in insertion order.
   */
  let filter:
    (((Var.t, DHExp.t)) => bool, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);

  /**
    [filter_keep_id] is like [map_keep_id], but for [filter].
   */
  let filter_keep_id: (((Var.t, DHExp.t)) => bool, t) => t;

  /**
    [fold f init env] is [env |> map_of |> Environment.foldo f init].
   */
  let fold: (((Var.t, DHExp.t), 'b) => 'b, 'b, t) => 'b;

  /**
    Placeholder used in DHCode. Is identified by an invalid EnvironmentId.t, only
    used for display purposes.
   */
  let placeholder: t;
};
