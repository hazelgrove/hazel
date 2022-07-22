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
     equal (see Result.fast_equals).
     */
  let fast_equals: (t, t) => bool;
}

and Environment: {
  include  (module type of VarBstMap) with type t_('a) = VarBstMap.t_('a);

  [@deriving sexp]
  type t = t_(DHExp.t);
}

/**
  ClosureEnvironment is an environment (mapping variables to expressions) that
  are used during evaluation. It is different from Environment.t in two ways:

  1. It maps Var.t to Evaluator.result (rather than to DHExp.t) 2. Each
  ClosureEnvironment has an ID associated with it (if evaluation reaches it),
  or is unreachable. (i.e., a placeholder environment)

  Environment.t may be useful in certain cases, namely pattern matching, when
  an evaluated result is not needed. ClosureEnvironment is used for environents
  during evaluation, including in closures. ClosureEnvironments are numbered so
  that operations on them (e.g., during hole numbering) can be memoized; the id
  allows for quick equality checking and allows environments to be comparable
  (e.g., so that they can be stored in a map).

  Both ClosureEnvironment.t and Environment.t are often named sigma (usually
  for hole environments) or env.

  This mimicks the VarMap interface on the extended ClosureEnvironment.t type.
  Most operations require an EvaluatorState.t parameter, which is used to
  generate unique ID's for each environment, and is created using
  ClosureEnvironment.empty (at the beginning of evaluation).
 */
and ClosureEnvironment: {
  [@deriving sexp]
  type t = (EnvironmentId.t, Environment.t);

  /**
  [id_of env] is the id of [env].
 */
  let id_of: t => EnvironmentId.t;

  /**
  [map_of env] is the {!type:map} of [env].
 */
  let map_of: t => Environment.t;

  /**
  [to_list env] is the list of bindings in [env].
 */
  let to_list: t => list((Var.t, DHExp.t));

  /**
  [equal env1 env2] is [true] if and only if [env1] and [env2] have the same
  id.
 */
  let equal: (t, t) => bool;

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
  [lookup env x] is [Some d] if [d] is bound for [x] in [env] and [None] otherwise.
 */
  let lookup: (t, Var.t) => option(DHExp.t);

  /**
  [contains env x] is [true] if [x] is bound in [env].
 */
  let contains: (t, Var.t) => bool;

  /**
  [extend env (x, d) eig] is [(env', eig')], where [env'] is [env] extended
  with the binding of [d] for [x].
 */
  let extend:
    (t, (Var.t, DHExp.t), EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

  /**
  [union env1 env2 eig] is [(env, eig)] where [env] is [env2] extended with
  [env1]. See {!val:VarBstMap.union}.
 */
  let union: (t, t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

  /**
  [map f env eig] is [(env', eig')] where [env'] contains the bindings of
  [env] mapped by [f].
 */
  let map:
    (((Var.t, DHExp.t)) => DHExp.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);

  /**
  [map_keep_id] is [map], but the id of the given environment is maintained.

  (This is used when transforming an environment, such as in the closure ->
  lambda stage after evaluation. More functions may be added like this
  as-needed for similar purposes.)
 */
  let map_keep_id: (((Var.t, DHExp.t)) => DHExp.t, t) => t;

  /**
  [filter f env eig] is [(env', eig')] where [env'] contains the bindings of
  [env] filtered by [f].
 */
  let filter:
    (((Var.t, DHExp.t)) => bool, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);

  /**
  Placeholder used in DHCode. Is identified by an invalid EnvironmentId.t, only
  used for display purposes.
 */
  let placeholder: t;
};
