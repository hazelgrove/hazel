module BinBoolOp = DH.DHExp.BinBoolOp;
module BinIntOp = DH.DHExp.BinIntOp;
module BinFloatOp = DH.DHExp.BinFloatOp;

[@deriving sexp]
type t_('env) =
  DH.DHExp.t_('env) =
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(
        ErrStatus.HoleReason.t,
        MetaVar.t,
        HoleInstanceId.t,
        t_('env),
      )
    | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case_('env))
    | Closure('env, t_('env))
    | BoundVar(Var.t)
    | Let(DHPat.t, t_('env), t_('env))
    | FixF(Var.t, HTyp.t, t_('env))
    | Fun(DHPat.t, HTyp.t, t_('env))
    | Ap(t_('env), t_('env))
    | ApBuiltin(string, list(t_('env)))
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | BinBoolOp(BinBoolOp.t, t_('env), t_('env))
    | BinIntOp(BinIntOp.t, t_('env), t_('env))
    | BinFloatOp(BinFloatOp.t, t_('env), t_('env))
    | ListNil(HTyp.t)
    | Cons(t_('env), t_('env))
    | Inj(HTyp.t, InjSide.t, t_('env))
    | Pair(t_('env), t_('env))
    | Triv
    | ConsistentCase(case_('env))
    | Cast(t_('env), HTyp.t, HTyp.t)
    | FailedCast(t_('env), HTyp.t, HTyp.t)
    | InvalidOperation(t_('env), InvalidOperationError.t)
and case_('env) =
  DH.DHExp.case_('env) = | Case(t_('env), list(rule_('env)), int)
and rule_('env) = DH.DHExp.rule_('env) = | Rule(DHPat.t, t_('env));

[@deriving sexp]
type t = t_(ClosureEnvironment.t);
type case = case_(ClosureEnvironment.t);
type rule = rule_(ClosureEnvironment.t);

[@deriving sexp]
type t' = t_(EnvironmentId.t);
type case' = case_(EnvironmentId.t);
type rule' = rule_(EnvironmentId.t);

let constructor_string: t_('env) => string;

let mk_tuple: list(t_('env)) => t_('env);

let cast: (t_('env), HTyp.t, HTyp.t) => t_('env);

let apply_casts: (t_('env), list((HTyp.t, HTyp.t))) => t_('env);

let fast_equal_: (('env, 'env) => bool, t_('env), t_('env)) => bool;
let fast_equal: (t, t) => bool;
let fast_equal': (t', t') => bool;

let of_t: t => (EnvironmentIdMap.t(Environment.t'), t');
let of_t': (EnvironmentIdMap.t(Environment.t'), t') => t;
