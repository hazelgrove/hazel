module BinBoolOp = DH.DHExp.BinBoolOp;
module BinIntOp = DH.DHExp.BinIntOp;
module BinFloatOp = DH.DHExp.BinFloatOp;
module BinStringOp = DH.DHExp.BinStringOp;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  DH.DHExp.t =
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
    | Closure(ClosureEnvironment.t, t)
    | BoundVar(Var.t)
    | Sequence(t, t)
    | Let(DHPat.t, t, t)
    | FixF(Var.t, HTyp.t, t)
    | Fun(DHPat.t, HTyp.t, t)
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | TestLit(KeywordID.t)
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | StringLit(string)
    | BinBoolOp(BinBoolOp.t, t, t)
    | BinIntOp(BinIntOp.t, t, t)
    | BinFloatOp(BinFloatOp.t, t, t)
    | BinStringOp(BinStringOp.t, t, t)
    | ListLit(MetaVar.t, MetaVarInst.t, ListErrStatus.t, HTyp.t, list(t))
    | Cons(t, t)
    | Inj(HTyp.t, InjSide.t, t)
    | Tuple(list(t))
    | ConsistentCase(case)
    | Cast(t, HTyp.t, HTyp.t)
    | FailedCast(t, HTyp.t, HTyp.t)
    | InvalidOperation(t, InvalidOperationError.t)
[@deriving sexp]
and case = DH.DHExp.case = | Case(t, list(rule), int)
[@deriving sexp]
and rule = DH.DHExp.rule = | Rule(DHPat.t, t);

let constructor_string: t => string;
let mk_tuple: list(t) => t;
let cast: (t, HTyp.t, HTyp.t) => t;
let apply_casts: (t, list((HTyp.t, HTyp.t))) => t;
let strip_casts: t => t;

let fast_equal: (t, t) => bool;
