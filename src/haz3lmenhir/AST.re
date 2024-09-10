open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, qcheck)]
type t = [@gen QCheck.Gen.return("hello")] string;

[@deriving (show({with_path: false}), sexp, qcheck)]
type filter_action =
  | Pause
  | Debug
  | Hide
  | Eval;

[@deriving (show({with_path: false}), sexp, qcheck)]
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
  | Equals
  | NotEquals;

[@deriving (show({with_path: false}), sexp, qcheck)]
type op_bin_bool =
  | And
  | Or;

[@deriving (show({with_path: false}), sexp, qcheck)]
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
  | Equals
  | NotEquals;

[@deriving (show({with_path: false}), sexp, qcheck)]
type binOp =
  | IntOp(op_bin_int)
  | FloatOp(op_bin_float)
  | BoolOp(op_bin_bool);

[@deriving (show({with_path: false}), sexp, qcheck)]
type op_un_meta =
  | Unquote;

[@deriving (show({with_path: false}), sexp, qcheck)]
type op_un_int =
  | Minus;

[@deriving (show({with_path: false}), sexp, qcheck)]
type op_un_bool =
  | Not;

[@deriving (show({with_path: false}), sexp, qcheck)]
type op_un =
  | Meta(op_un_meta)
  | Int(op_un_int)
  | Bool(op_un_bool);

[@deriving (show({with_path: false}), sexp, qcheck)]
type typ_provenance =
  | Internal;

[@deriving (show({with_path: false}), sexp, qcheck)]
type typ =
  | IntType
  | StringType
  | FloatType
  | BoolType
  | UnitType
  | UnknownType(typ_provenance)
  | TupleType(list(typ))
  | ArrayType(typ)
  | ArrowType(typ, typ)
  | InvalidTyp(t);

[@deriving (show({with_path: false}), sexp, qcheck)]
type pat =
  | EmptyHolePat
  | WildPat
  | IntPat(int)
  | FloatPat(float)
  | VarPat(t)
  | ConstructorPat(t, typ)
  | StringPat(t)
  | TuplePat(list(pat))
  | BoolPat(bool)
  | ConsPat(pat, pat)
  | ListPat(list(pat))
  | ApPat(pat, pat)
  | InvalidPat(t);

[@deriving (show({with_path: false}), sexp, qcheck)]
type if_consistency =
  | Consistent
  | Inconsistent;

[@deriving (show({with_path: false}), sexp, qcheck)]
type deferral_pos =
  | InAp
  | OutsideAp;

[@deriving (show({with_path: false}), sexp, qcheck)]
type tpat =
  | InvalidTPat(t)
  | EmptyHoleTPat
  | VarTPat(t);

[@deriving (show({with_path: false}), sexp, qcheck)]
type exp =
  | Int(int)
  | Float(float)
  | Var(t)
  | Constructor(t, typ)
  | String(t)
  | ListExp([@gen QCheck.Gen.return([])] list(exp))
  | TupleExp(list(exp))
  | BinExp(exp, binOp, exp)
  | UnOp(op_un, exp)
  | Let(pat, exp, exp)
  | Fun(pat, exp, option(t))
  | CaseExp(exp, [@gen QCheck.Gen.return([])] list((pat, exp)))
  | ApExp(exp, exp)
  | FixF(pat, exp)
  | Bool(bool)
  | Cast(exp, typ, typ)
  | FailedCast(exp, typ, typ)
  | EmptyHole
  | Filter(filter_action, exp, exp)
  | BuiltinFun(t)
  | Undefined
  | Seq(exp, exp)
  | Test(exp)
  | Deferral(deferral_pos)
  | TypFun(tpat, exp)
  | Cons(exp, exp)
  | ListConcat(exp, exp)
  | If(exp, exp, exp)
  | InvalidExp(t)
  | TypAp(exp, typ)
  | DeferredAp(exp, exp)
  | DynamicErrorHole(exp, t)
  | TyAlias(tpat, typ, exp);
