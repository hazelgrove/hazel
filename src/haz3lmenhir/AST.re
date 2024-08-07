open Sexplib.Std;

[@deriving (show({with_path: false}), sexp)]
type filter_action =
  | Pause
  | Debug
  | Hide
  | Eval;

[@deriving (show({with_path: false}), sexp)]
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

[@deriving (show({with_path: false}), sexp)]
type op_bin_bool =
  | And
  | Or;

[@deriving (show({with_path: false}), sexp)]
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

[@deriving (show({with_path: false}), sexp)]
type binOp =
  | IntOp(op_bin_int)
  | FloatOp(op_bin_float)
  | BoolOp(op_bin_bool);

[@deriving (show({with_path: false}), sexp)]
type op_un_meta =
  | Unquote;

[@deriving (show({with_path: false}), sexp)]
type op_un_int =
  | Minus;

[@deriving (show({with_path: false}), sexp)]
type op_un_bool =
  | Not;

[@deriving (show({with_path: false}), sexp)]
type op_un =
  | Meta(op_un_meta)
  | Int(op_un_int)
  | Bool(op_un_bool);

[@deriving (show({with_path: false}), sexp)]
type typ_provenance =
  | Internal;

[@deriving (show({with_path: false}), sexp)]
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
  | InvalidTyp(string);

[@deriving (show({with_path: false}), sexp)]
type pat =
  | EmptyHolePat
  | WildPat
  | MultiHolePat(pat)
  | IntPat(int)
  | FloatPat(float)
  | VarPat(string)
  | ConstructorPat(string, typ)
  | StringPat(string)
  | TuplePat(list(pat))
  | BoolPat(bool)
  | ConsPat(pat, pat)
  | ListPat(list(pat))
  | ApPat(pat, pat)
  | InvalidPat(string);

[@deriving (show({with_path: false}), sexp)]
type if_consistency =
  | Consistent
  | Inconsistent;

[@deriving (show({with_path: false}), sexp)]
type deferral_pos =
  | InAp
  | OutsideAp;

[@deriving (show({with_path: false}), sexp)]
type tpat =
  | InvalidTPat(string)
  | EmptyHoleTPat
  | MultiHoleTPat(tpat)
  | VarTPat(string);

[@deriving (show({with_path: false}), sexp)]
type exp =
  | Int(int)
  | Float(float)
  | Var(string)
  | Constructor(string, typ)
  | String(string)
  | ListExp(list(exp))
  | TupleExp(list(exp))
  | BinExp(exp, binOp, exp)
  | UnOp(op_un, exp)
  | Let(pat, exp, exp)
  | Fun(pat, exp, option(string))
  | CaseExp(exp, list((pat, exp)))
  | ApExp(exp, exp)
  | FixF(pat, exp)
  | Bool(bool)
  | Cast(exp, typ, typ)
  | FailedCast(exp, typ, typ)
  | MultiHole(exp)
  | EmptyHole
  | Filter(filter_action, exp, exp)
  | Seq(exp, exp)
  | Test(exp)
  | Deferral(deferral_pos)
  | TypFun(tpat, exp)
  | Cons(exp, exp)
  | ListConcat(exp, exp)
  | If(exp, exp, exp)
  | InvalidExp(string)
  | TypAp(exp, typ)
  | DeferredAp(exp, exp)
  | DynamicErrorHole(exp, string)
  | TyAlias(tpat, typ, exp);
