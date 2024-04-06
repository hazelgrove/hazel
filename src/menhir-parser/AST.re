open Sexplib.Std;

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
  | Equals
  | NotEquals;

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
  | Equals
  | NotEquals;

[@deriving (show({with_path: false}), sexp, yojson)]
type binOp =
  | IntOp(op_bin_int)
  | FloatOp(op_bin_float)
  | BoolOp(op_bin_bool);

[@deriving (show({with_path: false}), sexp, yojson)]
type typ_provenance =
  | Internal;

[@deriving (show({with_path: false}), sexp, yojson)]
type typ =
  | IntType
  | StringType
  | FloatType
  | BoolType
  | UnitType
  | UnknownType(typ_provenance)
  | TupleType(list(typ))
  | ArrayType(typ)
  | ArrowType(typ, typ);

[@deriving (show({with_path: false}), sexp, yojson)]
type pat =
  | IntPat(int)
  | FloatPat(float)
  | VarPat(string)
  | StringPat(string)
  | TypeAnn(pat, typ)
  | TuplePat(list(pat))
  | BoolPat(bool)
  | ApPat(pat, pat);

[@deriving (show({with_path: false}), sexp, yojson)]
type exp =
  | Int(int)
  | Float(float)
  | Var(string)
  | FreeVar(string)
  | String(string)
  | ArrayExp(list(exp))
  | TupleExp(list(exp))
  | BinExp(exp, binOp, exp)
  | Let(pat, exp, exp)
  | Fun(typ, pat, exp, option(string))
  | FixF(string, typ, exp)
  | CaseExp(exp, list((pat, exp)))
  | InconsistentCaseExp(exp, list((pat, exp)))
  | ApExp(exp, exp)
  | Bool(bool)
  | Cast(exp, typ, typ)
  | NonEmptyHole(exp)
  | EmptyHole
  | If(exp, exp, exp);
