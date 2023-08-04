open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type binOp =
  | Equals
  | NotEqual
  | Plus
  | Minus
  | Times
  | Divide
  | Power
  | LessThan
  | GreaterThan
  | LessThanEqual
  | GreaterThanEqual
  | Logical_And
  | Logical_Or
  | Logical_Not;

[@deriving (show({with_path: false}), sexp, yojson)]
type typ =
  | IntType
  | StringType
  | FloatType
  | BoolType
  | UnitType
  | TupleType(list(typ))
  | ArrayType(typ)
  | ArrowType(typ, typ);

[@deriving (show({with_path: false}), sexp, yojson)]
type pat =
  | IntPat(int)
  | FloatPat(float)
  | VarPat(string)
  | StringPat(string)
  | Var(string)
  | TypeAnn(pat, typ)
  | AsPat(pat, pat)
  | TuplePat(list(pat))
  | ApPat(pat, pat);

[@deriving (show({with_path: false}), sexp, yojson)]
type exp =
  | Int(int)
  | Float(float)
  | Var(string)
  | String(string)
  | ArrayExp(list(exp))
  | TupleExp(list(exp))
  | Unit
  | BinExp(exp, binOp, exp)
  | Let(pat, exp, exp)
  | Fun(pat, exp)
  | CaseExp(exp, list((pat, exp)))
  | ApExp(exp, exp)
  | If(exp, exp, exp);
