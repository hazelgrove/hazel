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
    | Arrow(typ, typ);


[@deriving (show({with_path: false}), sexp, yojson)]
type pat = 
    | Var(string)
    | TypeAnn(pat, typ)
    | TuplePat(list(pat));

[@deriving (show({with_path: false}), sexp, yojson)]
type exp = 
    | Int(int)
    | Float(float)
    | Var(string)
    | String(string)
    | ArrayExp(list(exp))
    | Unit
    | BinExp(exp, binOp, exp)
    | Let(pat, exp, exp)
    | Fun(pat, exp)
    | If(exp, exp, exp);

[@deriving (show({with_path: false}), sexp, yojson)]
type rul =
    | Rules(exp, list((pat, exp)));
