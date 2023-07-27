open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type exp = 
    | Int(int)
    | Float(float)
    | String(string)
    | Ident(string)
    | BinOp(exp, string, exp)
    | Let(string, exp, exp)
    | Fun(string, exp, exp);
