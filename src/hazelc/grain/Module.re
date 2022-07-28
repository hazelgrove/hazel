open Sexplib.Std;

[@deriving sexp]
type t = (list(Decl.t), Expr.block);

let empty = ([], []);
