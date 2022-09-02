include Map.Make(Int);
open Sexplib.Std;
module Sexp = Sexplib.Sexp;

[@deriving sexp]
type binding('v) = (int, 'v);
