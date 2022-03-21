[@deriving sexp]
type exp =
  | Str(string)
  | UH(UHExp.t)
  | DH(DHExp.t);

[@deriving sexp]
type expect =
  | Fail
  | Pass(string);

[@deriving sexp]
type t = (exp, expect);

let compile: (exp, string) => Compile.next_result;
let test: t => bool;
