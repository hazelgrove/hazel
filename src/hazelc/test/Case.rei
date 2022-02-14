[@deriving sexp]
type exp =
  | String(string)
  | UH(UHExp.t)
  | DH(DHExp.t);

module type Case = {
  let expr: exp;
  let expect: Compiler.compile_result;
};
module Make: (X: Case) => {let compile: unit => Compiler.compile_result;};
