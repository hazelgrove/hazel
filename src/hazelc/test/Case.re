open Sexplib.Std;

[@deriving sexp]
type exp =
  | String(string)
  | UH(UHExp.t)
  | DH(DHExp.t);

module type Case = {
  let expr: exp;
  let expect: Compiler.compile_result;
};

module Make = (X: Case) => {
  let opts: Compiler.compile_opts = {expr_only: true};

  let compile = () => {
    switch (X.expr) {
    | String(s) => Compiler.compile_string(~opts, s)
    | UH(e) => Compiler.compile_uhexp(~opts, e)
    | DH(d) => Compiler.compile_dhexp(~opts, d)
    };
  };

  let%test _ = compile() == X.expect;
};
