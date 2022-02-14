open Sexplib.Std;

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

let compile = exp => {
  let opts: Compiler.opts = {expr_only: true};

  switch (exp) {
  | Str(s) => Compiler.compile_string(~opts, s)
  | UH(e) => Compiler.compile_uhexp(~opts, e)
  | DH(d) => Compiler.compile_dhexp(~opts, d)
  };
};

let test = ((exp, expect)) => {
  let res =
    switch (compile(exp)) {
    | Ok(output) => Pass(output)
    | Error(_) => Fail
    };
  res == expect;
};
