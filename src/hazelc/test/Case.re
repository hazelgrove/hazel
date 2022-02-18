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
  let opts: Compiler.opts = {
    exp_only: true,
    grain: {
      grain: None,
      optimize: None,
      includes: None,
      debug: Some(true),
      wat: None,
    },
  };

  switch (exp) {
  | Str(s) => Compiler.grain_compile_string(~opts, s)
  | UH(e) => Compiler.grain_compile_uhexp(~opts, e)
  | DH(d) => Compiler.grain_compile_dhexp(~opts, d)
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
