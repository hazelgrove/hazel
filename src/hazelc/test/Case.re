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

let opts: Compile.opts = {
  exp_only: true,
  grain: {
    grain: None,
    optimize: None,
    includes: None,
    debug: Some(true),
    wat: None,
  },
};

let compile = (exp, outpath) => {
  switch (exp) {
  | Str(s) => Compile.resume(~opts, outpath, Source(SourceString(s)))
  | UH(e) => Compile.resume(~opts, outpath, Parsed(e))
  | DH(d) => Compile.resume(~opts, outpath, Elaborated(d))
  };
};

/*
   Unexpected state (indicative of an error in Compiler.resume).
 */
exception BadState;

let test = ((exp, expect)) => {
  // TODO: Use temporary file
  let outpath = "a.wasm";

  let res =
    switch (compile(exp, outpath)) {
    | Ok(state) =>
      switch (state) {
      | Wasmized(path) =>
        switch (Grain.run(~opts=opts.grain, {wasm: path})) {
        | Error(_) => Fail
        | Ok(s) => Pass(s)
        }
      | _ => raise(BadState)
      }
    | Error(_) => Fail
    };

  res == expect;
};
