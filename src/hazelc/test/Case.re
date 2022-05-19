open Sexplib.Std;
open ResultUtil.Syntax;

[@deriving sexp]
type expect =
  | Fail
  | Pass(string);

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

let compile = s =>
  Compile.resume_until_grain_text(~opts, Source(SourceString(s)));

let temp_prefix = "hazelc_test";
let compile_run = exp => {
  switch (compile(exp)) {
  | Ok(g) =>
    let src_path = Filename.temp_file(temp_prefix, "a.gr");
    let out_path = Filename.temp_file(temp_prefix, "a.wasm");
    switch (Compile.wasmize_next(~opts, src_path, out_path, g)) {
    | Ok () =>
      Grain.run(~opts=opts.grain, {wasm: out_path})
      |> Result.map_error(_ => ())
    | Error () => Error()
    };

  | Error(_) => Error()
  };
};

type ResultUtil.error +=
  | ParseError(string)
  | ElaborateError;

let eval = s => {
  let elab = Elaborator_Exp.elab(Contexts.initial, Delta.empty);

  let* e =
    Hazeltext.Parsing.ast_of_string(s)
    |> Result.map_error(err => ParseError(err));

  let+ d =
    switch (elab(e)) {
    | Elaborates(d, _, _) => Ok(d)
    | DoesNotElaborate => Error(ElaborateError)
    };

  d;
};

let test = (exp, expect) => {
  let res = compile_run(exp);

  switch (res) {
  | Ok(res) => res == expect
  | Error(_) => false
  };
};

let test_with_eval = exp => {
  let res = compile_run(exp);

  switch (res) {
  | Ok(res) =>
    switch (eval(exp)) {
    | Ok(expect) =>
      let expect = expect |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string;
      res == expect;
    | Error(_) => false
    }
  | Error(_) => false
  };
};
