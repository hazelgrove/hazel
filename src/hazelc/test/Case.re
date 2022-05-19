let temp_prefix = "hazelc_test";

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

let compile = s => {
  switch (Compile.resume_until_grain_text(~opts, Source(SourceString(s)))) {
  | Ok(g) =>
    let src_path = Filename.temp_file(temp_prefix, "a.gr");
    let out_path = Filename.temp_file(temp_prefix, "a.wasm");

    switch (Compile.wasmize_next(~opts, src_path, out_path, g)) {
    | Ok () => out_path
    | Error () => failwith("wasm compilation failed")
    };

  | Error(err) =>
    switch (err) {
    | ParseError(err) => failwith(err)
    | ElaborateError => failwith("elaboration failed")
    }
  };
};

let compile_run = exp => {
  let out_path = compile(exp);
  switch (Grain.run(~opts=opts.grain, {wasm: out_path})) {
  | Ok(output) => output
  | Error(_) => failwith("execution failed")
  };
};

let eval = s => {
  let elab = Elaborator_Exp.elab(Contexts.initial, Delta.empty);

  switch (Hazeltext.Parsing.ast_of_string(s)) {
  | Ok(e) =>
    switch (elab(e)) {
    | Elaborates(d, _, _) => d
    | DoesNotElaborate => failwith("elaboration failed")
    }
  | Error(err) => failwith(err)
  };
};

let test = (exp, expect) => {
  let res = compile_run(exp);
  Base.([%test_eq: string](res, expect));
};

let test_with_eval = exp => {
  let res = compile_run(exp);
  // FIXME: Fully complete program will print literal
  // FIXME: Sexp implementations will not fully match :(
  let expect = eval(exp) |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string;

  Base.([%test_eq: string](res, expect));
};
