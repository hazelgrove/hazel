let temp_prefix = "hazelc_test";

let opts: Compile.opts = {
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
  // Compile and execute expression.
  let compile_out = compile_run(exp);
  // Evaluate expression.
  let eval_res = eval(exp);

  // Transform evaluation result into an sexp.
  let eval_out =
    // FIXME: This approach of unwrapping doesn't really work for all
    // constructs (e.g. when compiler output is a primitive tuple; need to
    // inspect evaluation result for primitive tuple/cons/etc.)
    switch (eval_res) {
    | BoolLit(b) => string_of_bool(b)
    | IntLit(n) => string_of_int(n)
    | FloatLit(f) => string_of_float(f)
    | ListNil(_) => "[]"
    | Triv => "void"
    | d => d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string
    };

  Base.([%test_eq: string](compile_out, eval_out));
};
