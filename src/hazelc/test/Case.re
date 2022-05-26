let temp_prefix = "hazelc_test";

let opts: Compile.opts = {
  indet_analysis: Some(Local),
  grain: {
    grain: None,
    optimize: None,
    includes: None,
    debug: None,
    wat: None,
  },
};

let compile = s => {
  switch (Compile.resume_until_grain_text(~opts, Source(Text(s)))) {
  | Ok(g) =>
    let src_path = Filename.temp_file(temp_prefix, "a.gr");
    let out_path = Filename.temp_file(temp_prefix, "a.wasm");

    switch (Compile.wasmize(~opts, src_path, out_path, g)) {
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

  let d =
    switch (Hazeltext.Parsing.ast_of_string(s)) {
    | Ok(e) =>
      switch (elab(e)) {
      | Elaborates(d, _, _) => d
      | DoesNotElaborate => failwith("elaboration failed")
      }
    | Error(err) => failwith(err)
    };

  Evaluator.evaluate(d);
};

let rec string_of_boxed_value = (d: DHExp.t) => {
  switch (d) {
  | BoolLit(b) => string_of_bool(b)
  | IntLit(n) => string_of_int(n)
  | FloatLit(f) =>
    // FIXME: Irrational floats will not print the same.
    if (float_of_int(int_of_float(f)) == f) {
      Printf.sprintf("%.1f", f);
    } else {
      string_of_float(f);
    }
  | ListNil(_) => "[]"
  | Pair(d1, d2) => string_of_pair(d1, d2)
  | Cons(hd, tl) => string_of_cons(hd, tl)
  | Triv => "void"
  | _ =>
    failwith("Didn't handle some BoxedValue case in string_of_boxed_value")
  };
}
and string_of_pair = (d1, d2) => {
  let s1 = string_of_boxed_value(d1);
  let s2 = string_of_boxed_value(d2);
  "(" ++ s1 ++ ", " ++ s2 ++ ")";
}
and string_of_cons = (hd, tl) => {
  "[" ++ string_of_boxed_value(hd) ++ string_of_cons_tail(tl) ++ "]";
}
and string_of_cons_tail = tl => {
  switch (tl) {
  | ListNil(_) => ""
  | Cons(hd, tl) =>
    ", " ++ string_of_boxed_value(hd) ++ string_of_cons_tail(tl)
  | _ =>
    failwith("Didn't handle some list element case in string_of_cons_tail")
  };
};

let test_with_eval = exp => {
  // Compile and execute expression.
  let compile_out = compile_run(exp);
  // Evaluate expression.
  let eval_res = eval(exp);

  // Transform evaluation result into an sexp.
  let eval_out =
    switch (eval_res) {
    | BoxedValue(d) => string_of_boxed_value(d)
    | Indet(d) => d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string
    };

  Base.([%test_eq: string](compile_out, eval_out));
};

let test = (exp, expect) => {
  let res = compile_run(exp);
  Base.([%test_eq: string](res, expect));
};
