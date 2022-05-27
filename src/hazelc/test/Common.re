module Compile = {
  let temp_prefix = "hazelc_test";

  let opts: Compile.opts = {
    indet_analysis: Some(Local),
    codegen: {
      print_final_expr: true,
    },
  };
  let grain_opts: Compile.grain_opts = {
    grain: None,
    optimize: None,
    includes: None,
    debug: None,
    wat: None,
  };

  let compile = source => {
    switch (Compile.resume_until_printed(~opts, Source(Text(source)))) {
    | Ok(g) =>
      let src_path = Filename.temp_file(temp_prefix, "a.gr");
      let out_path = Filename.temp_file(temp_prefix, "a.wasm");

      switch (Compile.wasmize(~opts=grain_opts, src_path, out_path, g)) {
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

  let run = path => {
    switch (Grain.run(~opts=grain_opts, {wasm: path})) {
    | Ok(output) => output
    | Error(_) => failwith("execution failed")
    };
  };

  let run_no_output = path => {
    /* FIXME: Run function that doesn't collect stdout. */
    switch (Grain.run(~opts=grain_opts, {wasm: path})) {
    | Ok(output) => output
    | Error(_) => failwith("execution failed")
    };
  };
};

module Eval = {
  let parse = source =>
    switch (Hazeltext.Parsing.ast_of_string(source)) {
    | Ok(e) => e
    | Error(err) => failwith(err)
    };

  let elab = e => {
    switch (Elaborator_Exp.elab(Contexts.initial, Delta.empty, e)) {
    | Elaborates(d, _, _) => d
    | DoesNotElaborate => failwith("elaboration failed")
    };
  };

  let eval = Evaluator.evaluate;

  let stringify = (r: EvaluatorResult.t) => {
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
        failwith(
          "Didn't handle some BoxedValue case in string_of_boxed_value",
        )
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
        failwith(
          "Didn't handle some list element case in string_of_cons_tail",
        )
      };
    };

    switch (r) {
    | BoxedValue(d) => string_of_boxed_value(d)
    | Indet(d) => d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string
    };
  };
};
