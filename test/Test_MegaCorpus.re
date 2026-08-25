open Alcotest;
open Haz3lcore;
open Language;

/* Mega-corpus gate (hazel-programs/mega): each variant must
   fast-parse, typecheck with ZERO error ids, evaluate to a non-indet
   result, and have every `test` pass. Also reports Statics.mk wall
   time. Register in test/haz3ltest.re to run:
     bash test/run_node.sh test 'MegaCorpus' */

let read_file = (path: string): option(string) =>
  switch (open_in_bin(path)) {
  | ic =>
    let n = in_channel_length(ic);
    let s = really_input_string(ic, n);
    close_in(ic);
    Some(s);
  | exception _ => None
  };

let check_variant = (name: string): unit => {
  let path = "hazel-programs/mega/" ++ name;
  let path = Sys.file_exists(path) ? path : "../hazel-programs/mega/" ++ name;
  switch (read_file(path)) {
  | None => fail("unreadable: " ++ name)
  | Some(src) =>
    switch (
      FastParse.of_text(
        ~materialize=Triggers.invoked_projector,
        ~collect_refractors=true,
        ~root=Exp,
        src,
      )
    ) {
    | None =>
      fail(
        "fast-parse BAILED: "
        ++ name
        ++ " -- "
        ++ Option.value(FastParse.bail_note^, ~default="?"),
      )
    | Some(seg) =>
      let term = MakeTerm.go(seg).term;
      let t0 = Sys.time();
      let (info_map, elab) =
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Operators.default_mode)),
          term,
        );
      let statics_ms = (Sys.time() -. t0) *. 1000.;
      let errors = List.length(Statics.Map.error_ids(info_map));
      if (errors > 0) {
        List.iteri(
          (i, e) =>
            if (i < 6) {
              Printf.printf("MEGAERR %s: %s\n", name, e);
            },
          ErrorPrint.all(info_map),
        );
      };
      let t1 = Sys.time();
      let (result, state) = Evaluator.evaluate(~env=Builtins.env_init, elab);
      let eval_ms = (Sys.time() -. t1) *. 1000.;
      let tests = EvaluatorState.get_tests(state);
      let n_tests = List.length(tests);
      let failing =
        List.filter(
          ((_, reports)) =>
            TestMap.joint_status(reports) != TestStatus.Pass,
          tests,
        );
      List.iter(
        ((id, reports)) =>
          Printf.printf(
            "MEGAFAIL %s: test %s [%s]\n",
            name,
            Id.to_string(id),
            String.concat("; ", TestMap.joint_hints(reports)),
          ),
        failing,
      );
      /* the corpus programs end in `final.ok == N`: the result must be
         literally `true` — one uniform, decisive gate */
      let result_true =
        switch (result.term) {
        | Atom(Bool(b)) => b
        | _ => false
        };
      Printf.printf(
        "MEGA %s: %d lines, statics %.0fms, eval %.0fms, %d errors, %d/%d tests pass, result_true %b\n",
        name,
        List.length(String.split_on_char('\n', src)),
        statics_ms,
        eval_ms,
        errors,
        n_tests - List.length(failing),
        n_tests,
        result_true,
      );
      check(int, name ++ ": zero static errors", 0, errors);
      check(int, name ++ ": zero failing tests", 0, List.length(failing));
      check(bool, name ++ ": result is true", true, result_true);
    }
  };
};

let tests = (
  "MegaCorpus",
  [
    test_case("mega-1k", `Quick, () => check_variant("mega-1k.hz")),
    test_case("mega-2k", `Quick, () => check_variant("mega-2k.hz")),
    test_case("mega-4k", `Quick, () => check_variant("mega-4k.hz")),
  ],
);
