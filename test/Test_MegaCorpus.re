open Alcotest;
open Haz3lcore;
open Language;

/* each mega program fast-parses, has no static errors, passes every test,
   and evaluates to true */

let check_variant = (name: string): unit => {
  let term = MakeTerm.go(CorpusUtil.mega_seg(name)).term;
  let (info_map, elab) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    );
  if (Statics.Map.error_ids(info_map) != []) {
    fail(
      name
      ++ " has static errors: "
      ++ String.concat(
           "; ",
           Util.ListUtil.take(6, ErrorPrint.all(info_map)),
         ),
    );
  };
  let (result, state) = Evaluator.evaluate(~env=Builtins.env_init, elab);
  let failing =
    EvaluatorState.get_tests(state)
    |> List.filter(((_, reports)) =>
         TestMap.joint_status(reports) != TestStatus.Pass
       )
    |> List.map(((id, reports)) =>
         Id.to_string(id)
         ++ " ["
         ++ String.concat("; ", TestMap.joint_hints(reports))
         ++ "]"
       );
  check(list(string), name ++ ": failing tests", [], failing);
  /* the corpus programs end in `final.ok == N` */
  check(
    bool,
    name ++ ": result is true",
    true,
    switch (result.term) {
    | Atom(Bool(b)) => b
    | _ => false
    },
  );
};

let tests = (
  "MegaCorpus",
  [
    test_case("mega-1k", `Quick, () => check_variant("mega-1k.hz")),
    test_case("mega-2k", `Quick, () => check_variant("mega-2k.hz")),
    test_case("mega-4k", `Quick, () => check_variant("mega-4k.hz")),
  ],
);
