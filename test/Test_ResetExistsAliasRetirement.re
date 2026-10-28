/* D83 (dq-D83-5ccc282c, quantifier): `exists` is the existential
   keyword; the builtin list alias `exists(list, predicate)` is retired
   in favour of `any(list, predicate)`. Each check is its own test and
   runs the real parser / statics / evaluator. The old alias call is
   kept as a RETIRED negative control. */
open Alcotest;
open Language;

module RE = Test_ResetExistentials;

let has_builtin = name =>
  List.exists(
    b => BuiltinsUtil.name_of_builtin(b) == name,
    Builtins.builtins,
  );
let in_ctx = name =>
  Ctx.lookup_var(Builtins.ctx_init(Some(Operators.default_mode)), name)
  != None;
let in_env = name => Environment.lookup(Builtins.env_init, name) != None;

let registry = name =>
  "builtin "
  ++ string_of_bool(has_builtin(name))
  ++ " ctx "
  ++ string_of_bool(in_ctx(name))
  ++ " env "
  ++ string_of_bool(in_env(name));

/* A well-typed program: statics clean, then its value. */
let any_case = (label, src, expected) =>
  test_case("any " ++ label ++ " ~> " ++ expected, `Quick, () => {
    let o = RE.observe("alias_" ++ label, src);
    RE.valid("alias_" ++ label, o);
    check(string, label, expected, RE.eval_print(src));
  });

let old_call = "exists([1, 2], fun (x : Int) -> x > 1)";

let tests = (
  "ResetExistsAliasRetirement",
  [
    test_case("registry: builtin alias `exists` is absent (builtins, ctx, env)", `Quick, () => {
      print_endline("ALIAS registry exists: " ++ registry("exists"));
      check(bool, "not a builtin", false, has_builtin("exists"));
      check(bool, "not in the initial ctx", false, in_ctx("exists"));
      check(bool, "not in the initial env", false, in_env("exists"));
    }),
    test_case("registry: `any` is present (builtins, ctx, env)", `Quick, () => {
      print_endline("ALIAS registry any: " ++ registry("any"));
      check(bool, "builtin", true, has_builtin("any"));
      check(bool, "ctx", true, in_ctx("any"));
      check(bool, "env", true, in_env("any"));
    }),
    any_case("positive", "any([1, 2], fun (x : Int) -> x > 1)", "true"),
    any_case("negative", "any([1, 2], fun (x : Int) -> x > 5)", "false"),
    any_case("empty", "any([], fun (x : Int) -> x > 1)", "false"),
    test_case("RETIRED negative control: the old list call is not a list function", `Quick, () => {
      switch (Haz3lcore.Parser.to_term(old_call, ~root=Exp)) {
      | None =>
        print_endline("ALIAS old call " ++ old_call ++ " => PARSE FAILURE")
      | Some(e) =>
        let (statics, elab) =
          Statics.mk(
            CoreSettings.on,
            Builtins.ctx_init(Some(Operators.default_mode)),
            e,
          );
        let errs = Haz3lcore.ErrorPrint.all(statics);
        let (r, _) = Evaluator.evaluate(~env=Builtins.env_init, elab);
        let cls = Exp.show_cls(Exp.cls_of_term(e.term));
        let v = RE.print_exp(r);
        print_endline(
          "ALIAS old call "
          ++ old_call
          ++ " => parsed as "
          ++ cls
          ++ "; static errors "
          ++ string_of_int(List.length(errs))
          ++ " ["
          ++ String.concat(" | ", errs)
          ++ "]; value "
          ++ v,
        );
        let is_ap =
          switch (e.term) {
          | Ap(_) => true
          | _ => false
          };
        check(bool, "not parsed as an application", false, is_ap);
        check(bool, "statics reject it", true, errs != []);
        check(bool, "does not compute a list result", false, v == "true" || v == "false");
      };
    }),
    test_case("existential syntax: parse/print fixed point and clean statics", `Quick, () => {
      let src = "exists (n : Int) -> n * n == 9";
      let t1 = RE.print_exp(Test_Evaluator_Prelude.parse_exp(src));
      let t2 = RE.print_exp(Test_Evaluator_Prelude.parse_exp(t1));
      print_endline("ALIAS roundtrip " ++ src ++ " => " ++ t1);
      check(string, "fixed point", t1, t2);
      RE.valid("alias_exists_syntax", RE.observe("alias_exists_syntax", src));
    }),
    test_case("existential syntax: witness proof still Proven", `Quick, () => {
      let o =
        RE.observe(
          "alias_exists_witness",
          "theorem w = exists (n : Int) -> n == 3 proof == true by witness 3 end; axiom refl_eq at 0 on 3 == 3 end in 0",
        );
      RE.valid("alias_exists_witness", o);
      check(string, "full", "Proven", RE.full(o, "w"));
    }),
  ],
);
