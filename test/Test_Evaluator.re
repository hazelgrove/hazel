open Alcotest;
open Haz3lcore;
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

let evaluation_test = (msg, expected, unevaluated) =>
  check(
    dhexp_typ,
    msg,
    expected,
    unevaluated |> Evaluator.evaluate(~env=Builtins.env_init) |> fst,
  );

let evaluate_probes = unevaluated =>
  unevaluated
  |> Evaluator.evaluate(~env=Builtins.env_init)
  |> snd
  |> EvaluatorState.get_probes;

let parse_exp = (s: string) => {
  switch (MakeTerm.parse_exp(s)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};
let elaborate = u =>
  Elaborator.elaborate(
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), u),
    u,
  )
  |> fst;

module PGrammar =
  Grammar.Factory({
    type t = list(Grammar.exp_t(unit));
    let default_value = (): list(Grammar.exp_t(unit)) => [];
  });
module UG = Grammar.UnitGrammar;
let probe_test =
    (msg: string, expected: Grammar.exp_t(list(Grammar.exp_t(unit)))) => {
  let fresh: Exp.t =
    Grammar.map_exp_annotation(_ => IdTagged.IdTag.fresh(), expected);
  let elaborated = elaborate(fresh);
  let probes = evaluate_probes(elaborated);
  let probed: Grammar.exp_t(list(Grammar.exp_t(unit))) =
    Grammar.map_exp_annotation(
      ({ids, _}: IdTagged.IdTag.t) => {
        let probe_closures = Dynamics.Map.lookup(List.hd(ids), probes);
        Option.map(
          List.map((c: Dynamics.Probe.Closure.t) =>
            Grammar.map_exp_annotation(_ => (), DHExp.strip_casts(c.value))
          ), // Idk why there's casts on the probed values
          probe_closures,
        )
        |> Option.value(~default=[]);
      },
      fresh,
    );

  check(
    testable(
      Fmt.using(
        [%derive.show: Grammar.exp_t(list(Grammar.exp_t(unit)))],
        Fmt.string,
      ),
      Grammar.equal_exp_t(List.equal(Grammar.equal_exp_t(Unit.equal))),
    ),
    msg,
    expected,
    probed,
  );
};

let probed_value = (exp): Grammar.exp_t(unit) => {
  term: exp,
  annotation: (),
};
let expected_probe =
    (exp, probes): Grammar.exp_t(list(Grammar.exp_t(unit))) => {
  term: exp,
  annotation: probes,
};
let expected_probe_pat =
    (exp, probes): Grammar.pat_t(list(Grammar.exp_t(unit))) => {
  term: exp,
  annotation: probes,
};
let parse_and_evaluate = (s: string) =>
  fst(Evaluator.evaluate(~env=Builtins.env_init, elaborate(parse_exp(s))));

let parse_and_evaluate_test =
    (~msg: option(string)=?, expected: string, actual: string) =>
  evaluation_test(
    Option.value(~default=expected ++ " == " ++ actual, msg),
    parse_exp(expected),
    elaborate(parse_exp(actual)),
  );
open IdTagged.FreshGrammar;
open Exp;
let test_int = () => evaluation_test("8", int(8), int(8));

let test_sum = () =>
  evaluation_test("4 + 5", int(9), bin_op(Int(Plus), int(4), int(5)));

let test_labeled_tuple_projection = () =>
  evaluation_test(
    "(a=1, b=2, c=?).a",
    int(1),
    dot(
      tuple([
        tup_label(label("a"), int(1)),
        tup_label(label("b"), int(2)),
        tup_label(label("c"), empty_hole()),
      ]),
      label("a") // This is a var now for parsing reasons
    ),
  );

let test_function_application = () =>
  evaluation_test(
    "float_of_int(1)",
    float(1.0),
    ap(Forward, var("float_of_int"), int(1)),
  );

let test_function_deferral = () =>
  evaluation_test(
    "string_sub(\"hello\", 1, _)(2)",
    string("el"),
    ap(
      Forward,
      deferred_ap(
        var("string_sub"),
        [string("hello"), int(1), deferral(InAp)],
      ),
      int(2),
    ),
  );

let test_ap_of_hole_deferral = () =>
  evaluation_test(
    "?(_, _, 3)(1., true)",
    ap(
      Forward,
      cast(
        empty_hole(),
        Typ.unknown(Internal),
        Typ.(arrow(unknown(Internal), unknown(Internal))),
      ),
      cast(
        tuple([
          cast(float(1.), Typ.float(), Typ.unknown(Internal)),
          cast(bool(true), Typ.bool(), Typ.unknown(Internal)),
          cast(int(3), Typ.int(), Typ.unknown(Internal)),
        ]),
        Typ.(
          prod([unknown(Internal), unknown(Internal), unknown(Internal)])
        ),
        Typ.unknown(Internal),
      ),
    ),
    ap(
      Forward,
      deferred_ap(
        cast(
          cast(
            empty_hole(),
            Typ.unknown(Internal),
            Typ.(arrow(unknown(Internal), unknown(Internal))),
          ),
          Typ.(arrow(unknown(Internal), unknown(Internal))),
          Typ.(
            arrow(
              prod([
                unknown(Internal),
                unknown(Internal),
                unknown(Internal),
              ]),
              unknown(Internal),
            )
          ),
        ),
        [
          deferral(InAp),
          deferral(InAp),
          cast(int(3), Typ.int(), Typ.unknown(Internal)),
        ],
      ),
      tuple([
        cast(float(1.), Typ.float(), Typ.unknown(Internal)),
        cast(bool(true), Typ.bool(), Typ.unknown(Internal)),
      ]),
    ),
  );

let test_multi_arg_builtin_cast = () =>
  evaluation_test(
    "string_compare((\"Hello\", \"World\"):(?, ?))",
    int(-1),
    ap(
      Forward,
      builtin_fun("string_compare"),
      cast(
        tuple([
          cast(string("Hello"), Typ.string(), Typ.unknown(Internal)),
          cast(string("World"), Typ.string(), Typ.unknown(Internal)),
        ]),
        Typ.(prod([Typ.unknown(Internal), Typ.unknown(Internal)])),
        Typ.(prod([string(), string()])),
      ),
    ),
  );

let test_variable_capture = () =>
  evaluation_test(
    {|let u = 5 in let f = fun () -> u in let u = 3 in f()|},
    int(5),
    let_(
      Pat.(var("u")),
      int(5),
      let_(
        Pat.(var("f")),
        fn(Pat.(tuple([])), var("u"), None, None),
        let_(Pat.(var("u")), int(3), ap(Forward, var("f"), tuple([]))),
      ),
    ),
  );

let test_unbound_lookup = () =>
  evaluation_test(
    "(fun x -> x)(x)",
    var("x"),
    ap(Forward, fn(Pat.(var("x")), var("x"), None, None), var("x")),
  );

let test_unevaluated_if = () =>
  evaluation_test(
    "let x = 5 in if ? then x else x",
    if_(empty_hole(), int(5), int(5)),
    let_(Pat.(var("x")), int(5), if_(empty_hole(), var("x"), var("x"))),
  );

let test_invalid_constructor_match = () => {
  let invalid_constructor_match =
    elaborate(
      let_(Pat.(constructor("T", Some(None))), int(1), empty_hole()),
    );
  evaluation_test(
    "let T = 1 in ?",
    invalid_constructor_match,
    invalid_constructor_match,
  );
};

let test_typfun_application = () =>
  evaluation_test(
    "(typfun T -> fun x -> 1)@<Int>(2)",
    int(1),
    ap(
      Forward,
      typ_ap(
        typ_fun(
          TPat.(var("T")),
          fn(Pat.(var("x")), int(1), None, None),
          None,
        ),
        Typ.int(),
      ),
      int(2),
    ),
  );

let skip_current_unboxing_error = (err: string, expression: string) =>
  test_case(err ++ " (Unboxing Error)", `Quick, () => {
    [@warning "-21"]
    {
      // Currently fails https://github.com/hazelgrove/hazel/issues/1588
      Alcotest.skip();
      let exp = parse_and_evaluate(expression);
      check(pass, err, exp, exp);
    }
  });

let qcheck_evaluator_does_not_crash_test =
  QCheck.Test.make(
    ~name="Evaluator does not crash",
    ~count=10000,
    QCheck_Util.arb_exp(~minimal_idents=true, 50),
    exp => {
    switch (
      Elaborator.elaborate(
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
        exp,
      )
      |> fst
    ) {
    | exp =>
      switch (
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=10000,
          exp,
        )
      ) {
      | Completed((_, _))
      | StepLimitExceeded => true
      | exception e =>
        switch (e) {
        | Failure(msg)
            when
              List.exists(
                (==)(msg),
                ["type application in dynamics"] // "type application in dynamics" https://github.com/hazelgrove/hazel/issues/1625
              ) =>
          print_endline("Skipping failure: " ++ msg);
          true;
        // https://github.com/hazelgrove/hazel/issues/1588 unboxing errors
        | EvaluatorError.Exception(InvalidBoxedListLit(_))
        | EvaluatorError.Exception(InvalidBoxedBoolLit(_))
        | EvaluatorError.Exception(InvalidBoxedListCons(_))
        | EvaluatorError.Exception(InvalidBoxedTuple(_))
        | EvaluatorError.Exception(InvalidBoxedSumConstructor(_))
        | EvaluatorError.Exception(InvalidBoxedFloatLit(_))
        | EvaluatorError.Exception(InvalidBoxedIntLit(_))
        | EvaluatorError.Exception(InvalidBoxedStringLit(_))
        | EvaluatorError.Exception(InvalidBoxedTypFun(_)) => true
        | _ => raise(e)
        }
      }
    | exception e =>
      print_endline(
        "Skipping statics/elaborate failure: " ++ Printexc.to_string(e),
      );
      true;
    }
  });

let tests = (
  "Evaluator",
  [
    test_case("Integer literal", `Quick, test_int),
    test_case("Integer sum", `Quick, test_sum),
    test_case("Function application", `Quick, test_function_application),
    test_case("Function deferral", `Quick, test_function_deferral),
    test_case("Elaborated Pattern for labeled tuple", `Quick, () =>
      parse_and_evaluate_test(
        "2",
        {|let x : (a=Int) -> Int = fun a -> a in x(2)|},
      )
    ),
    test_case("Labeled tuple field access", `Quick, () =>
      parse_and_evaluate_test("1", {|(a=1,b=2).a|})
    ),
    test_case("Anonymous function with explicit label", `Quick, () => {
      parse_and_evaluate_test(
        "5",
        {|let fn : (a=String) -> Int =
  fun (a=a : String) -> string_length(a)
in fn("hello")|},
      )
    }),
    test_case("Anonymous function without explicit label", `Quick, () => {
      parse_and_evaluate_test(
        "5",
        {|let fn : (a=String) -> Int =
            fun (a : String) -> string_length(a)
          in fn("hello")|},
      )
    }),
    test_case("Dot operation for missing label", `Quick, () =>
      parse_and_evaluate_test("(a=1,b=2).c", "(a=1,b=2).c")
    ),
    test_case("Desructuring labeled tuple", `Quick, () =>
      parse_and_evaluate_test(
        "(1, 2, 3.0)",
        {|let (a=a', b=b', c) = (a=1, b=2, 3.0) in (a',b',c)|},
      )
    ),
    test_case("Deferral applied to hole", `Quick, test_ap_of_hole_deferral),
    test_case(
      "Multi-arg builtin with cast",
      `Quick,
      test_multi_arg_builtin_cast,
    ),
    test_case("Variable capture", `Quick, test_variable_capture),
    test_case("Unbound lookup", `Quick, test_unbound_lookup),
    test_case("Unevaluated if closure", `Quick, test_unevaluated_if),
    test_case(
      "Invalid constructor match",
      `Quick,
      test_invalid_constructor_match,
    ),
    test_case("Typfun application", `Quick, test_typfun_application),
    test_case("Negative integer literal", `Quick, () =>
      evaluation_test("-8", int(-8), un_op(Int(Minus), int(8)))
    ),
    test_case("String_concat builtin", `Quick, () => {
      parse_and_evaluate_test(
        {|"hazel hello world"|},
        {|string_join(" ", ["hazel", "hello", "world"])|},
      )
    }),
    test_case("Simple probe", `Quick, () => {
      PGrammar.(
        probe_test(
          "let x = 1 + 2 in 4",
          Exp.(
            let_(
              Pat.(var("x")),
              Exp.(
                probe(
                  ~ann=[probed_value(Atom(Int(Bigint.of_int(3))))],
                  bin_op(Int(Plus), int(1), int(2)),
                  {refs: []},
                )
              ),
              var("x"),
            )
          ),
        )
      )
    }),
    test_case(
      "Probes in factorial function",
      `Quick,
      () => {
        // TODO Better helpers. We really need a way to build these with a builder for the "free element".
        open PGrammar;
        module UE = UG.Exp;
        module UP = UG.Pat;
        let npp = expected_probe_pat(_, []);
        let np = expected_probe(_, []);
        let p = (p, es: list(UG.Exp.t)) =>
          expected_probe(Probe(p, {refs: []}), es);
        let pp = (p, es: list(UE.t)) =>
          expected_probe_pat(Probe(npp(p), {refs: []}), es);

        probe_test(
          {|let fact = fun x ->
           case x
             | 1 => 1
             | _ =>
             let r = fact(x-1)
             in x*r
         end in fact(5)|},
          Exp.(
            let_(
              Pat.(var("fact")),
              fn(
                pp(
                  Var("x"),
                  UE.[int(5), int(4), int(3), int(2), int(1)],
                ),
                match(
                  p(
                    var("x"),
                    UE.[int(5), int(4), int(3), int(2), int(1)],
                  ),
                  [
                    (Pat.(int(1)), p(int(1), UE.[int(1)])),
                    (
                      Pat.wild(),
                      np(
                        Let(
                          npp(Var("r")),
                          p(
                            ap(
                              Forward,
                              var("fact"),
                              bin_op(Int(Minus), var("x"), int(1)),
                            ),
                            UE.[int(1), int(2), int(6), int(24)],
                          ),
                          p(
                            bin_op(Int(Times), var("x"), var("r")),
                            UE.[int(2), int(6), int(24), int(120)],
                          ),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                None,
              ),
              ap(Forward, var("fact"), int(5)),
            )
          ),
        );
      },
    ),
    test_case(
      "Evaluate probe around inferred labeled tuple",
      `Quick,
      () => {
        let npp = expected_probe_pat(_, []);
        let np = expected_probe(_, []);
        let p = (p, es: list(Grammar.exp_term(unit))) =>
          expected_probe(
            Probe(np(p), {refs: []}),
            List.map(Grammar.Annotated.empty, es),
          );
        let npt = (t): Grammar.typ_t(list(Grammar.exp_t(unit))) => {
          term: t,
          annotation: [],
        };
        let uexp =
          np(
            Let(
              npp(
                Cast(
                  npp(Var("x")),
                  npt(
                    Parens(
                      npt(
                        Prod([
                          npt(
                            TupLabel(npt(Label("l")), npt(Atom(String))),
                          ),
                        ]),
                      ),
                    ),
                  )
                  |> TypSlice.t_of_typ_t_parametric,
                  npt(Unknown(Internal)) |> TypSlice.t_of_typ_t_parametric,
                ),
              ),
              p(
                Atom(String("a")),
                [
                  Tuple([
                    {
                      term:
                        TupLabel(
                          {
                            term: Label("l"),
                            annotation: (),
                          },
                          {
                            term: Atom(String("a")),
                            annotation: (),
                          },
                        ),
                      annotation: (),
                    },
                  ]),
                ],
              ),
              np(Var("x")),
            ),
          );
        probe_test({|let x : (a=String) = PROBE("a") in x|}, uexp);
      },
    ),
    test_case(
      "Evaluate probe around inferred labeled tuple",
      `Quick,
      () => {
        let np = expected_probe(_, []);
        let p = (p, es: list(Grammar.exp_term(unit))) =>
          expected_probe(
            Probe(np(p), {refs: []}),
            List.map(Grammar.Annotated.empty, es),
          );
        let npt = (t): Grammar.typ_t(list(Grammar.exp_t(unit))) => {
          term: t,
          annotation: [],
        };
        let uexp =
          np(
            Cast(
              p(Atom(String("a")), [Atom(String("a"))]),
              npt(
                Parens(
                  npt(
                    Prod([
                      npt(TupLabel(npt(Label("l")), npt(Atom(String)))),
                    ]),
                  ),
                ),
              )
              |> TypSlice.t_of_typ_t_parametric,
              npt(Unknown(Internal)) |> TypSlice.t_of_typ_t_parametric,
            ),
          );

        probe_test({|PROBE("a") : (a=String)|}, uexp);
      },
    ),
    test_case(
      "Evaluate probe around inferred singleton labeled tuple in pattern",
      `Quick,
      () => {
        let npp = expected_probe_pat(_, []);
        let np = expected_probe(_, []);
        let p = (p, es: list(Grammar.exp_term(unit))) =>
          expected_probe_pat(
            Probe(npp(p), {refs: []}),
            List.map(Grammar.Annotated.empty, es),
          );
        let npt = (t): Grammar.typ_t(list(Grammar.exp_t(unit))) => {
          term: t,
          annotation: [],
        };
        let uexp =
          np(
            Let(
              npp(
                Cast(
                  p(
                    Var("x"),
                    [
                      Tuple([
                        {
                          term:
                            TupLabel(
                              {
                                term: Label("l"),
                                annotation: (),
                              },
                              {
                                term: Atom(String("a")),
                                annotation: (),
                              },
                            ),
                          annotation: (),
                        },
                      ]),
                    ],
                  ),
                  npt(
                    Parens(
                      npt(
                        Prod([
                          npt(
                            TupLabel(npt(Label("l")), npt(Atom(String))),
                          ),
                        ]),
                      ),
                    ),
                  )
                  |> TypSlice.t_of_typ_t_parametric,
                  npt(Unknown(Internal)) |> TypSlice.t_of_typ_t_parametric,
                ),
              ),
              np(Atom(String("a"))),
              np(Var("x")),
            ),
          );
        probe_test({|let PROBE(x) : (a=String) = "a" in x|}, uexp);
      },
    ),
    skip_current_unboxing_error(
      "InvalidBoxSumConstructor",
      "let B : (+B( )) = ? in ?",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedListLit",
      "type g = + On in let [] = On in",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedListCons",
      "let (_:: []) = type y = + B in B in ?",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedBoolLit",
      "type y = + B(Float) in if B then false else A",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedTuple",
      "let () = type x = + A in A in ?",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedTypfun",
      "type y = + B in case true  | a => B end @<?> ",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedSumConstructor",
      "type x = + A(Float) in let A = a in 0",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedStringLit",
      {|type y = + A in ""++A|},
    ),
    skip_current_unboxing_error("InvalidBoxedIntLit", "type y = + A in -A"),
    QCheck_alcotest.to_alcotest(qcheck_evaluator_does_not_crash_test),
  ],
);
