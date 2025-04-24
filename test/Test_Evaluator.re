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

let test_int = () =>
  evaluation_test(
    "8",
    Exp.fresh(Atom(Int(Bigint.of_int(8)))),
    Exp.fresh(Atom(Int(Bigint.of_int(8)))),
  );

let test_sum = () =>
  evaluation_test(
    "4 + 5",
    Exp.fresh(Atom(Int(Bigint.of_int(9)))),
    Exp.fresh(
      BinOp(
        Int(Plus),
        Exp.fresh(Atom(Int(Bigint.of_int(4)))),
        Exp.fresh(Atom(Int(Bigint.of_int(5)))),
      ),
    ),
  );

let test_labeled_tuple_projection = () =>
  evaluation_test(
    "(a=1, b=2, c=?).a",
    Exp.fresh(Atom(Int(Bigint.of_int(1)))),
    Exp.fresh(
      Dot(
        Exp.fresh(
          Tuple([
            Exp.fresh(
              TupLabel(
                Exp.fresh(Label("a")),
                Exp.fresh(Atom(Int(Bigint.of_int(1)))),
              ),
            ),
            Exp.fresh(
              TupLabel(
                Exp.fresh(Label("b")),
                Exp.fresh(Atom(Int(Bigint.of_int(2)))),
              ),
            ),
            Exp.fresh(
              TupLabel(Exp.fresh(Label("c")), Exp.fresh(EmptyHole)),
            ),
          ]),
        ),
        Exp.fresh(Label("a")) // This is a var now for parsing reasons
      ),
    ),
  );

let test_function_application = () =>
  evaluation_test(
    "float_of_int(1)",
    Exp.fresh(Atom(Float(1.0))),
    Exp.fresh(
      Ap(
        Forward,
        Exp.fresh(Var("float_of_int")),
        Exp.fresh(Atom(Int(Bigint.of_int(1)))),
      ),
    ),
  );

let test_function_deferral = () =>
  evaluation_test(
    "string_sub(\"hello\", 1, _)(2)",
    Exp.fresh(Atom(String("el"))),
    Exp.fresh(
      Ap(
        Forward,
        Exp.fresh(
          DeferredAp(
            Exp.fresh(Var("string_sub")),
            [
              Exp.fresh(Atom(String("hello"))),
              Exp.fresh(Atom(Int(Bigint.of_int(1)))),
              Exp.fresh(Deferral(InAp)),
            ],
          ),
        ),
        Exp.fresh(Atom(Int(Bigint.of_int(2)))),
      ),
    ),
  );

let test_ap_of_hole_deferral = () =>
  evaluation_test(
    "?(_, _, 3)(1., true)",
    Exp.fresh(
      Ap(
        Forward,
        Exp.fresh(
          Cast(
            Exp.fresh(EmptyHole),
            Typ.fresh(Unknown(Internal)),
            Typ.fresh(
              Arrow(
                Typ.fresh(Unknown(Internal)),
                Typ.fresh(Unknown(Internal)),
              ),
            ),
          ),
        ),
        Exp.fresh(
          Cast(
            Exp.fresh(
              Tuple([
                Exp.fresh(
                  Cast(
                    Exp.fresh(Atom(Float(1.))),
                    Typ.fresh(Atom(Float)),
                    Typ.fresh(Unknown(Internal)),
                  ),
                ),
                Exp.fresh(
                  Cast(
                    Exp.fresh(Atom(Bool(true))),
                    Typ.fresh(Atom(Bool)),
                    Typ.fresh(Unknown(Internal)),
                  ),
                ),
                Exp.fresh(
                  Cast(
                    Exp.fresh(Atom(Int(Bigint.of_int(3)))),
                    Typ.fresh(Atom(Int)),
                    Typ.fresh(Unknown(Internal)),
                  ),
                ),
              ]),
            ),
            Typ.fresh(
              Prod([
                Typ.fresh(Unknown(Internal)),
                Typ.fresh(Unknown(Internal)),
                Typ.fresh(Unknown(Internal)),
              ]),
            ),
            Typ.fresh(Unknown(Internal)),
          ),
        ),
      ),
    ),
    Exp.fresh(
      Ap(
        Forward,
        Exp.fresh(
          DeferredAp(
            Exp.fresh(
              Cast(
                Exp.fresh(
                  Cast(
                    Exp.fresh(EmptyHole),
                    Typ.fresh(Unknown(Internal)),
                    Typ.fresh(
                      Arrow(
                        Typ.fresh(Unknown(Internal)),
                        Typ.fresh(Unknown(Internal)),
                      ),
                    ),
                  ),
                ),
                Typ.fresh(
                  Arrow(
                    Typ.fresh(Unknown(Internal)),
                    Typ.fresh(Unknown(Internal)),
                  ),
                ),
                Typ.fresh(
                  Arrow(
                    Typ.fresh(
                      Prod([
                        Typ.fresh(Unknown(Internal)),
                        Typ.fresh(Unknown(Internal)),
                        Typ.fresh(Unknown(Internal)),
                      ]),
                    ),
                    Typ.fresh(Unknown(Internal)),
                  ),
                ),
              ),
            ),
            [
              Exp.fresh(Deferral(InAp)),
              Exp.fresh(Deferral(InAp)),
              Exp.fresh(
                Cast(
                  Exp.fresh(Atom(Int(Bigint.of_int(3)))),
                  Typ.fresh(Atom(Int)),
                  Typ.fresh(Unknown(Internal)),
                ),
              ),
            ],
          ),
        ),
        Exp.fresh(
          Tuple([
            Exp.fresh(
              Cast(
                Exp.fresh(Atom(Float(1.))),
                Typ.fresh(Atom(Float)),
                Typ.fresh(Unknown(Internal)),
              ),
            ),
            Exp.fresh(
              Cast(
                Exp.fresh(Atom(Bool(true))),
                Typ.fresh(Atom(Bool)),
                Typ.fresh(Unknown(Internal)),
              ),
            ),
          ]),
        ),
      ),
    ),
  );

let test_multi_arg_builtin_cast = () =>
  evaluation_test(
    "string_compare((\"Hello\", \"World\"):(?, ?))",
    Exp.fresh(Atom(Int(Bigint.of_int(-1)))),
    Exp.fresh(
      Ap(
        Forward,
        Exp.fresh(BuiltinFun("string_compare")),
        Exp.fresh(
          Cast(
            Exp.fresh(
              Tuple([
                Exp.fresh(
                  Cast(
                    Exp.fresh(Atom(String("Hello"))),
                    Typ.fresh(Atom(String)),
                    Typ.fresh(Unknown(Internal)),
                  ),
                ),
                Exp.fresh(
                  Cast(
                    Exp.fresh(Atom(String("World"))),
                    Typ.fresh(Atom(String)),
                    Typ.fresh(Unknown(Internal)),
                  ),
                ),
              ]),
            ),
            Typ.fresh(
              Prod([
                Typ.fresh(Unknown(Internal)),
                Typ.fresh(Unknown(Internal)),
              ]),
            ),
            Typ.fresh(
              Prod([Typ.fresh(Atom(String)), Typ.fresh(Atom(String))]),
            ),
          ),
        ),
      ),
    ),
  );

let test_variable_capture = () =>
  evaluation_test(
    {|let u = 5 in let f = fun () -> u in let u = 3 in f()|},
    Exp.fresh(Atom(Int(Bigint.of_int(5)))),
    Exp.fresh(
      Let(
        Pat.fresh(Var("u")),
        Exp.fresh(Atom(Int(Bigint.of_int(5)))),
        Exp.fresh(
          Let(
            Pat.fresh(Var("f")),
            Exp.fresh(
              Fun(Pat.fresh(Tuple([])), Exp.fresh(Var("u")), None, None),
            ),
            Exp.fresh(
              Let(
                Pat.fresh(Var("u")),
                Exp.fresh(Atom(Int(Bigint.of_int(3)))),
                Exp.fresh(
                  Ap(Forward, Exp.fresh(Var("f")), Exp.fresh(Tuple([]))),
                ),
              ),
            ),
          ),
        ),
      ),
    ),
  );

let test_unbound_lookup = () =>
  evaluation_test(
    "(fun x -> x)(x)",
    Exp.fresh(Var("x")),
    Exp.fresh(
      Ap(
        Forward,
        Exp.fresh(
          Fun(Pat.fresh(Var("x")), Exp.fresh(Var("x")), None, None),
        ),
        Exp.fresh(Var("x")),
      ),
    ),
  );

let test_unevaluated_if = () =>
  evaluation_test(
    "let x = 5 in if ? then x else x",
    Exp.fresh(
      If(
        Exp.fresh(EmptyHole),
        Exp.fresh(Atom(Int(Bigint.of_int(5)))),
        Exp.fresh(Atom(Int(Bigint.of_int(5)))),
      ),
    ),
    Exp.fresh(
      Let(
        Pat.fresh(Var("x")),
        Exp.fresh(Atom(Int(Bigint.of_int(5)))),
        Exp.fresh(
          If(
            Exp.fresh(EmptyHole),
            Exp.fresh(Var("x")),
            Exp.fresh(Var("x")),
          ),
        ),
      ),
    ),
  );

let test_invalid_constructor_match = () => {
  let invalid_constructor_match =
    elaborate(
      Exp.fresh(
        Let(
          Pat.fresh(Constructor("T", Some(None))),
          Exp.fresh(Atom(Int(Bigint.of_int(1)))),
          Exp.fresh(EmptyHole),
        ),
      ),
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
    Exp.fresh(Atom(Int(Bigint.of_int(1)))),
    Exp.fresh(
      Ap(
        Forward,
        Exp.fresh(
          TypAp(
            Exp.fresh(
              TypFun(
                TPat.fresh(Var("T")),
                Exp.fresh(
                  Fun(
                    Pat.fresh(Var("x")),
                    Exp.fresh(Atom(Int(Bigint.of_int(1)))),
                    None,
                    None,
                  ),
                ),
                None,
              ),
            ),
            Typ.fresh(Atom(Int)),
          ),
        ),
        Exp.fresh(Atom(Int(Bigint.of_int(2)))),
      ),
    ),
  );

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
      evaluation_test(
        "-8",
        Exp.fresh(Atom(Int(Bigint.of_int(-8)))),
        Exp.fresh(
          UnOp(Int(Minus), Exp.fresh(Atom(Int(Bigint.of_int(8))))),
        ),
      )
    ),
    test_case("Simple probe", `Quick, () => {
      probe_test(
        "let x = 1 + 2 in 4",
        expected_probe(
          Let(
            expected_probe_pat(Var("x"), []),
            expected_probe(
              Probe(
                expected_probe(
                  BinOp(
                    Int(Plus),
                    expected_probe(Atom(Int(Bigint.of_int(1))), []),
                    expected_probe(Atom(Int(Bigint.of_int(2))), []),
                  ),
                  [],
                ),
                {refs: []},
              ),
              [probed_value(Atom(Int(Bigint.of_int(3))))],
            ),
            expected_probe(Var("x"), []),
          ),
          [],
        ),
      )
    }),
    test_case(
      "Probes in factorial function",
      `Quick,
      () => {
        // TODO Better helpers. We really need a way to build these with a builder for the "free element".
        let npp = expected_probe_pat(_, []);
        let np = expected_probe(_, []);
        let p = (p, es: list(Grammar.exp_term(unit))) =>
          expected_probe(
            Probe(np(p), {refs: []}),
            List.map(Grammar.Annotated.empty, es),
          );
        let pp = (p, es: list(Grammar.exp_term(unit))) =>
          expected_probe_pat(
            Probe(npp(p), {refs: []}),
            List.map(Grammar.Annotated.empty, es),
          );

        probe_test(
          {|let fact = fun x ->
           case x
             | 1 => 1
             | _ =>
             let r = fact(x-1)
             in x*r
         end in fact(5)|},
          np(
            Let(
              npp(Var("fact")),
              np(
                Fun(
                  pp(
                    Var("x"),
                    [
                      Atom(Int(Bigint.of_int(5))),
                      Atom(Int(Bigint.of_int(4))),
                      Atom(Int(Bigint.of_int(3))),
                      Atom(Int(Bigint.of_int(2))),
                      Atom(Int(Bigint.of_int(1))),
                    ],
                  ),
                  np(
                    Match(
                      p(
                        Var("x"),
                        [
                          Atom(Int(Bigint.of_int(5))),
                          Atom(Int(Bigint.of_int(4))),
                          Atom(Int(Bigint.of_int(3))),
                          Atom(Int(Bigint.of_int(2))),
                          Atom(Int(Bigint.of_int(1))),
                        ],
                      ),
                      [
                        (
                          npp(Atom(Int(Bigint.of_int(1)))),
                          p(
                            Atom(Int(Bigint.of_int(1))),
                            [Atom(Int(Bigint.of_int(1)))],
                          ),
                        ),
                        (
                          npp(Wild),
                          np(
                            Let(
                              npp(Var("r")),
                              p(
                                Ap(
                                  Forward,
                                  np(Var("fact")),
                                  np(
                                    BinOp(
                                      Int(Minus),
                                      np(Var("x")),
                                      np(Atom(Int(Bigint.of_int(1)))),
                                    ),
                                  ),
                                ),
                                [
                                  Atom(Int(Bigint.of_int(1))),
                                  Atom(Int(Bigint.of_int(2))),
                                  Atom(Int(Bigint.of_int(6))),
                                  Atom(Int(Bigint.of_int(24))),
                                ],
                              ),
                              p(
                                BinOp(
                                  Int(Times),
                                  np(Var("x")),
                                  np(Var("r")),
                                ),
                                [
                                  Atom(Int(Bigint.of_int(2))),
                                  Atom(Int(Bigint.of_int(6))),
                                  Atom(Int(Bigint.of_int(24))),
                                  Atom(Int(Bigint.of_int(120))),
                                ],
                              ),
                            ),
                          ),
                        ),
                      ],
                    ),
                  ),
                  None,
                  None,
                ),
              ),
              np(
                Ap(
                  Forward,
                  np(Var("fact")),
                  np(Atom(Int(Bigint.of_int(5)))),
                ),
              ),
            ),
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
                  ),
                  npt(Unknown(Internal)),
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
              ),
              npt(Unknown(Internal)),
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
                  ),
                  npt(Unknown(Internal)),
                ),
              ),
              np(Atom(String("a"))),
              np(Var("x")),
            ),
          );
        probe_test({|let PROBE(x) : (a=String) = "a" in x|}, uexp);
      },
    ),
  ],
);
