open Alcotest;
open Language;
open Test_Evaluator_Prelude;
let evaluate_probes = unevaluated =>
  unevaluated
  |> Evaluator.evaluate(~env=Builtins.env_init)
  |> snd
  |> EvaluatorState.get_probes;

module PGrammar =
  Grammar.Factory({
    type t = list(Grammar.exp_t(unit));
    let default_value = (): list(Grammar.exp_t(unit)) => [];
  });

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

let tests = (
  "Evaluator.Probes",
  [
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
        open PGrammar; // TODO Better helpers. We really need a way to build these with a builder for the "free element".

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
