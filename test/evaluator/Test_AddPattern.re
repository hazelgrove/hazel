open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;

// Matching

let check_match = (msg, expected, pat, exp) => {
  let equal = (r1: PatternMatch.match_result, r2: PatternMatch.match_result) =>
    switch (r1, r2) {
    | (DoesNotMatch, DoesNotMatch)
    | (IndetMatch, IndetMatch) => true
    | (Matches(env1), Matches(env2)) =>
      List.equal(
        Environment.equal_binding(Exp.fast_equal),
        List.sort(env1, ~compare=Poly.compare),
        List.sort(env2, ~compare=Poly.compare),
      )
    | _ => false
    };
  check(
    testable(PatternMatch.pp_match_result, equal),
    msg,
    expected,
    PatternMatch.matches(Util.Id.Map.empty, pat, exp).matches,
  );
};

let big = Bigint.of_int;

let matching = {
  open IdTagged.FreshGrammar;
  let m_plus = k => Exp.(bin_op(Nat(Plus), var("m"), nat(big(k))));
  [
    test_case("x + 1 against 5", `Quick, () =>
      check_match(
        "x is 4",
        Matches([("x", Exp.int(4))]),
        Pat.(add(var("x"), int(1))),
        Exp.int(5),
      )
    ),
    test_case("1 + x against 5", `Quick, () =>
      check_match(
        "x is 4",
        Matches([("x", Exp.int(4))]),
        Pat.(add(int(1), var("x"))),
        Exp.int(5),
      )
    ),
    test_case(
      "1 + 2 against 3 and 4",
      `Quick,
      () => {
        check_match(
          "3",
          Matches([]),
          Pat.(add(int(1), int(2))),
          Exp.int(3),
        );
        check_match(
          "4",
          DoesNotMatch,
          Pat.(add(int(1), int(2))),
          Exp.int(4),
        );
      },
    ),
    test_case("(1 + 2) + x against 6", `Quick, () =>
      check_match(
        "x is 3",
        Matches([("x", Exp.int(3))]),
        Pat.(add(add(int(1), int(2)), var("x"))),
        Exp.int(6),
      )
    ),
    test_case("x + 1 against an Int below 1", `Quick, () =>
      check_match(
        "x is -1",
        Matches([("x", Exp.int(-1))]),
        Pat.(add(var("x"), int(1))),
        Exp.int(0),
      )
    ),
    test_case("x + 1 against Nat 0", `Quick, () =>
      check_match(
        "no Nat is one below 0",
        DoesNotMatch,
        Pat.(add(var("x"), nat(big(1)))),
        Exp.nat(big(0)),
      )
    ),
    test_case("x + y", `Quick, () =>
      check_match(
        "two unknowns",
        IndetMatch,
        Pat.(add(var("x"), var("y"))),
        Exp.int(5),
      )
    ),
    test_case("x + 1 against an open m + 1", `Quick, () =>
      check_match(
        "x is m",
        Matches([("x", Exp.var("m"))]),
        Pat.(add(var("x"), nat(big(1)))),
        m_plus(1),
      )
    ),
    test_case("x + 1 against an open m + 3", `Quick, () =>
      check_match(
        "x is m + 2",
        Matches([("x", m_plus(2))]),
        Pat.(add(var("x"), nat(big(1)))),
        m_plus(3),
      )
    ),
    test_case("x + 2 against an open m + 1", `Quick, () =>
      check_match(
        "m may be 0",
        IndetMatch,
        Pat.(add(var("x"), nat(big(2)))),
        m_plus(1),
      )
    ),
    test_case("x + 2 against an open (m + 1) + 1", `Quick, () =>
      check_match(
        "x is m",
        Matches([("x", Exp.var("m"))]),
        Pat.(add(var("x"), nat(big(2)))),
        Exp.(bin_op(Nat(Plus), m_plus(1), nat(big(1)))),
      )
    ),
    test_case("0 against an open m + 1", `Quick, () =>
      check_match(
        "m + 1 is at least 1",
        DoesNotMatch,
        Pat.nat(big(0)),
        m_plus(1),
      )
    ),
    test_case("1 against an open m + 1", `Quick, () =>
      check_match("m may be 0", IndetMatch, Pat.nat(big(1)), m_plus(1))
    ),
  ];
};

// Evaluation

let evaluation = [
  test_case("let x + 1 = 5 in x", `Quick, () =>
    parse_and_evaluate_test("4", {|let x + 1 = 5 in x|})
  ),
  test_case("let 1 + x = 5 in x", `Quick, () =>
    parse_and_evaluate_test("4", {|let 1 + x = 5 in x|})
  ),
  test_case("let (1 + 2) + x = 10 in x", `Quick, () =>
    parse_and_evaluate_test("7", {|let (1 + 2) + x = 10 in x|})
  ),
  test_case("case on an addition pattern", `Quick, () =>
    parse_and_evaluate_test("4", {|case 5 | x + 1 => x end|})
  ),
  test_case("Nat case falls through to the base case", `Quick, () =>
    parse_and_evaluate_test(
      "true",
      {|let n : Nat = 0 in case n | m + 1 => false | _ => true end|},
    )
  ),
  test_case("Nat recursion through m + 1", `Quick, () =>
    parse_and_evaluate_test(
      "6",
      {|let len = fun (n : Nat) -> case n | 0 => 0 | m + 1 => 2 + len(m) end in len(3)|},
    )
  ),
];

// Statics

let marks_of_add_pattern = (src: string): list(Mark.t) => {
  let info_map = Test_Coverage.statics(parse_exp(src));
  Id.Map.fold(
    (_, info: Info.t, acc) =>
      switch (info) {
      | InfoPat({user_term: {term: Add(_), _}, marks, _}) => marks @ acc
      | _ => acc
      },
    info_map,
    [],
  );
};

let statics = [
  test_case("x + y needs a number", `Quick, () =>
    check(
      bool,
      "marked",
      true,
      List.mem(
        marks_of_add_pattern("let x + y = 3 in x"),
        Mark.BadAddPattern(NoConstant),
        ~equal=Poly.equal,
      ),
    )
  ),
  test_case("Float addition patterns are refused", `Quick, () =>
    check(
      bool,
      "marked",
      true,
      List.mem(
        marks_of_add_pattern("let (x + 1.) : Float = 2. in x"),
        Mark.BadAddPattern(Float),
        ~equal=Poly.equal,
      ),
    )
  ),
  test_case("x + 1 is fine", `Quick, () =>
    check(
      list(string),
      "no marks",
      [],
      List.map(marks_of_add_pattern("let x + 1 = 3 in x"), ~f=Mark.show),
    )
  ),
];

// Coverage

let inexhaustive = (witness: int) =>
  Test_Statics_Prelude.Marks([
    Mark.InexhaustiveMatch(
      Typ.temp(Unknown(Internal)),
      [],
      Grammar.Pat(IdTagged.FreshGrammar.Pat.nat(big(witness))),
    ),
  ]);

let coverage = [
  Test_Coverage.no_errors(
    "Int: x + 1 covers every Int",
    {|let n : Int = ? in case n | x + 1 => x end|},
  ),
  Test_Coverage.no_errors(
    "Nat: 0 and m + 1",
    {|let n : Nat = ? in case n | 0 => true | m + 1 => false end|},
  ),
  Test_Coverage.no_errors(
    "Nat: 0, 1 and m + 2",
    {|let n : Nat = ? in case n | 0 => true | 1 => true | m + 2 => false end|},
  ),
  Test_Coverage.no_errors(
    "Nat: a large literal costs no more than a small one",
    {|let n : Nat = ? in case n | 1000000 => true | 0 => true | m + 1 => false end|},
  ),
  Test_Coverage.has_errors(
    "Nat: m + 1 alone misses 0",
    {|let n : Nat = ? in {{{case n | m + 1 => false end}}}|},
    [inexhaustive(0)],
  ),
  Test_Coverage.has_errors(
    "Nat: 0 and m + 2 miss 1",
    {|let n : Nat = ? in {{{case n | 0 => true | m + 2 => false end}}}|},
    [inexhaustive(1)],
  ),
  Test_Coverage.has_errors(
    "Nat: m + 2 after m + 1 is redundant",
    {|let n : Nat = ? in case n | 0 => true | m + 1 => false | {{{k + 2}}} => true end|},
    [Marks([Redundant])],
  ),
  Test_Coverage.no_errors(
    "Nat: m + 2 after m + 1 is needed in a second column",
    {|let p : (Nat, Bool) = ? in case p | (0, _) => 0 | (m + 1, true) => 1 | (k + 2, false) => 2 | (1, false) => 3 end|},
  ),
];

// Induction

let fact_names = (pm: ProofMap.t, body: Proof.t): list(string) =>
  switch (ProofMap.lookup(Proof.rep_id(body), pm)) {
  | Some({ctx, _}) =>
    SemanticCtx.facts(ctx) |> List.map(~f=((name, _, _)) => name)
  | None => Alcotest.fail("no scope recorded for the case")
  };

let case_bodies = (proof: Proof.t): list(Proof.t) =>
  switch (proof.term) {
  | Induction(_, cases) => List.map(cases, ~f=snd)
  | _ => Alcotest.fail("no induction at the root")
  };

let double = {|use Nat in let double = fun (n : Nat) -> case n | 0 => 0 | m + 1 => double(m) + 2 end in |};

let induction = [
  test_case(
    "double(n) == n + n by induction on n",
    `Quick,
    () => {
      let (pm, proof) =
        run_named(
          "t",
          double
          ++ {|theorem t = forall n: Nat -> double(n) == n + n proof
  induction n
  | 0 =>
    from double(0) do step end;
    from 0 + 0 do step end;
    rewrite refl end
  | m + 1 =>
    from double(m + 1) do step end;
    from double(m) do rewrite ih_m end end;
    from m + m + 2 do algebra m + 1 + (m + 1) end end;
    rewrite refl end
  end
in t|},
        );
      check_proven("proven", pm, proof);
    },
  ),
  test_case(
    "2 * sum(n) == n * (n + 1) by induction on n",
    `Quick,
    () => {
      let (pm, proof) =
        run_named(
          "gauss",
          {|use Nat in
let sum = fun (n : Nat) -> case n | 0 => 0 | m + 1 => (m + 1) + sum(m) end in
theorem dist = forall a: Nat -> forall s: Nat -> 2 * (a + s) == 2 * a + 2 * s proof
  from 2 * (a + s) do algebra 2 * a + 2 * s end end;
  rewrite refl end
in
theorem gauss = forall n: Nat -> 2 * sum(n) == n * (n + 1) proof
  induction n
  | 0 =>
    from sum(0) do step end;
    from 2 * 0 == 0 * (0 + 1) do algebra 0 == 0 end end;
    rewrite refl end
  | m + 1 =>
    from sum(m + 1) do step end;
    from 2 * (m + 1 + sum(m)) do rewrite dist end end;
    from 2 * sum(m) do rewrite ih_m end end;
    from 2 * (m + 1) + m * (m + 1) do algebra (m + 1) * (m + 1 + 1) end end;
    rewrite refl end
  end
in gauss|},
        );
      check_proven("proven", pm, proof);
    },
  ),
  /* `sum` matches `m + 2`, which `step` has to see in the case's
     `m + 1 + 1`. */
  test_case(
    "a sum on 0, 1 and m + 2 by induction on 0, 1 and m + 1 + 1",
    `Quick,
    () => {
      let (pm, proof) =
        run_named(
          "gauss",
          {|use Nat in
let sum = fun (n : Nat) -> case n | 0 => 0 | 1 => 1 | m + 2 => (m + 2) + ((m + 1) + sum(m)) end in
theorem dist = forall a: Nat -> forall s: Nat -> 2 * (a + s) == 2 * a + 2 * s proof
  from 2 * (a + s) do algebra 2 * a + 2 * s end end;
  rewrite refl end
in
theorem gauss = forall n: Nat -> 2 * sum(n) == n * (n + 1) proof
  induction n
  | 0 =>
    from sum(0) do step end;
    from 2 * 0 == 0 * (0 + 1) do algebra 0 == 0 end end;
    rewrite refl end
  | 1 =>
    from sum(1) do step end;
    from 2 * 1 == 1 * (1 + 1) do algebra 2 == 2 end end;
    rewrite refl end
  | m + 1 + 1 =>
    from sum(m + 1 + 1) do step end;
    from 2 * (m + 2 + (m + 1 + sum(m))) do rewrite dist end end;
    from 2 * (m + 1 + sum(m)) do rewrite dist end end;
    from 2 * sum(m) do rewrite ih_m end end;
    from 2 * (m + 2) + (2 * (m + 1) + m * (m + 1)) do algebra (m + 1 + 1) * (m + 1 + 1 + 1) end end;
    rewrite refl end
  end
in gauss|},
        );
      check_proven("proven", pm, proof);
    },
  ),
  test_case(
    "m + 1 + 1 has hypotheses for m + 1 and m",
    `Quick,
    () => {
      let (pm, proof) =
        run_named(
          "t",
          {|use Nat in theorem t = forall n: Nat -> n == n proof induction n | 0 => ? | 1 => ? | m + 1 + 1 => ? end in t|},
        );
      switch (case_bodies(proof)) {
      | [_, _, step] =>
        let names = fact_names(pm, step);
        check(
          bool,
          "ih for m + 1",
          true,
          List.mem(names, "ih", ~equal=String.equal),
        );
        check(
          bool,
          "ih_m",
          true,
          List.mem(names, "ih_m", ~equal=String.equal),
        );
      | _ => Alcotest.fail("expected three cases")
      };
    },
  ),
  test_case(
    "the constant is not a hypothesis",
    `Quick,
    () => {
      let (pm, proof) =
        run_named(
          "t",
          double
          ++ {|theorem t = forall n: Nat -> double(n) == n + n proof induction n | 0 => ? | m + 1 => ? end in t|},
        );
      switch (case_bodies(proof)) {
      | [_, step] =>
        let names = fact_names(pm, step);
        check(
          bool,
          "ih_m",
          true,
          List.mem(names, "ih_m", ~equal=String.equal),
        );
        check(
          bool,
          "no ih for 1",
          false,
          List.mem(names, "ih", ~equal=String.equal),
        );
      | _ => Alcotest.fail("expected two cases")
      };
    },
  ),
  test_case(
    "Int has no inductive hypothesis",
    `Quick,
    () => {
      let (pm, proof) =
        run_named(
          "t",
          {|theorem t = forall n: Int -> n == n proof induction n | m + 1 => ? end in t|},
        );
      switch (case_bodies(proof)) {
      | [step] =>
        check(
          bool,
          "no ih_m",
          false,
          List.mem(fact_names(pm, step), "ih_m", ~equal=String.equal),
        )
      | _ => Alcotest.fail("expected one case")
      };
    },
  ),
  test_case(
    "(m + 1) in parens is not its own hypothesis",
    `Quick,
    () => {
      let (pm, proof) =
        run_named(
          "t",
          {|use Nat in theorem t = forall n: Nat -> n == 0 proof induction n | 0 => ? | (m + 1) => ? end in t|},
        );
      switch (case_bodies(proof)) {
      | [_, step] =>
        let names = fact_names(pm, step);
        check(
          bool,
          "ih_m",
          true,
          List.mem(names, "ih_m", ~equal=String.equal),
        );
        check(
          bool,
          "no ih for the case itself",
          false,
          List.mem(names, "ih", ~equal=String.equal),
        );
      | _ => Alcotest.fail("expected two cases")
      };
    },
  ),
  test_case(
    "m + 0 has no inductive hypothesis",
    `Quick,
    () => {
      let (pm, proof) =
        run_named(
          "t",
          {|use Nat in theorem t = forall n: Nat -> n == n proof induction n | m + 0 => ? end in t|},
        );
      switch (case_bodies(proof)) {
      | [step] =>
        check(
          bool,
          "no ih_m",
          false,
          List.mem(fact_names(pm, step), "ih_m", ~equal=String.equal),
        )
      | _ => Alcotest.fail("expected one case")
      };
    },
  ),
  test_case(
    "Nat induction needs a base case",
    `Quick,
    () => {
      let (pm, proof) =
        run_named(
          "t",
          double
          ++ {|theorem t = forall n: Nat -> double(n) == n + n proof induction n | m + 1 => rewrite ih_m end end in t|},
        );
      check(
        bool,
        "not exhaustive",
        true,
        Test_ProofMap.has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.InductionNotExhaustive => true
          | _ => false,
        ),
      );
    },
  ),
  /* The function needs no `use Nat`: its `m + 1` takes Nat from `n`. A
     proof step's `m + 1` has nothing to take it from, so without
     `use Nat` it is Int addition and doesn't find the goal's. */
  test_case(
    "a proof's m + 1 is Nat addition under use Nat",
    `Quick,
    () => {
      let not_found = (prefix: string) => {
        let (pm, proof) =
          run_named(
            "t",
            prefix
            ++ {|let double = fun (n : Nat) -> case n | 0 => n | m + 1 => double(m) end in theorem t = forall n: Nat -> double(n) == double(n) proof induction n | 0 => ? | m + 1 => from double(m + 1) do step end; ? end in t|},
          );
        Test_ProofMap.has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.PatternNotFound(_) => true
          | _ => false,
        );
      };
      check(bool, "found under use Nat", false, not_found("use Nat in "));
      check(bool, "not found without it", true, not_found(""));
    },
  ),
];

let tests = (
  "AddPattern",
  matching @ evaluation @ statics @ coverage @ induction,
);
