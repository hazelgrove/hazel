open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* `p where g`, a pattern that matches when `p` matches and then `g` is
 * true. `fun p where g -> e` and `forall p where g -> e` are `Fun` and
 * `Forall` of this pattern; it can appear wherever a pattern can. */

let pp_settings: Haz3lcore.ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
  use_literal_lexemes: true,
  project_tables: false,
  hole_tiles: false,
};

let print = (e: Exp.t): string =>
  Haz3lcore.Printer.of_segment(
    ~holes="?",
    Haz3lcore.ExpToSegment.exp_to_segment(~settings=pp_settings, e),
  );

let print_back = (src: string): string => print(parse_exp(src));

let statics_marks = (src: string): list(Mark.t) => {
  let (m, _) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Int)),
      parse_exp(src),
    );
  Id.Map.fold(
    (_, info: Info.t, acc) =>
      switch (info) {
      | InfoExp({marks, _})
      | InfoPat({marks, _}) => acc @ marks
      | _ => acc
      },
    m,
    [],
  );
};

let unused_vars = (src: string): list(string) => {
  let (m, _) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Int)),
      parse_exp(src),
    );
  Id.Map.fold(
    (_, info: Info.t, acc) =>
      switch (info) {
      | InfoPat({warnings, user_term: {term: Var(x), _}, _})
          when warnings != [] => [
          x,
          ...acc,
        ]
      | _ => acc
      },
    m,
    [],
  );
};

let is_inexhaustive: Mark.t => bool =
  fun
  | InexhaustiveMatch(_) => true
  | _ => false;

let first_arm_pattern = (src: string): Pat.t =>
  switch (parse_exp(src) |> Exp.term_of) {
  | Match(_, [(p, _), ..._]) => p
  | _ => Alcotest.fail("expected a case: " ++ src)
  };

let check_eval = (msg, expected, src) => {
  let result = parse_and_evaluate(src);
  check(
    bool,
    msg ++ " (got " ++ print(result) ++ ")",
    true,
    Exp.fast_equal(result, parse_and_evaluate(expected)),
  );
};

/* Strip evaluation wrappers to get at the result's head term. */
let rec unwrap = (d: Exp.t): Exp.t =>
  switch (Exp.term_of(d)) {
  | Closure(_, d1)
  | Parens(d1)
  | Asc(d1, _) => unwrap(d1)
  | _ => d
  };

/* The error is around the application or `let` the program wrote, not a
   form the evaluator built on the way: its printed form mentions
   `around`. */
let contains = (s, sub) => {
  let n = String.length(sub);
  let rec at = i =>
    i + n <= String.length(s) && (String.sub(s, i, n) == sub || at(i + 1));
  at(0);
};

let check_unsatisfied_where = (~around, msg, src) => {
  let result = parse_and_evaluate(src);
  switch (Exp.term_of(unwrap(result))) {
  | DynamicErrorHole(failed, UnsatisfiedWhere) =>
    check(
      bool,
      msg ++ ": the error is around " ++ print(failed),
      true,
      contains(print(failed), around),
    )
  | _ =>
    Alcotest.fail(
      msg ++ ": expected an UnsatisfiedWhere error, got " ++ print(result),
    )
  };
};

/* A pattern that doesn't match its value is stuck, as without guards. */
let check_stuck = (msg, src) => {
  let result = parse_and_evaluate(src);
  check(
    bool,
    msg ++ " (got " ++ print(result) ++ ")",
    false,
    switch (Exp.term_of(unwrap(result))) {
    | DynamicErrorHole(_)
    | Atom(_) => true
    | _ => false
    },
  );
};

let parsing = [
  test_case("fun p where g -> e is Fun of Where", `Quick, () =>
    switch (parse_exp("fun x where x != 0 -> 1 / x") |> Exp.term_of) {
    | Fun({term: Where(p, g), _}, _, _, _) =>
      check(bool, "pattern", true, Pat.fast_equal(p, Pat.fresh(Var("x"))));
      check(bool, "guard", true, Exp.fast_equal(g, parse_exp("x != 0")));
    | _ => Alcotest.fail("expected Fun(Where(x, x != 0), _)")
    }
  ),
  test_case("forall p where g -> e is Forall of Where", `Quick, () =>
    switch (parse_exp("forall x where x != 0 -> x / x == 1") |> Exp.term_of) {
    | Forall({term: Where(_), _}, _) => ()
    | _ => Alcotest.fail("expected Forall(Where(_), _)")
    }
  ),
  test_case("let p where g = e is Let of Where", `Quick, () =>
    switch (parse_exp("let x where x != 0 = 1 in x") |> Exp.term_of) {
    | Let({term: Where(_), _}, _, _) => ()
    | _ => Alcotest.fail("expected Let(Where(_), _, _)")
    }
  ),
  test_case("where is looser than ::", `Quick, () =>
    switch (
      first_arm_pattern("case xs | h :: t where h != 0 => 1 | _ => 2 end")
      |> Pat.term_of
    ) {
    | Where({term: Cons(_), _}, _) => ()
    | _ => Alcotest.fail("expected Where(h :: t, _)")
    }
  ),
  test_case("where is tighter than ,", `Quick, () =>
    switch (
      first_arm_pattern("case v | (x where x > 0, y) => 1 | _ => 2 end")
      |> Pat.term_of
    ) {
    | Parens({term: Tuple([{term: Where(_), _}, {term: Var("y"), _}]), _}) =>
      ()
    | _ =>
      Alcotest.fail(
        "expected (Where(x, _), y), got "
        ++ print_back("case v | (x where x > 0, y) => 1 | _ => 2 end"),
      )
    }
  ),
  /* The remolder has to switch to Exp sort at the guard, or `p(x)` reads
     as a constructor-application pattern. */
  test_case("the guard is an expression", `Quick, () =>
    switch (parse_exp("fun x where p(x) -> x") |> Exp.term_of) {
    | Fun({term: Where(_, g), _}, _, _, _) =>
      check(
        bool,
        "guard is the call p(x)",
        true,
        Exp.fast_equal(g, parse_exp("p(x)")),
      )
    | _ => Alcotest.fail("expected Fun(Where(_), _)")
    }
  ),
  test_case("an ascribed forall binder's guard is an expression", `Quick, () =>
    switch (
      parse_exp("forall e0: Int where p(e0) -> e0 == e0") |> Exp.term_of
    ) {
    | Forall({term: Where(_, g), _}, _) =>
      check(
        bool,
        "guard is the call p(e0)",
        true,
        Exp.fast_equal(g, parse_exp("p(e0)")),
      )
    | _ => Alcotest.fail("expected Forall(Where(_), _)")
    }
  ),
  test_case("round-trips through the printer", `Quick, () =>
    List.iter(
      src => check(string, src, src, print_back(src)),
      [
        "let x where x != 0 = 1 in x",
        "let (x where x != 0) = 1 in x",
        "fun x where x != 0 -> 1 / x",
        "forall x where x != 0 -> x / x == 1",
        "let (x where x > 0, y) = (1, 2) in x",
      ],
    )
  ),
];

let statics = [
  test_case("a well-typed guard has no marks", `Quick, () =>
    check(
      int,
      "marks",
      0,
      List.length(statics_marks("let (x where x != 0) = 1 in x")),
    )
  ),
  test_case("a non-boolean guard is marked", `Quick, () =>
    check(
      bool,
      "marked",
      true,
      statics_marks("let (x where x + 1) = 1 in x") != [],
    )
  ),
  test_case("a guard sees the bindings to its left", `Quick, () =>
    check(
      int,
      "marks",
      0,
      List.length(
        statics_marks("case (1, 2) | (y, x where x > y) => x | _ => 0 end"),
      ),
    )
  ),
  test_case(
    "a variable used only in a guard is used",
    `Quick,
    () => {
      check(
        list(string),
        "pattern variable",
        [],
        unused_vars("let f = fun x where x > 0 -> 1 in f(1)"),
      );
      check(
        list(string),
        "left sibling",
        [],
        unused_vars("case (1, 2) | (y, x where x > y) => x | _ => 0 end"),
      );
      check(
        list(string),
        "outer variable",
        [],
        unused_vars("let y = 1 in case 2 | x where x > y => x | _ => 0 end"),
      );
    },
  ),
];

let dynamics = [
  test_case("let binds when the guard holds", `Quick, () =>
    check_eval("let", "1", "let x where x != 0 = 1 in x")
  ),
  test_case(
    "case guard filters",
    `Quick,
    () => {
      check_eval(
        "true: arm taken",
        "1",
        "case 3 | x where x != 0 => 1 | _ => 2 end",
      );
      check_eval(
        "false: next arm",
        "2",
        "case 0 | x where x != 0 => 1 | _ => 2 end",
      );
      check_eval(
        "cons arm taken",
        "1",
        "case [3, 4] | h :: t where h != 0 => 1 | _ => 2 end",
      );
      check_eval(
        "cons arm skipped",
        "2",
        "case [0, 4] | h :: t where h != 0 => 1 | _ => 2 end",
      );
    },
  ),
  test_case(
    "a nested guard filters",
    `Quick,
    () => {
      check_eval(
        "taken",
        "4",
        "case (1, 2) | (x where x > 0, y where y > x) => x + y + 1 | _ => 0 end",
      );
      check_eval(
        "first fails",
        "0",
        "case (0, 2) | (x where x > 0, y where y > x) => 1 | _ => 0 end",
      );
      check_eval(
        "second fails",
        "0",
        "case (1, 1) | (x where x > 0, y where y > x) => 1 | _ => 0 end",
      );
      check_eval(
        "structure after the guard fails",
        "0",
        "case (1, 3) | (x where x > 0, 2) => 1 | _ => 0 end",
      );
    },
  ),
  /* Bodies and later arms see the enclosing scope, not the failed arm's
     bindings. */
  test_case("a failed arm's bindings don't leak", `Quick, () =>
    check_eval(
      "fallback sees the outer x",
      "1",
      "let x = 1 in case (5, 0) | (x where x > 9, y) => 0 | _ => x end",
    )
  ),
  test_case(
    "a guard sees the bindings to its left",
    `Quick,
    () => {
      check_eval(
        "a sibling to the left shadows the outer y",
        "1",
        "let y = 10 in case (0, 5) | (y, x where x > y) => 1 | _ => 2 end",
      );
      check_eval(
        "a sibling to the right doesn't",
        "2",
        "let y = 10 in case (5, 0) | (x where x > y, y) => 1 | _ => 2 end",
      );
    },
  ),
  /* The guard runs before the structure to its right is looked at: here
     it divides by zero, so the case is stuck rather than falling
     through to the catch-all. */
  test_case(
    "guards run left to right, interleaved with matching",
    `Quick,
    () => {
      let result =
        parse_and_evaluate(
          "case (0, 1) | (x where 1 / x == 1, 2) => 1 | _ => 3 end",
        );
      check(
        bool,
        "not the catch-all (got " ++ print(result) ++ ")",
        false,
        Exp.fast_equal(result, parse_and_evaluate("3")),
      );
    },
  ),
  test_case(
    "an off-guard fun application is an error",
    `Quick,
    () => {
      check_eval("on-guard", "50", "(fun x where x != 0 -> 100 / x)(2)");
      check_unsatisfied_where(
        ~around="x where x != 0",
        "off-guard",
        "(fun x where x != 0 -> 100 / x)(0)",
      );
      check_unsatisfied_where(
        ~around="x where x > 0, y where y > 0",
        "second guard",
        "(fun (x where x > 0, y where y > 0) -> x + y)((1, 0))",
      );
    },
  ),
  test_case(
    "an off-guard let is an error",
    `Quick,
    () => {
      check_unsatisfied_where(
        ~around="let x where x != 0",
        "off-guard",
        "let x where x != 0 = 0 in 100 / x",
      );
      check_unsatisfied_where(
        ~around="x where x > 0, y where y > 0",
        "second guard",
        "let (x where x > 0, y where y > 0) = (1, 0) in x + y",
      );
    },
  ),
  test_case(
    "a structural mismatch after a passing guard is stuck",
    `Quick,
    () => {
      check_stuck("fun", "(fun (x where x > 0, 2) -> x)((1, 3))");
      check_stuck("let", "let (x where x > 0, 2) = (1, 3) in x");
    },
  ),
  test_case(
    "parenthesized and bare binders agree",
    `Quick,
    () => {
      check(
        bool,
        "same term",
        true,
        Exp.fast_equal(
          parse_exp("fun x where x != 0 -> 100 / x"),
          parse_exp("fun (x where x != 0) -> 100 / x"),
        ),
      );
      check_eval("on-guard", "50", "(fun (x where x != 0) -> 100 / x)(2)");
    },
  ),
  /* In the stepper a guarded arm steps to an ordinary `if`, then to the
     arm's body or the later arms. */
  test_case(
    "a guarded case steps to an if",
    `Quick,
    () => {
      let trace = src => {
        let rec go = (n, e) =>
          n == 0
            ? []
            : (
              switch (single_step(e)) {
              | Some(e') => [print(e'), ...go(n - 1, e')]
              | None => []
              }
            );
        go(10, elaborate(parse_exp(src)));
      };
      check(
        list(string),
        "taken",
        [
          "if 3 > 0 then 3 else case 3 end",
          "if true then 3 else case 3 end",
          "3",
        ],
        trace("case 3 | x where x > 0 => x end"),
      );
      check(
        list(string),
        "falls through",
        [
          "if 0 > 0 then 0 else case 0 | _ => 1 end",
          "if false then 0 else case 0 | _ => 1 end",
          "case 0 | _ => 1 end",
          "1",
        ],
        trace("case 0 | x where x > 0 => x | _ => 1 end"),
      );
    },
  ),
];

let coverage = [
  test_case("guarded arm plus catch-all is exhaustive", `Quick, () =>
    check(
      bool,
      "no mark",
      false,
      statics_marks("case 0 | x where x != 0 => 1 | _ => 2 end")
      |> List.exists(is_inexhaustive),
    )
  ),
  test_case("a guarded arm alone is inexhaustive", `Quick, () =>
    check(
      bool,
      "marked",
      true,
      statics_marks("case 0 | x where x != 0 => 1 end")
      |> List.exists(is_inexhaustive),
    )
  ),
  test_case("a guard nested in a row still makes it guarded", `Quick, () =>
    check(
      bool,
      "marked",
      true,
      statics_marks("case (0, 1) | (x where x != 0, y) => 1 end")
      |> List.exists(is_inexhaustive),
    )
  ),
  /* Deciding that `x > 0 || x <= 0` is complete would need a solver. */
  test_case("complementary guards are still inexhaustive", `Quick, () =>
    check(
      bool,
      "marked",
      true,
      statics_marks("case 0 | x where x > 0 => 1 | y where y <= 0 => 2 end")
      |> List.exists(is_inexhaustive),
    )
  ),
];

/* In a proof, a guard is a citable `where` fact in the scope its pattern
   binds into. */

let obligations = (src: string): (ProofMap.t, Proof.t, list(Obligation.t)) => {
  let (state, _, elab) = src |> parse_exp |> Test_ProofMap.eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = Test_ProofMap.proof_of(elab);
  (pm, proof, ProofMap.obligations_of_proof(pm, proof));
};

let where_fact = (e: Ctx.entry): option(Exp.t) =>
  switch (e) {
  | Ctx.TheoremEntry({name: "where", prop: Some(p), _}) => Some(p)
  | _ => None
  };

let is_remote: Obligation.discharge => bool =
  fun
  | Obligation.Remote(_) => true
  | _ => false;

/* Obligations whose bindings carry the `where` fact `fact`. */
let with_where_fact = (fact: string, obs: list(Obligation.t)) =>
  List.filter(
    (ob: Obligation.t) =>
      List.exists(
        w => Exp.fast_equal(w, parse_exp(fact)),
        List.filter_map(where_fact, ob.bindings),
      ),
    obs,
  );

/* Both arms owe `w != 0` (from `8 / w`). The first arm's guard discharges
   it. The second arm is reached only when the first failed, so it carries
   the first guard's negation, and its obligation stays pending. */
let induction_two_arms = {|theorem t = forall z -> 8 / z == 8 / z proof induction z | w where w != 0 => from 8 / w == 8 / w do axiom refl_eq end end | w => from 8 / w == 8 / w do axiom refl_eq end end end in t|};

let proofs = [
  test_case(
    "a forall's guard discharges its body's condition",
    `Quick,
    () => {
      let src = {|theorem t = forall z where z != 0 -> 8 / z == 8 / z proof from 8 / z == 8 / z do axiom refl_eq end end in t|};
      let (pm, proof, obs) = obligations(src);
      switch (obs) {
      | [ob] =>
        check(
          bool,
          "discharged against the guard (got "
          ++ Obligation.show_discharge(ob.discharge)
          ++ ")",
          true,
          is_remote(ob.discharge),
        )
      | _ =>
        Alcotest.fail(
          "expected one condition, got " ++ string_of_int(List.length(obs)),
        )
      };
      check(
        string,
        "proven",
        ProofMap.show_full_status(ProofMap.Proven),
        ProofMap.show_full_status(ProofMap.full_status_of_proof(pm, proof)),
      );
    },
  ),
  test_case(
    "an induction arm's guard is a where fact",
    `Quick,
    () => {
      let (_, _, obs) = obligations(induction_two_arms);
      check(int, "one condition per arm", 2, List.length(obs));
      let positive = with_where_fact("w != 0", obs);
      check(int, "one arm carries `w != 0`", 1, List.length(positive));
      check(
        bool,
        "and discharges its condition against it",
        true,
        List.for_all(
          (ob: Obligation.t) => is_remote(ob.discharge),
          positive,
        ),
      );
    },
  ),
  test_case(
    "a later overlapping arm gets the negated guard",
    `Quick,
    () => {
      let (_, _, obs) = obligations(induction_two_arms);
      let negated = with_where_fact("!(w != 0)", obs);
      check(int, "one arm carries `!(w != 0)`", 1, List.length(negated));
      check(
        bool,
        "and its condition stays pending",
        true,
        List.for_all(
          (ob: Obligation.t) => !is_remote(ob.discharge),
          negated,
        ),
      );
    },
  ),
  /* A `| Z =>` arm must not import an `| S(m) where P =>` arm's negated
     guard: `P` may not even mention its variables. */
  test_case(
    "constructor-disjoint arms don't overlap",
    `Quick,
    () => {
      let overlap = (p1, p2) =>
        ProofCheck.Where.pats_overlap(
          first_arm_pattern("case ? | " ++ p1 ++ " => 1 end"),
          first_arm_pattern("case ? | " ++ p2 ++ " => 1 end"),
        );
      check(bool, "[] and h :: t", false, overlap("h :: t", "[]"));
      check(bool, "true and false", false, overlap("true", "false"));
      check(
        bool,
        "a variable overlaps everything",
        true,
        overlap("w", "[]"),
      );
      check(bool, "same constructor", true, overlap("h :: t", "a :: b"));
    },
  ),
];

let tests = [
  ("WherePat.Parsing", parsing),
  ("WherePat.Statics", statics),
  ("WherePat.Dynamics", dynamics),
  ("WherePat.Coverage", coverage),
  ("WherePat.Proofs", proofs),
];
