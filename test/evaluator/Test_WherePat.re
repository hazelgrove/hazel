open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* `where` as a PATTERN form: `Pat.Where(p, g)`
 * (docs/prover-obligations.md §2.2).
 *
 * `where` on a pattern, on a `fun`, and on a `case`/`induction` arm are
 * ONE construct with two faces:
 *   - dynamics: `p where g` matches iff `p` matches and `g` holds under
 *     its bindings (PatternMatch.re). Guard false => DoesNotMatch, so a
 *     case falls through to the next arm and an off-guard application is
 *     Indet (the body never runs).
 *   - proofs: inside the body you reach by matching, `g` is a citable
 *     `where` fact (ProofCheck.add_where_facts). The runtime filter is
 *     what makes that sound.
 *
 * The dedicated `fun p where g -> e` (FunWhere) and
 * `forall p where g -> e` (ForallWhere) tiles are UNCHANGED and must
 * keep parsing and proving exactly as before; see
 * `test_fun_where_tile_unchanged` / `test_forall_where_tile_unchanged`.
 *
 * Reuses the Test_ProofMap / Test_Definedness harnesses. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of = Test_ProofMap.proof_of;
let statics_and_elab = Test_ProofMap.statics_and_elab;

let obligations = (src: string): (ProofMap.t, Proof.t, list(Obligation.t)) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  (pm, proof, ProofMap.obligations_of_proof(pm, proof));
};

let check_proven = (msg: string, src: string) => {
  let (pm, proof, _) = obligations(src);
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProofMap.Proven => ()
  | other =>
    Alcotest.fail(
      msg
      ++ ": expected fully Proven, got "
      ++ ProofMap.show_full_status(other),
    )
  };
};

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
};

let print_back = (src: string): string =>
  Haz3lcore.Printer.of_segment(
    ~holes="?",
    Haz3lcore.ExpToSegment.exp_to_segment(
      ~settings=pp_settings,
      parse_exp(src),
    ),
  );

/* Marks recorded by statics anywhere in the program. */
let statics_marks = (src: string): list(Mark.t) => {
  let (m, _) = statics_and_elab(parse_exp(src));
  Id.Map.fold(
    (_, info: Info.t, acc) =>
      switch (info) {
      | Info.InfoExp({marks, _}) => acc @ marks
      | Info.InfoPat({marks, _}) => acc @ marks
      | _ => acc
      },
    m,
    [],
  );
};

let is_inexhaustive: Mark.t => bool =
  fun
  | Mark.InexhaustiveMatch(_) => true
  | _ => false;

/* ===== 1. The dedicated tiles are untouched ========================= */

/* `fun x where g -> e` still parses to FunWhere, NOT to
 * `Fun(Where(x, g), e)`. This is the fork the infix pattern `where`
 * could have stolen. */
let test_fun_where_tile_unchanged = () =>
  switch (parse_exp("fun x where x != 0 -> 1 / x") |> Exp.term_of) {
  | FunWhere(p, g, _) =>
    Alcotest.check(
      Alcotest.bool,
      "FunWhere's own pattern carries NO Where node",
      false,
      Pat.is_guarded(p),
    );
    Alcotest.check(
      Alcotest.bool,
      "guard is the boolean the user wrote",
      true,
      Exp.fast_equal(g, parse_exp("x != 0")),
    );
  | other =>
    Alcotest.fail(
      "`fun x where g -> e` no longer parses as FunWhere: "
      ++ Exp.show(Exp.fresh(other)),
    )
  };

let test_forall_where_tile_unchanged = () =>
  switch (parse_exp("forall x where x != 0 -> x / x == 1") |> Exp.term_of) {
  | ForallWhere(p, _, _) =>
    Alcotest.check(
      Alcotest.bool,
      "ForallWhere's own pattern carries NO Where node",
      false,
      Pat.is_guarded(p),
    )
  | other =>
    Alcotest.fail(
      "`forall x where g -> e` no longer parses as ForallWhere: "
      ++ Exp.show(Exp.fresh(other)),
    )
  };

/* Copied from Test_Definedness.test_where_covers_scanned_condition: the
 * ForallWhere restriction still discharges the scanned denominator
 * condition Remotely and the theorem is still fully Proven. */
let test_forall_where_still_proves = () => {
  let src = {|theorem t = forall z where z != 0 -> 8 / z == 8 / z proof axiom refl_eq at 0 on 8 / z == 8 / z end in t|};
  let (pm, proof, obs) = obligations(src);
  Alcotest.check(Alcotest.int, "one scanned condition", 1, List.length(obs));
  switch (obs) {
  | [ob] =>
    Alcotest.check(
      Alcotest.bool,
      "discharged Remote against the where-restriction — got: "
      ++ Obligation.show_discharge(ob.discharge),
      true,
      switch (ob.discharge) {
      | Obligation.Remote(_) => true
      | _ => false
      },
    )
  | _ => ()
  };
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProofMap.Proven => ()
  | other =>
    Alcotest.fail(
      "expected fully Proven, got: " ++ ProofMap.show_full_status(other),
    )
  };
};

/* ===== 2. Parsing / round-trip ====================================== */

let test_parses_as_where_pat = () =>
  switch (parse_exp("let x where x != 0 = 1 in x") |> Exp.term_of) {
  | Let(p, _, _) =>
    switch (p |> Pat.term_of) {
    | Where(inner, g) =>
      Alcotest.check(
        Alcotest.bool,
        "binds like the bare pattern",
        true,
        Pat.fast_equal(inner, Pat.fresh(Var("x"))),
      );
      Alcotest.check(
        Alcotest.bool,
        "guard is the boolean the user wrote",
        true,
        Exp.fast_equal(g, parse_exp("x != 0")),
      );
    | other =>
      Alcotest.fail("not a Where pattern: " ++ Pat.show(Pat.fresh(other)))
    }
  | _ => Alcotest.fail("expected a Let")
  };

/* Precedence: `where` is looser than `::`, so the whole cons pattern is
 * guarded (Precedence.pat_where). */
let test_precedence_looser_than_cons = () =>
  switch (
    parse_exp("case xs | h :: t where h != 0 => 1 | _ => 2 end")
    |> Exp.term_of
  ) {
  | Match(_, [(p, _), ..._]) =>
    switch (p |> Pat.term_of) {
    | Where({term: Cons(_, _), _}, g) =>
      Alcotest.check(
        Alcotest.bool,
        "guard is the whole boolean",
        true,
        Exp.fast_equal(g, parse_exp("h != 0")),
      )
    | other =>
      Alcotest.fail(
        "expected Where(Cons(..), ..), got " ++ Pat.show(Pat.fresh(other)),
      )
    }
  | _ => Alcotest.fail("expected a Match")
  };

let test_round_trips = () => {
  Alcotest.check(
    Alcotest.string,
    "let-bound guarded pattern",
    "let x where x != 0 = 1 in x",
    print_back("let x where x != 0 = 1 in x"),
  );
  Alcotest.check(
    Alcotest.string,
    "parenthesized guarded pattern",
    "let (x where x != 0) = 1 in x",
    print_back("let (x where x != 0) = 1 in x"),
  );
  /* The dedicated tiles print as before. */
  Alcotest.check(
    Alcotest.string,
    "FunWhere tile",
    "fun x where x != 0 -> 1 / x",
    print_back("fun x where x != 0 -> 1 / x"),
  );
  Alcotest.check(
    Alcotest.string,
    "ForallWhere tile",
    "forall x where x != 0 -> x / x == 1",
    print_back("forall x where x != 0 -> x / x == 1"),
  );
};

/* ===== 3. Statics =================================================== */

/* The guard analyzes against Bool with the pattern's bindings in scope,
 * and the pattern's type is the type of `p`. A well-typed guard puts no
 * mark on the program. */
let test_statics_ok = () =>
  Alcotest.check(
    Alcotest.int,
    "no marks on `let (x where x != 0) = 1 in x`",
    0,
    statics_marks("let (x where x != 0) = 1 in x") |> List.length,
  );

/* A non-boolean guard is a type error on the guard. */
let test_statics_guard_must_be_bool = () =>
  Alcotest.check(
    Alcotest.bool,
    "non-boolean guard is marked",
    true,
    statics_marks("let (x where x + 1) = 1 in x") != [],
  );

/* ===== 4. Dynamics: the guard is LIVE ============================== */

let check_eval = (msg, expected, src) =>
  Alcotest.check(
    Alcotest.bool,
    msg ++ " (got " ++ Exp.show(evaluate(elaborate(parse_exp(src)))) ++ ")",
    true,
    Exp.fast_equal(
      evaluate(elaborate(parse_exp(src))),
      evaluate(elaborate(parse_exp(expected))),
    ),
  );

let test_let_binds_ignoring_nothing = () =>
  check_eval(
    "a let-bound guarded pattern binds",
    "1",
    "let x where x != 0 = 1 in x",
  );

let test_case_guard_filters = () => {
  check_eval(
    "guard true: the guarded arm is taken",
    "1",
    "case 3 | x where x != 0 => 1 | _ => 2 end",
  );
  check_eval(
    "guard false: fall through to the catch-all",
    "2",
    "case 0 | x where x != 0 => 1 | _ => 2 end",
  );
};

let test_cons_guard_filters = () => {
  check_eval(
    "guarded cons arm taken",
    "1",
    "case [3, 4] | h :: t where h != 0 => 1 | _ => 2 end",
  );
  check_eval(
    "guarded cons arm skipped",
    "2",
    "case [0, 4] | h :: t where h != 0 => 1 | _ => 2 end",
  );
};

/* FunWhere application: on-guard runs, off-guard does NOT run the body
 * (Indet, not a division error). */
let test_fun_where_application_filters = () => {
  check_eval(
    "on-guard application runs",
    "50",
    "(fun x where x != 0 -> 100 / x)(2)",
  );
  let off =
    evaluate(elaborate(parse_exp("(fun x where x != 0 -> 100 / x)(0)")));
  switch (off |> Exp.term_of) {
  | Ap(_, _, _) => ()
  | other =>
    Alcotest.fail(
      "off-guard application should be a stuck Ap (Indet), got: "
      ++ Exp.show(Exp.fresh(other)),
    )
  };
};

/* PINNED — UNRESOLVED FORK (tile/token collision).
 *
 * `fun (p where g) -> e` and `forall (p where g) -> e` do NOT mean
 * `Fun(Where(p, g), e)`. The `where` in the `fun _ where _ -> _` /
 * `forall _ where _ -> _` POLYTILE reaches inside the parentheses and
 * claims the token, so the enclosing tile molds as the three-shard
 * FunWhere/ForallWhere.
 *
 * This is not new: on the base commit the same input already parsed as
 * `FunWhere(Parens(x), Parens(x != 0), body)` -- the parens were split
 * across the tile's two child slots. Adding the infix pattern `where`
 * changes WHICH wrong shape you get (the guard now lands in the pattern
 * and the tile's guard slot is left an EmptyHole), so this spelling
 * needs a decision, not a silent resolution. The UNPARENTHESIZED
 * `fun x where g -> e` / `forall x where g -> e` are unaffected -- see
 * the two tests above.
 *
 * Consequence for this slice: in the tile parser, a `Where` pattern in a
 * `fun`/`forall` BINDER position is only reachable through this
 * spelling. The reachable, correct uses of pattern-`where` are `let`,
 * `case` arms and `induction` arms, which is what the rest of this file
 * exercises. */
let test_paren_where_in_fun_binder_is_pinned = () =>
  switch (parse_exp("fun (x where x != 0) -> 100 / x") |> Exp.term_of) {
  | FunWhere(p, g, _) =>
    Alcotest.check(
      Alcotest.bool,
      "the pattern absorbed the guard",
      true,
      Pat.is_guarded(p),
    );
    Alcotest.check(
      Alcotest.bool,
      "and the tile's own guard slot was left empty",
      true,
      switch (g |> Exp.term_of) {
      | EmptyHole => true
      | _ => false
      },
    );
  | other =>
    Alcotest.fail(
      "pinned shape changed; re-decide the fork. Got: "
      ++ Exp.show(Exp.fresh(other)),
    )
  };

/* ===== 5. Proofs =================================================== */

/* An INDUCTION arm written `| w where w != 0 =>` installs `w != 0` as a
 * `where` hypothesis in that arm, exactly as a ForallWhere binder does.
 *
 * The program below has two arms with the SAME domain condition
 * `w != 0` (from `8 / w` in the arm goal). The guarded arm discharges it
 * against its own guard; the unguarded arm cannot. So exactly ONE
 * obligation stays pending -- that difference IS the fact installation.
 *
 * Additionally, the SECOND arm carries `!(w != 0)`: an arm is only
 * reached when the arms above it failed, so an earlier overlapping
 * guarded arm contributes its guard's NEGATION. */
let induction_two_arms = {|theorem t = forall z -> 8 / z == 8 / z proof induction z | w where w != 0 => axiom refl_eq at 0 on 8 / w == 8 / w end | w => axiom refl_eq at 0 on 8 / w == 8 / w end end in t|};

let is_where_entry = (e: Ctx.entry): option(Exp.t) =>
  switch (e) {
  | Ctx.TheoremEntry({name: "where", prop: Some(p), _}) => Some(p)
  | _ => None
  };

let is_remote: Obligation.discharge => bool =
  fun
  | Obligation.Remote(_) => true
  | _ => false;

let test_induction_arm_installs_where_fact = () => {
  let (_pm, _proof, obs) = obligations(induction_two_arms);
  Alcotest.check(
    Alcotest.int,
    "one `w != 0` condition per arm",
    2,
    List.length(obs),
  );
  /* The arm carrying the POSITIVE guard discharges its condition
     Remotely against that guard; the other arm's stays Pending. */
  let positive =
    obs
    |> List.filter((ob: Obligation.t) =>
         List.exists(
           w => Exp.fast_equal(w, parse_exp("w != 0")),
           List.filter_map(is_where_entry, ob.bindings),
         )
       );
  Alcotest.check(
    Alcotest.int,
    "exactly one arm carries the positive `w != 0` where-fact",
    1,
    List.length(positive),
  );
  Alcotest.check(
    Alcotest.bool,
    "and its domain condition is discharged Remote against it — got: "
    ++ String.concat(
         ", ",
         List.map(
           (ob: Obligation.t) => Obligation.show_discharge(ob.discharge),
           positive,
         ),
       ),
    true,
    List.for_all((ob: Obligation.t) => is_remote(ob.discharge), positive),
  );
};

let test_later_arm_gets_negated_guard = () => {
  let (_pm, _proof, obs) = obligations(induction_two_arms);
  let negated =
    obs
    |> List.filter((ob: Obligation.t) =>
         List.exists(
           w => Exp.fast_equal(w, parse_exp("!(w != 0)")),
           List.filter_map(is_where_entry, ob.bindings),
         )
       );
  Alcotest.check(
    Alcotest.int,
    "the LATER, overlapping arm carries `!(w != 0)` — the guard of the arm "
    ++ "above it, negated",
    1,
    List.length(negated),
  );
  Alcotest.check(
    Alcotest.bool,
    "and `w != 0` is NOT provable there: its condition stays Pending",
    true,
    List.for_all((ob: Obligation.t) => !is_remote(ob.discharge), negated),
  );
};

/* Constructor-DISJOINT earlier arms contribute nothing: a `| Z =>` arm
 * must not import an `| S(m) where P =>` arm's negated guard (`P` may not
 * even mention variables bound in this arm). Tested directly on the
 * overlap predicate, since building a program whose disjoint arm also
 * emits a domain obligation needs machinery unrelated to `where`. */
let test_disjoint_arms_do_not_overlap = () => {
  let pat = (s: string) =>
    switch (parse_exp("case ? " ++ s ++ " => 1 end") |> Exp.term_of) {
    | Match(_, [(p, _), ..._]) => p
    | _ => Alcotest.fail("could not parse pattern: " ++ s)
    };
  Alcotest.check(
    Alcotest.bool,
    "`[]` and `h :: t` are constructor-disjoint",
    false,
    ProofCheck.pats_overlap(pat("| h :: t"), pat("| []")),
  );
  Alcotest.check(
    Alcotest.bool,
    "`true` and `false` are constructor-disjoint",
    false,
    ProofCheck.pats_overlap(pat("| true"), pat("| false")),
  );
  Alcotest.check(
    Alcotest.bool,
    "a variable pattern overlaps everything",
    true,
    ProofCheck.pats_overlap(pat("| w"), pat("| []")),
  );
  Alcotest.check(
    Alcotest.bool,
    "same constructor overlaps",
    true,
    ProofCheck.pats_overlap(pat("| h :: t"), pat("| a :: b")),
  );
};

/* ===== 6. Coverage: a guard is not a case split ===================== */

/* `| x where g => .. | _ => ..` IS exhaustive: the unguarded catch-all
 * covers everything, and the guarded arm must not be flagged redundant. */
let test_guarded_arm_plus_catchall_is_exhaustive = () =>
  Alcotest.check(
    Alcotest.bool,
    "guarded arm + catch-all: no inexhaustiveness mark",
    false,
    statics_marks("case 0 | x where x != 0 => 1 | _ => 2 end")
    |> List.exists(is_inexhaustive),
  );

/* A guarded arm ALONE is NOT exhaustive: the guard can fail. */
let test_guarded_arm_alone_is_inexhaustive = () =>
  Alcotest.check(
    Alcotest.bool,
    "guarded arm alone: inexhaustiveness mark",
    true,
    statics_marks("case 0 | x where x != 0 => 1 end")
    |> List.exists(is_inexhaustive),
  );

/* PINNED, deliberately: complementary guards without a catch-all are
 * still reported inexhaustive. Deciding `x > 0 || x <= 0` is complete
 * needs an SMT; v1 does not fake it. */
let test_complementary_guards_still_inexhaustive = () =>
  Alcotest.check(
    Alcotest.bool,
    "complementary guards, no catch-all: still inexhaustive (pinned)",
    true,
    statics_marks("case 0 | x where x > 0 => 1 | y where y <= 0 => 2 end")
    |> List.exists(is_inexhaustive),
  );

let tests = (
  "WherePat",
  [
    test_case(
      "fun-where tile unchanged",
      `Quick,
      test_fun_where_tile_unchanged,
    ),
    test_case(
      "forall-where tile unchanged",
      `Quick,
      test_forall_where_tile_unchanged,
    ),
    test_case(
      "forall-where tile still proves",
      `Quick,
      test_forall_where_still_proves,
    ),
    test_case("parses as Where pattern", `Quick, test_parses_as_where_pat),
    test_case(
      "precedence looser than cons",
      `Quick,
      test_precedence_looser_than_cons,
    ),
    test_case("round-trips", `Quick, test_round_trips),
    test_case("statics ok", `Quick, test_statics_ok),
    test_case("guard must be Bool", `Quick, test_statics_guard_must_be_bool),
    test_case("let binds", `Quick, test_let_binds_ignoring_nothing),
    test_case("case guard filters", `Quick, test_case_guard_filters),
    test_case("cons guard filters", `Quick, test_cons_guard_filters),
    test_case(
      "FunWhere application filters",
      `Quick,
      test_fun_where_application_filters,
    ),
    test_case(
      "PINNED: `fun (p where g) ->` collides with the FunWhere tile",
      `Quick,
      test_paren_where_in_fun_binder_is_pinned,
    ),
    test_case(
      "induction arm installs its where fact",
      `Quick,
      test_induction_arm_installs_where_fact,
    ),
    test_case(
      "later overlapping arm gets the negated guard",
      `Quick,
      test_later_arm_gets_negated_guard,
    ),
    test_case(
      "disjoint arms do not overlap",
      `Quick,
      test_disjoint_arms_do_not_overlap,
    ),
    test_case(
      "guarded arm + catch-all is exhaustive",
      `Quick,
      test_guarded_arm_plus_catchall_is_exhaustive,
    ),
    test_case(
      "guarded arm alone is inexhaustive",
      `Quick,
      test_guarded_arm_alone_is_inexhaustive,
    ),
    test_case(
      "complementary guards still inexhaustive (pinned)",
      `Quick,
      test_complementary_guards_still_inexhaustive,
    ),
  ],
);
