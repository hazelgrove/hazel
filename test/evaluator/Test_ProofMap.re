open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Tests for the big-step proof checking phase. Each test parses a
 * program containing a theorem, evaluates it through the big-step
 * evaluator, and inspects the resulting EvaluatorState.proof_map for
 * the mark we expect on the proof term just inside the theorem. */

let statics_and_elab = (exp: Exp.t): (Statics.Map.t, Exp.t) =>
  Statics.mk(
    CoreSettings.on,
    Builtins.ctx_init(Some(Operators.default_mode)),
    exp,
  );

let eval_with_proof = (exp: Exp.t): (EvaluatorState.t, Statics.Map.t, Exp.t) => {
  let (statics, elab) = statics_and_elab(exp);
  let (_, state) =
    Evaluator.evaluate(~statics, ~env=Builtins.env_init, elab);
  (state, statics, elab);
};

/* Find a Theorem's proof sub-term in the elaborated expression. We use
 * map_term's stash side-channel to capture the first Theorem proof we
 * encounter; test programs only contain one. */
let find_theorem_proof = (e: Exp.t): option(Proof.t) => {
  let found = ref(None);
  let f_exp = (continue, e: Exp.t): Exp.t => {
    switch (e.term) {
    | Theorem(_, _, proof, _) when found^ == None =>
      found := Some(proof);
      e;
    | _ => continue(e)
    };
  };
  let _ = TermBase.Exp.map_term(~f_exp, e);
  found^;
};

let proof_id_of = (elab: Exp.t): Id.t =>
  switch (find_theorem_proof(elab)) {
  | Some(p) => Proof.rep_id(p)
  | None => Alcotest.fail("no theorem found in elaborated expression")
  };

let proof_of = (elab: Exp.t): Proof.t =>
  switch (find_theorem_proof(elab)) {
  | Some(p) => p
  | None => Alcotest.fail("no theorem found in elaborated expression")
  };

let proof_entry = (state: EvaluatorState.t, elab: Exp.t): ProofMap.entry =>
  switch (
    ProofMap.lookup(
      Proof.rep_id(proof_of(elab)),
      EvaluatorState.get_proof_map(state),
    )
  ) {
  | Some(entry) => entry
  | None => Alcotest.fail("no proof-map entry for theorem proof")
  };

let elaborated_exp = (src: string): Exp.t => {
  let (_, elab) = statics_and_elab(parse_exp(src));
  elab;
};

let check_exp = (msg: string, expected: string, actual: Exp.t) =>
  Alcotest.check(
    Alcotest.bool,
    msg,
    true,
    Exp.fast_equal(elaborated_exp(expected), actual),
  );

let require_exp = (msg: string, exp: option(Exp.t)): Exp.t =>
  switch (exp) {
  | Some(exp) => exp
  | None => Alcotest.fail(msg)
  };

/* A parenthesized redex has one hidden transition before the visible
 * arithmetic step. auto_incoming stores (justification, resulting_exp):
 * the expression is therefore the result of removing parentheses, not
 * the input that still contains them. */
let test_leading_auto_step_trace = () => {
  let src = {|theorem t = (1 + 2) == 3 proof eval (1 + 2) at 0 end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let entry = proof_entry(state, elab);
  check_exp(
    "incoming precedes hidden step",
    "(1 + 2) == 3",
    require_exp("expected incoming", entry.incoming),
  );
  switch (entry.auto_incoming) {
  | [("remove parentheses", resulting_exp)] =>
    check_exp(
      "auto_incoming expression follows hidden step",
      "1 + 2 == 3",
      resulting_exp,
    )
  | _ => Alcotest.fail("expected one leading remove-parentheses transition")
  };
  Alcotest.check(
    Alcotest.int,
    "no hidden transitions follow visible arithmetic",
    0,
    List.length(entry.auto_outgoing),
  );
  check_exp(
    "outgoing follows visible arithmetic",
    "3 == 3",
    require_exp("expected outgoing", entry.outgoing),
  );
};

/* Selecting the conditional branch is the visible step; removing the
 * selected branch's parentheses is hidden afterward. auto_outgoing stores
 * (input_exp, justification), so it must retain the still-parenthesized
 * input rather than the result of the hidden transition. */
let test_trailing_auto_step_trace = () => {
  let src = {|theorem t = (if true then (3) else 4) == 3 proof eval (if true then (3) else 4) at 0 end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let entry = proof_entry(state, elab);
  switch (entry.auto_outgoing) {
  | [(input_exp, "remove parentheses")] =>
    check_exp(
      "auto_outgoing expression precedes hidden step",
      "(3) == 3",
      input_exp,
    )
  | _ =>
    Alcotest.fail(
      "expected one trailing remove-parentheses transition: "
      ++ ProofMap.show_entry(entry),
    )
  };
  check_exp(
    "outgoing follows trailing lookup",
    "3 == 3",
    require_exp("expected outgoing", entry.outgoing),
  );
};

/* Reflexivity axiom on a closed equality: outgoing of the top-level
 * proof should be `true`, giving a checkmark. */
let test_refl_checkmark = () => {
  let src = {|theorem t = 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  Alcotest.check(
    Alcotest.option(bool),
    "refl proof should be checkmark",
    Some(true),
    ProofMap.status_of_proof(pm, proof_of(elab)),
  );
};

/* A theorem with outer universal quantifiers: the foralls are
 * auto-introduced, so a bare reflexivity proof on the conclusion proves
 * the theorem (outgoing `true`) without needing an explicit `forall`
 * proof step. */
let test_refl_forall_checkmark = () => {
  let src = {|theorem t = forall x -> x == x proof axiom refl_eq at 0 on x == x end in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  Alcotest.check(
    Alcotest.option(bool),
    "forall reflexivity proof should be checkmark",
    Some(true),
    ProofMap.status_of_proof(pm, proof_of(elab)),
  );
};

/* An empty-hole proof acts as the identity (recovery): the goal passes
 * through unchanged, so an unreduced goal stays unproven. */
let test_empty_hole_nothing = () => {
  let src = {|theorem t = 1 + 1 == 2 proof ? in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.option(bool),
    "empty-hole proof should be nothing",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
  /* The hole passes the goal through rather than going dark. */
  let outgoing =
    switch (ProofMap.lookup(Proof.rep_id(proof), pm)) {
    | Some({outgoing, _}) => outgoing
    | None => None
    };
  Alcotest.check(
    Alcotest.bool,
    "empty-hole proof passes the goal through",
    true,
    outgoing != None,
  );
};

/* A hole-free but unsuccessful proof of a false statement: the axiom
 * doesn't apply, so outgoing is None. That is not a disproof
 * (`Some(false)` requires outgoing `false`), and it must not be marked
 * proven either — status stays `None`. */
let test_false_goal_not_checkmark = () => {
  let src = {|theorem t = 1 == 2 proof axiom refl_eq at 0 on 1 == 2 end in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.bool,
    "proof has no EmptyHole/Invalid/MultiHole",
    false,
    Proof.has_hole(proof),
  );
  Alcotest.check(
    Alcotest.option(bool),
    "failed proof is neither proven nor disproven",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
};

/* A state-merge round-trip: append a freshly-evaluated state (carrying a
 * proof-map entry) onto an empty base and check the entry survives the
 * merge. */
let test_append_proof_map = () => {
  let src = {|theorem t = 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pid = proof_id_of(elab);
  let pm = EvaluatorState.get_proof_map(state);
  /* Merge the evaluated state onto a fresh empty base; the proof_map
   * should be carried through the append. */
  let merged = EvaluatorState.append(EvaluatorState.empty, state);
  let pm_merged = EvaluatorState.get_proof_map(merged);
  Alcotest.check(
    Alcotest.bool,
    "merged proof map has entry for proof id",
    ProofMap.lookup(pid, pm_merged) != None,
    ProofMap.lookup(pid, pm) != None,
  );
};

/* --- Proof-mark tests ---------------------------------------------
 *
 * Each test exercises a specific `ProofMark.t` variant by constructing
 * a theorem whose inner proof step should fail in the expected way,
 * then asserts that the proof-map marks at the inner step's id include
 * the right mark kind. `proof-mark error_ids` must also surface that id. */

/* Walk the inner proof recursively and collect the first proof sub-term
 * whose mark list is non-empty. Used to locate whichever sub-step the
 * checker reported a mark against. */
let rec find_marked_sub =
        (pm: ProofMap.t, proof: Proof.t): option((Id.t, list(ProofMark.t))) => {
  let id = Proof.rep_id(proof);
  let marks = ProofMap.marks_of(id, pm);
  if (marks != []) {
    Some((id, marks));
  } else {
    switch (proof.term) {
    | Seq(p1, p2) =>
      switch (find_marked_sub(pm, p1)) {
      | Some(_) as s => s
      | None => find_marked_sub(pm, p2)
      }
    | Forall(_, body) => find_marked_sub(pm, body)
    | Induction(_, cases) =>
      let rec scan = (
        fun
        | [] => None
        | [(_p, body), ...rest] =>
          switch (find_marked_sub(pm, body)) {
          | Some(_) as s => s
          | None => scan(rest)
          }
      );
      scan(cases);
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep(_) => None
    };
  };
};

let has_mark_kind =
    (pm: ProofMap.t, proof: Proof.t, pred: ProofMark.t => bool): bool =>
  switch (find_marked_sub(pm, proof)) {
  | Some((_, marks)) => List.exists(pred, marks)
  | None => false
  };

/* The proof-map error_ids list must include the id of any marked sub-term
 * — this drives the red shard overlay in the code view. */
let marked_id_is_surfaced = (pm: ProofMap.t, proof: Proof.t): bool =>
  switch (find_marked_sub(pm, proof)) {
  | Some((id, _)) => List.mem(id, ProofMap.error_ids(pm))
  | None => false
  };

/* UnknownEquality: an axiom that names a rule not in scope. */
let test_unknown_equality_mark = () => {
  let src = {|theorem t = 1 == 1 proof axiom bogus at 0 on 1 == 1 end in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.bool,
    "UnknownEquality mark is emitted",
    true,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.UnknownEquality(_) => true
      | _ => false,
    ),
  );
  Alcotest.check(
    Alcotest.bool,
    "marked id appears in ProofMap.error_ids",
    true,
    marked_id_is_surfaced(pm, proof),
  );
};

/* PatternNotFound: axiom's at_exp pattern isn't present at the
 * requested occurrence in the incoming goal. */
let test_pattern_not_found_mark = () => {
  let src = {|theorem t = 1 == 1 proof axiom refl_eq at 0 on 5 == 5 end in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.bool,
    "PatternNotFound mark is emitted",
    true,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.PatternNotFound(_) => true
      | _ => false,
    ),
  );
};

/* Rewrite (AlgebriteStep) should locate the `at_exp` pattern in the
 * incoming goal. Here `x + x` is literally the left-hand side of the
 * goal `x + x == 2 * x`, so occurrence 0 must be found and NO
 * PatternNotFound mark should be emitted. */
let test_rewrite_finds_pattern = () => {
  let src = {|theorem t = forall x -> x + x == 2 * x proof rewrite x + x with 2 * x at 0 end in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.bool,
    "rewrite must find `x + x` (no PatternNotFound mark)",
    false,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.PatternNotFound(_) => true
      | _ => false,
    ),
  );
};

/* Same as above but under `use Nat`, mirroring the user's program. The
 * `use Nat` sets the numeric operator mode to Nat, so both the goal's
 * `x + x` and the rewrite's `at_exp` must elaborate to Nat(Plus) and
 * still match. Regression test: previously the proof term's expressions
 * were never elaborated, so `at_exp` stayed Int(Plus) and failed to
 * match the goal's Nat(Plus), spuriously reporting PatternNotFound. */
let test_rewrite_finds_pattern_nat = () => {
  let src = {|use Nat in theorem t = forall x -> x + x == 2 * x proof rewrite x + x with 2 * x at 0 end in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.bool,
    "rewrite under `use Nat` must find `x + x` (no PatternNotFound mark)",
    false,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.PatternNotFound(_) => true
      | _ => false,
    ),
  );
};

/* NothingToStep: an eval step on a fully-normalised subterm. The literal
 * `1` has no reducible step under the evaluator. */
let test_nothing_to_step_mark = () => {
  let src = {|theorem t = 1 == 1 proof eval 1 at 0 end in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.bool,
    "NothingToStep mark is emitted",
    true,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.NothingToStep(_) => true
      | _ => false,
    ),
  );
};

/* ExpectedForallGoal: a `forall` proof when the goal isn't a forall. */
let test_expected_forall_goal_mark = () => {
  let src = {|theorem t = 1 == 1 proof forall x => axiom refl_eq at 0 on 1 == 1 end in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.bool,
    "ExpectedForallGoal mark is emitted",
    true,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.ExpectedForallGoal => true
      | _ => false,
    ),
  );
};

/* MissingIncoming: once a step breaks propagation, the next step in a
 * sequence should record MissingIncoming against its own id. */
let test_missing_incoming_mark = () => {
  /* First axiom is bogus → it records a mark but passes the goal
   * through (error recovery), so the second axiom still acts on
   * `1 == 1` and succeeds. The proof reaches `true`, but the broken
   * step keeps it from counting as proven. */
  let src = {|theorem t = 1 == 1 proof axiom bogus at 0 on 1 == 1 end; axiom refl_eq at 0 on 1 == 1 end in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  /* No step goes dark: nothing carries MissingIncoming any more. */
  let rec scan = (p: Proof.t): bool => {
    let here =
      List.exists(
        fun
        | ProofMark.MissingIncoming => true
        | _ => false,
        ProofMap.marks_of(Proof.rep_id(p), pm),
      );
    here
    || (
      switch (p.term) {
      | Seq(a, b) => scan(a) || scan(b)
      | _ => false
      }
    );
  };
  Alcotest.check(
    Alcotest.bool,
    "broken step recovers instead of blocking the second step",
    false,
    scan(proof),
  );
  /* The second step ran against the passed-through goal and reduced it
   * to `true`... */
  let outgoing =
    switch (ProofMap.lookup(Proof.rep_id(proof), pm)) {
    | Some({outgoing: Some(e), _}) => Some(e)
    | _ => None
    };
  Alcotest.check(
    Alcotest.bool,
    "later step still ran on the recovered goal",
    true,
    switch (outgoing) {
    | Some(e) => Exp.fast_equal(e, Exp.temp(Atom(Bool(true))))
    | None => false
    },
  );
  /* ...but a proof with a broken step in it is not proven. */
  Alcotest.check(
    Alcotest.option(bool),
    "broken-but-recovered proof is not marked proven",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
};

/* Chained eval steps: each step's outgoing feeds the next. */
let test_one_eval_step_outgoing = () => {
  let src = {|theorem thm = 1 + 4 == 5 proof eval 1 + 4 at 0 end in thm|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let entry = proof_entry(state, elab);
  check_exp(
    "one-step outgoing",
    "5 == 5",
    require_exp("expected outgoing", entry.outgoing),
  );
};

let test_two_eval_steps_outgoing = () => {
  let src = {|theorem thm = 1 + 4 == 5 proof eval 1 + 4 at 0 end; eval 5 == 5 at 0 end in thm|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let entry = proof_entry(state, elab);
  check_exp(
    "two-step outgoing",
    "true",
    require_exp("expected outgoing", entry.outgoing),
  );
};

/* A theorem mid-edit (body after `in` still empty) must still get its
   proof checked into the proof map. */
let test_empty_body_still_checked = () => {
  let src = "theorem thm = 1 + 4 == 5 proof \neval 1 + 4 at 0 end\n  in";
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let entry = proof_entry(state, elab);
  check_exp(
    "empty-body one-step outgoing",
    "5 == 5",
    require_exp("expected outgoing (empty body)", entry.outgoing),
  );
};

/* MalformedProofTerm: unparseable text in proof position is an error,
 * unlike an EmptyHole (an intentionally-incomplete proof, unmarked). */
let test_malformed_proof_term_mark = () => {
  let src = {|theorem t = 1 == 1 proof 1 + 1 in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.bool,
    "MalformedProofTerm mark is emitted",
    true,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.MalformedProofTerm => true
      | _ => false,
    ),
  );
};

let test_empty_hole_proof_unmarked = () => {
  let src = {|theorem t = 1 == 1 proof ? in t|};
  let uexp = parse_exp(src);
  let (state, _, elab) = eval_with_proof(uexp);
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.bool,
    "an empty-hole proof carries no mark",
    true,
    find_marked_sub(pm, proof) == None,
  );
};

/* Direction survives the axiom surface syntax: `axiomrev` parses to a
 * Left-direction step, and printing + reparsing preserves it. (Before
 * the `axiomrev` form, direction was silently dropped on every
 * serialization round trip and reset to Right.) */
let axiom_direction_of = (p: Proof.t): option(Util.Direction.t) => {
  let rec go = (p: Proof.t): option(Util.Direction.t) =>
    switch (p.term) {
    | AxiomStep({direction, _}) => Some(direction)
    | Seq(p1, p2) =>
      switch (go(p1)) {
      | Some(_) as d => d
      | None => go(p2)
      }
    | Forall(_, body) => go(body)
    | _ => None
    };
  go(p);
};

let print_exp = (e: Exp.t): string =>
  e
  |> Haz3lcore.ExpToSegment.exp_to_segment(
       ~settings=Haz3lcore.ExpToSegment.Settings.editable(~inline=true),
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]);

let test_axiom_direction_roundtrip = () => {
  let src = {|theorem t = 1 == 1 proof axiomrev refl_eq at 0 on 1 end in t|};
  let direction_of = (e: Exp.t): option(Util.Direction.t) =>
    switch (find_theorem_proof(e)) {
    | Some(p) => axiom_direction_of(p)
    | None => None
    };
  let uexp = parse_exp(src);
  Alcotest.check(
    Alcotest.bool,
    "axiomrev parses to a Left-direction step",
    true,
    direction_of(uexp) == Some(Util.Direction.Left),
  );
  let reparsed = parse_exp(print_exp(uexp));
  Alcotest.check(
    Alcotest.bool,
    "direction survives print + reparse",
    true,
    direction_of(reparsed) == Some(Util.Direction.Left),
  );
};

let tests = (
  "Evaluator.ProofMap",
  [
    test_case(
      "one eval step's outgoing feeds the goal",
      `Quick,
      test_one_eval_step_outgoing,
    ),
    test_case(
      "two chained eval steps discharge the goal",
      `Quick,
      test_two_eval_steps_outgoing,
    ),
    test_case(
      "empty theorem body still checks the proof",
      `Quick,
      test_empty_body_still_checked,
    ),
    test_case(
      "eval trace records leading hidden transition",
      `Quick,
      test_leading_auto_step_trace,
    ),
    test_case(
      "eval trace records trailing hidden transition",
      `Quick,
      test_trailing_auto_step_trace,
    ),
    test_case("refl produces checkmark", `Quick, test_refl_checkmark),
    test_case(
      "forall refl produces checkmark",
      `Quick,
      test_refl_forall_checkmark,
    ),
    test_case("empty hole produces nothing", `Quick, test_empty_hole_nothing),
    test_case(
      "false goal is not marked as proven",
      `Quick,
      test_false_goal_not_checkmark,
    ),
    test_case("append carries proof_map", `Quick, test_append_proof_map),
    test_case(
      "unknown equality emits mark",
      `Quick,
      test_unknown_equality_mark,
    ),
    test_case(
      "pattern-not-found emits mark",
      `Quick,
      test_pattern_not_found_mark,
    ),
    test_case("rewrite finds pattern", `Quick, test_rewrite_finds_pattern),
    test_case(
      "rewrite finds pattern under use Nat",
      `Quick,
      test_rewrite_finds_pattern_nat,
    ),
    test_case(
      "nothing-to-step emits mark",
      `Quick,
      test_nothing_to_step_mark,
    ),
    test_case(
      "expected forall goal emits mark",
      `Quick,
      test_expected_forall_goal_mark,
    ),
    test_case(
      "missing-incoming propagates to later step",
      `Quick,
      test_missing_incoming_mark,
    ),
    test_case(
      "malformed proof term emits mark",
      `Quick,
      test_malformed_proof_term_mark,
    ),
    test_case(
      "empty-hole proof stays unmarked",
      `Quick,
      test_empty_hole_proof_unmarked,
    ),
    test_case(
      "axiom direction survives round trip",
      `Quick,
      test_axiom_direction_roundtrip,
    ),
  ],
);
