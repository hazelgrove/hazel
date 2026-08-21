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
    | Forall(_, body)
    | Assume(_, body)
    | Generalize(_, body)
    | Revert(_, _, body) => find_marked_sub(pm, body)
    | Have(_, sub, body) =>
      switch (find_marked_sub(pm, sub)) {
      | Some(_) as s => s
      | None => find_marked_sub(pm, body)
      }
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
    | Contradiction(_)
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

/* --- Assume / obligation tests -------------------------------------
 *
 * `assume <exp> => <proof>` hypothesizes <exp> for the sub-proof and
 * incurs a sequent obligation, recorded on the assume node's proof-map
 * entry with discharge provenance. `full_status_of_proof` refines the
 * legacy `status_of_proof` with the obligation-aware `ProvenModulo`. */

/* (a) An unused, undischargeable assumption: the proof still reaches
 * `true` (legacy status unchanged), but full status is ProvenModulo with
 * exactly one pending obligation `1 == 2`. (The assumption is closed but
 * evaluates to `false`, so discharge channel 2 does NOT fire — a closed
 * `true` assumption would discharge as `Evaluated`, see the next test.) */
let test_assume_proven_modulo = () => {
  let src = {|theorem t = 1 == 1 proof assume 1 == 2 => axiom refl_eq at 0 on 1 == 1 end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.option(bool),
    "legacy status still reports proven",
    Some(true),
    ProofMap.status_of_proof(pm, proof),
  );
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProvenModulo([ob]) =>
    check_exp("pending obligation goal is the assumption", "1 == 2", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "the obligation is pending",
      true,
      Obligation.is_pending(ob),
    );
  | other =>
    Alcotest.fail(
      "expected ProvenModulo with one obligation, got: "
      ++ ProofMap.show_full_status(other),
    )
  };
};

/* (a') Discharge channel 2 (closed evaluation): a closed assumption that
 * evaluates to literal `true` discharges as `Evaluated`, so the theorem
 * is fully Proven despite the (baked) assumption. */
let test_assume_closed_true_discharges_evaluated = () => {
  let src = {|theorem t = 1 == 1 proof assume 2 == 2 => axiom refl_eq at 0 on 1 == 1 end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  let all = ProofMap.obligations_of_proof(pm, proof);
  Alcotest.check(
    Alcotest.bool,
    "the obligation discharges by closed evaluation",
    true,
    switch (all) {
    | [ob] => ob.discharge == Obligation.Evaluated
    | _ => false
    },
  );
  Alcotest.check(
    Alcotest.bool,
    "full status is Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* (b) The assumed equation is USED: citing the generated hypothesis name
 * (`assume` — SemanticCtx free-name generation from base "assume")
 * rewrites the goal with the assumed equation, so the theorem is
 * ProvenModulo its single pending obligation. */
let test_assume_hypothesis_used = () => {
  let src = {|theorem t = forall x -> x == 1 proof assume x == 1 => axiom assume at 0 on x end; axiom refl_eq at 0 on 1 == 1 end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProvenModulo([ob]) =>
    check_exp("pending obligation goal", "x == 1", ob.goal)
  | other =>
    Alcotest.fail(
      "expected ProvenModulo with one obligation, got: "
      ++ ProofMap.show_full_status(other),
    )
  };
};

/* (c) A nested assume of an identical proposition: the inner obligation
 * discharges Remote against the outer hypothesis (channel-1 lookup), so
 * two obligations are recorded but only ONE is pending. (Channel 1 is
 * tried before channel 2, and `1 == 2` evaluates to `false` anyway, so
 * the outer one stays pending.) */
let test_nested_assume_discharges_remote = () => {
  let src = {|theorem t = 1 == 1 proof assume 1 == 2 => assume 1 == 2 => axiom refl_eq at 0 on 1 == 1 end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  let all = ProofMap.obligations_of_proof(pm, proof);
  Alcotest.check(
    Alcotest.int,
    "two obligations are recorded",
    2,
    List.length(all),
  );
  Alcotest.check(
    Alcotest.int,
    "only one obligation is pending",
    1,
    List.length(ProofMap.pending_obligations(pm, proof)),
  );
  Alcotest.check(
    Alcotest.bool,
    "the discharged one is Remote",
    true,
    List.exists(
      (ob: Obligation.t) =>
        switch (ob.discharge) {
        | Remote(_) => true
        | Local(_)
        | Evaluated
        | Pending => false
        },
      all,
    ),
  );
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProvenModulo([_]) => ()
  | other =>
    Alcotest.fail(
      "expected ProvenModulo with one pending obligation, got: "
      ++ ProofMap.show_full_status(other),
    )
  };
};

/* (d) Regression: an assume-free proof has no obligations and full
 * status Proven. */
let test_no_assume_full_status_proven = () => {
  let src = {|theorem t = 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.int,
    "no obligations recorded",
    0,
    List.length(ProofMap.obligations_of_proof(pm, proof)),
  );
  Alcotest.check(
    Alcotest.bool,
    "full status is Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- Phase 2: implication and restrictions --------------------------- */

/* Find the proof of the theorem bound to `name` (test programs may now
 * contain several theorems, e.g. a guarded lemma and its use site). */
let rec pat_var_name = (p: Pat.t): option(string) =>
  switch (p.term) {
  | Var(x) => Some(x)
  | Parens(p)
  | Projector(_, p)
  | Asc(p, _) => pat_var_name(p)
  | _ => None
  };

let find_theorem_proof_named = (name: string, e: Exp.t): option(Proof.t) => {
  let found = ref(None);
  let f_exp = (continue, e: Exp.t): Exp.t => {
    switch (e.term) {
    | Theorem(p, _, proof, _) when pat_var_name(p) == Some(name) =>
      if (found^ == None) {
        found := Some(proof);
      };
      continue(e);
    | _ => continue(e)
    };
  };
  let _ = TermBase.Exp.map_term(~f_exp, e);
  found^;
};

let proof_of_named = (name: string, elab: Exp.t): Proof.t =>
  switch (find_theorem_proof_named(name, elab)) {
  | Some(p) => p
  | None => Alcotest.fail("no theorem named " ++ name)
  };

/* `==>` parses right-associative, just below `||`, and round-trips
 * through printing. */
let test_implies_parse_and_roundtrip = () => {
  let same = (msg, a, b) =>
    Alcotest.check(
      Alcotest.bool,
      msg,
      true,
      Exp.fast_equal(parse_exp(a), parse_exp(b)),
    );
  let diff = (msg, a, b) =>
    Alcotest.check(
      Alcotest.bool,
      msg,
      false,
      Exp.fast_equal(parse_exp(a), parse_exp(b)),
    );
  same(
    "==> binds looser than ==",
    "a == b ==> c == d",
    "(a == b) ==> (c == d)",
  );
  same("==> is right-associative", "a ==> b ==> c", "a ==> (b ==> c)");
  diff("==> is not left-associative", "a ==> b ==> c", "(a ==> b) ==> c");
  same(
    "==> binds looser than ||",
    "a || b ==> c || d",
    "(a || b) ==> (c || d)",
  );
  let roundtrip = src => {
    let e = parse_exp(src);
    Alcotest.check(
      Alcotest.bool,
      "round-trip: " ++ src,
      true,
      Exp.fast_equal(e, parse_exp(print_exp(e))),
    );
  };
  roundtrip("a == b ==> c == d");
  roundtrip("a ==> b ==> c");
  roundtrip("(a ==> b) ==> c");
};

/* `forall p where g -> e` parses, prints, and round-trips; exp_to_rule
 * reads the guard as an assumption. */
let test_forall_where_parse_and_rule = () => {
  let src = "forall x where x != 0 -> x == x";
  let e = parse_exp(src);
  Alcotest.check(
    Alcotest.bool,
    "parses to ForallWhere",
    true,
    switch (Exp.term_of(e)) {
    | ForallWhere(_, _, _) => true
    | _ => false
    },
  );
  Alcotest.check(
    Alcotest.bool,
    "round-trips through printing",
    true,
    Exp.fast_equal(e, parse_exp(print_exp(e))),
  );
  let rule = ProofRule.exp_to_rule(e);
  Alcotest.check(
    Alcotest.int,
    "guard becomes one assumption",
    1,
    List.length(rule.assumptions),
  );
  check_exp(
    "the assumption is the guard",
    "x != 0",
    List.hd(rule.assumptions),
  );
  Alcotest.check(
    Alcotest.bool,
    "conclusion is the equality",
    true,
    switch (rule.conclusion) {
    | Equality(_, _) => true
    /* `classify` never grants the bare-boolean reading; only
       `with_bool_fact_reading` does, at the point of use. */
    | BoolFact(_)
    | Other(_) => false
    },
  );
};

/* Implication INTRO via assume: when the assumed exp is the antecedent of
 * the incoming `A ==> B` goal, the checker strips it — body's incoming is
 * B — and NO obligation is incurred. A complete proof goes through with
 * full status Proven and zero obligations. */
let test_impl_intro_via_assume = () => {
  let src = {|theorem t = forall x -> x == 2 ==> x + 1 == 3 proof assume x == 2 => axiom assume at 0 on x end; eval 2 + 1 at 0 end; axiom refl_eq at 0 on 3 == 3 end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.int,
    "intro incurs ZERO obligations",
    0,
    List.length(ProofMap.obligations_of_proof(pm, proof)),
  );
  Alcotest.check(
    Alcotest.bool,
    "full status is Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* Non-matching assume against an implication goal falls back to Phase-1
 * behavior: the goal keeps its antecedent, so the proof can't strip it,
 * and the assume incurs its obligation. */
let test_assume_non_antecedent_keeps_goal = () => {
  let src = {|theorem t = forall x -> x == 2 ==> x + 1 == 3 proof assume x == 5 => ? in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.int,
    "non-intro assume still incurs its obligation",
    1,
    List.length(ProofMap.obligations_of_proof(pm, proof)),
  );
  Alcotest.check(
    Alcotest.option(bool),
    "nothing is proven",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
};

/* Conditional rule via a restricted binder: a use site with a CLOSED
 * instantiation incurs the instantiated guard, which discharge channel 2
 * evaluates away (`Evaluated`) — full status Proven. */
let test_conditional_rule_closed_discharges = () => {
  let src = {|theorem inv = forall x where x != 0 -> x == x proof axiom refl_eq at 0 on x == x end in theorem u = 2 == 2 proof axiom inv at 0 on 2 end; axiom refl_eq at 0 on 2 == 2 end in u|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let use_proof = proof_of_named("u", elab);
  let obs = ProofMap.obligations_of_proof(pm, use_proof);
  Alcotest.check(
    Alcotest.int,
    "the use site incurs one obligation",
    1,
    List.length(obs),
  );
  switch (obs) {
  | [ob] =>
    check_exp("the obligation is the instantiated guard", "2 != 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "discharged by closed evaluation",
      true,
      ob.discharge == Obligation.Evaluated,
    );
  | _ => ()
  };
  Alcotest.check(
    Alcotest.bool,
    "use is fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, use_proof) == ProofMap.Proven,
  );
};

/* Same rule with an OPEN instantiation: the instantiated guard can't be
 * evaluated (never evaluate open goals) and nothing in scope covers it —
 * Pending, so the use is ProvenModulo. */
let test_conditional_rule_open_pending = () => {
  let src = {|theorem inv = forall x where x != 0 -> x == x proof axiom refl_eq at 0 on x == x end in theorem u = forall y -> y == y proof axiom inv at 0 on y end; axiom refl_eq at 0 on y == y end in u|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let use_proof = proof_of_named("u", elab);
  switch (ProofMap.full_status_of_proof(pm, use_proof)) {
  | ProvenModulo([ob]) =>
    check_exp("pending obligation is the open guard", "y != 0", ob.goal)
  | other =>
    Alcotest.fail(
      "expected ProvenModulo with one pending obligation, got: "
      ++ ProofMap.show_full_status(other),
    )
  };
};

/* Peeling a restricted binder installs the guard as a citable hypothesis
 * (base name "where") — free, sound intro: the proof can rewrite with it
 * and close with ZERO obligations. */
let test_forall_where_hypothesis_cited = () => {
  let src = {|theorem t = forall x where x == 1 -> x == 1 proof axiom where at 0 on x end; axiom refl_eq at 0 on 1 == 1 end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  Alcotest.check(
    Alcotest.int,
    "where-intro incurs no obligations",
    0,
    List.length(ProofMap.obligations_of_proof(pm, proof)),
  );
  Alcotest.check(
    Alcotest.bool,
    "full status is Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* Underdetermined instantiation: a conditional rule whose assumption
 * mentions a metavariable (`k`) the conclusion match cannot fix is
 * refused with UnderdeterminedInstantiation. */
let test_underdetermined_instantiation = () => {
  let src = {|theorem r = forall x -> forall k -> k != 0 ==> x == x proof ? in theorem u = 2 == 2 proof axiom r at 0 on 2 end in u|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let use_proof = proof_of_named("u", elab);
  Alcotest.check(
    Alcotest.bool,
    "UnderdeterminedInstantiation mark is emitted",
    true,
    has_mark_kind(
      pm,
      use_proof,
      fun
      | ProofMark.UnderdeterminedInstantiation(_) => true
      | _ => false,
    ),
  );
};

/* Built-in Kleene axioms are usable as ordinary axiom steps. */
let test_kleene_axiom_and_comm = () => {
  let src = {|theorem t = (true && false) == (false && true) proof axiom and_comm at 0 on true && false end; axiom refl_eq at 0 on (false && true) == (false && true) end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  Alcotest.check(
    Alcotest.bool,
    "and_comm proof is Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof_of(elab)) == ProofMap.Proven,
  );
};

let test_kleene_axiom_impl_def = () => {
  let src = {|theorem t = forall p -> forall q -> (p ==> q) == (!p || q) proof axiom impl_def at 0 on p ==> q end; axiom refl_eq at 0 on (!p || q) == (!p || q) end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  Alcotest.check(
    Alcotest.bool,
    "impl_def proof is Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof_of(elab)) == ProofMap.Proven,
  );
};

/* ---- Pattern targets in `on`/`at` slots ----------------------------
 *
 * The target slot of a proof step accepts a pattern with `$e`/`$v`/`$x`
 * metavariables (MetaVar), reusing the stepper filter convention. The
 * matched *concrete* subterm is what gets rewritten or evaluated, so
 * these tests assert the same outcomes as their ground-quote
 * neighbours above.
 */

/* `axiom refl_eq at 0 on $e == $e` in place of the exact quote: the
 * pattern locates the equation, and the axiom still applies to the
 * concrete `1 == 1` it matched. Compare test_refl_checkmark. */
let test_pattern_refl_checkmark = () => {
  let src = {|theorem t = 1 == 1 proof axiom refl_eq at 0 on $e == $e end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  Alcotest.check(
    Alcotest.option(bool),
    "pattern-target refl proof should be checkmark",
    Some(true),
    ProofMap.status_of_proof(pm, proof_of(elab)),
  );
};

/* The same under a binder: the pattern does not have to name `x`, which
 * is the point — editing the statement's variable no longer breaks the
 * proof. Compare test_refl_forall_checkmark. */
let test_pattern_refl_forall_checkmark = () => {
  let src = {|theorem t = forall x -> x == x proof axiom refl_eq at 0 on $e == $e end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  Alcotest.check(
    Alcotest.option(bool),
    "pattern-target forall refl proof should be checkmark",
    Some(true),
    ProofMap.status_of_proof(pm, proof_of(elab)),
  );
};

/* An eval step whose target is a pattern: `$e + $e` locates `1 + 2`
 * and the concrete subterm is what steps. Compare
 * test_leading_auto_step_trace, which quotes `(1 + 2)` exactly. */
let test_pattern_eval_step_outgoing = () => {
  let src = {|theorem t = (1 + 2) == 3 proof eval $e + $e at 0 end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let entry = proof_entry(state, elab);
  check_exp(
    "pattern-target eval step reaches the same outgoing",
    "3 == 3",
    require_exp("expected outgoing", entry.outgoing),
  );
};

/* `$v` is value-restricted, so it discriminates where `$e` would not:
 * in `(1 + 2) + (3 + 4)` the outer sum has non-value operands, so it is
 * not a `$v + $v` match and only the two inner sums are. This also
 * exercises `at <idx>` indexing among *pattern* matches; the index
 * counts in `nth_exp`'s existing traversal order, which reaches a
 * binary operator's right operand first, so `at 1` is the left sum. */
let test_pattern_value_metavar_indexing = () => {
  let src = {|theorem t = (1 + 2) + (3 + 4) == 10 proof eval $v + $v at 1 end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let entry = proof_entry(state, elab);
  check_exp(
    "second $v + $v match is the left-hand sum",
    "3 + (3 + 4) == 10",
    require_exp("expected outgoing", entry.outgoing),
  );
};

/* A pattern that matches nothing is reported exactly like a missing
 * exact quote. Compare the PatternNotFound test for `5 == 5`. */
let test_pattern_not_found = () => {
  let src = {|theorem t = 1 == 1 proof axiom refl_eq at 0 on $e + $e end in t|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  Alcotest.check(
    Alcotest.bool,
    "unmatched pattern target is a PatternNotFound",
    true,
    has_mark_kind(
      pm,
      proof_of(elab),
      fun
      | ProofMark.PatternNotFound(_) => true
      | _ => false,
    ),
  );
};

/* Metavariables in a target slot must not read as free variables: the
 * statics case that covers stepper filters now covers these slots too,
 * so the program has no free-variable mark. */
let test_pattern_metavar_not_free = () => {
  let src = {|theorem t = 1 == 1 proof axiom refl_eq at 0 on $e == $e end in t|};
  let (statics, _) = statics_and_elab(parse_exp(src));
  let free =
    Id.Map.exists(
      (_, info) =>
        switch (info) {
        | Info.InfoExp({marks, _}) =>
          List.exists(
            fun
            | Mark.Free(name) => MetaVar.is_meta_name(name)
            | _ => false,
            marks,
          )
        | _ => false
        },
      statics,
    );
  Alcotest.check(
    Alcotest.bool,
    "no metavariable is marked free",
    false,
    free,
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
    test_case(
      "assume incurs a pending obligation (ProvenModulo)",
      `Quick,
      test_assume_proven_modulo,
    ),
    test_case(
      "assumed hypothesis is citable and rewrites the goal",
      `Quick,
      test_assume_hypothesis_used,
    ),
    test_case(
      "nested identical assume discharges Remote",
      `Quick,
      test_nested_assume_discharges_remote,
    ),
    test_case(
      "assume-free proof is Proven with no obligations",
      `Quick,
      test_no_assume_full_status_proven,
    ),
    test_case(
      "closed true assumption discharges Evaluated",
      `Quick,
      test_assume_closed_true_discharges_evaluated,
    ),
    test_case(
      "==> parses and round-trips",
      `Quick,
      test_implies_parse_and_roundtrip,
    ),
    test_case(
      "forall-where parses and reads as a conditional rule",
      `Quick,
      test_forall_where_parse_and_rule,
    ),
    test_case(
      "implication intro via assume (no obligation)",
      `Quick,
      test_impl_intro_via_assume,
    ),
    test_case(
      "non-antecedent assume keeps the goal and its obligation",
      `Quick,
      test_assume_non_antecedent_keeps_goal,
    ),
    test_case(
      "conditional rule: closed instantiation discharges Evaluated",
      `Quick,
      test_conditional_rule_closed_discharges,
    ),
    test_case(
      "conditional rule: open instantiation stays Pending",
      `Quick,
      test_conditional_rule_open_pending,
    ),
    test_case(
      "forall-where restriction is a citable hypothesis",
      `Quick,
      test_forall_where_hypothesis_cited,
    ),
    test_case(
      "underdetermined instantiation is refused",
      `Quick,
      test_underdetermined_instantiation,
    ),
    test_case(
      "Kleene axiom and_comm is usable",
      `Quick,
      test_kleene_axiom_and_comm,
    ),
    test_case(
      "Kleene axiom impl_def is usable",
      `Quick,
      test_kleene_axiom_impl_def,
    ),
    test_case(
      "pattern target: $e == $e proves reflexivity",
      `Quick,
      test_pattern_refl_checkmark,
    ),
    test_case(
      "pattern target: $e == $e under a binder",
      `Quick,
      test_pattern_refl_forall_checkmark,
    ),
    test_case(
      "pattern target: eval $e + $e reaches the same outgoing",
      `Quick,
      test_pattern_eval_step_outgoing,
    ),
    test_case(
      "pattern target: $v is value-restricted and `at` indexes matches",
      `Quick,
      test_pattern_value_metavar_indexing,
    ),
    test_case(
      "pattern target: an unmatched pattern is PatternNotFound",
      `Quick,
      test_pattern_not_found,
    ),
    test_case(
      "pattern target: metavariables are not free variables",
      `Quick,
      test_pattern_metavar_not_free,
    ),
  ],
);
