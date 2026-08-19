open Alcotest;
open Language;
open Test_Evaluator_Prelude;

module ObligationsPanel = Web.ObligationsPanel;

/* UI-1: the obligation-surfacing layer (docs/prover-obligations.md §3–§4).
 *
 * Covers the pure functions the theorem panel is built from — the
 * ProvenModulo status mapping, obligation grouping (proof-tree vs
 * definition-time), display ordering, discharge receipts — plus the
 * core-side `Obligation.display_goal` that makes goals printable.
 *
 * Reuses the Test_ProofMap harness like Test_Definedness. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of = Test_ProofMap.proof_of;
let check_exp = Test_ProofMap.check_exp;

let run = (src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  (EvaluatorState.get_proof_map(state), proof_of(elab));
};

let bool_check = (msg, expected, actual) =>
  Alcotest.check(Alcotest.bool, msg, expected, actual);

let int_check = (msg, expected, actual) =>
  Alcotest.check(Alcotest.int, msg, expected, actual);

let str_check = (msg, expected, actual) =>
  Alcotest.check(Alcotest.string, msg, expected, actual);

/* The repro program the panel is reviewed against in the browser. One
 * theorem, three obligations in its proof — one per discharge state — and
 * one definition-time obligation from the unguarded `g`:
 *
 *   z != 0   Pending    (open; nothing in scope covers it)
 *   2 != 0   Evaluated  (ground; closed evaluation, channel 2)
 *   y != 0   Remote     (covered by the theorem's `where` restriction)
 *   w != 0   Pending    at the definition of `g`, whose body divides by an
 *                        unconstrained parameter
 */
let repro = {|let f = fun x where x != 0 -> 100 / x in
let g = fun w -> 100 / w in
theorem t = forall y where y != 0 -> forall z -> f(y) + 4 / 2 + 8 / z == f(y) + 4 / 2 + 8 / z
proof axiom refl_eq at 0 on f(y) + 4 / 2 + 8 / z == f(y) + 4 / 2 + 8 / z end
in t|};

let repro_group = () => {
  let (pm, proof) = run(repro);
  (pm, proof, ObligationsPanel.group_of(~pm, ~proofs=[proof]));
};

/* --- status mapping (§3.1's third outcome) --------------------------- */

/* The four statuses get four distinct chip classes; ProvenModulo is
 * neither the proven nor the incomplete one. */
let test_status_classes_distinct = () => {
  let cls = ObligationsPanel.status_class;
  str_check("Proven", "true", cls(ProofMap.Proven));
  str_check("Refuted", "false", cls(ProofMap.Refuted));
  str_check("Incomplete", "unknown", cls(ProofMap.Incomplete));
  str_check("ProvenModulo", "modulo", cls(ProofMap.ProvenModulo([])));
  bool_check(
    "Refuted is distinct from Incomplete",
    true,
    cls(ProofMap.Refuted) != cls(ProofMap.Incomplete),
  );
  bool_check(
    "ProvenModulo is distinct from Proven and Incomplete",
    true,
    cls(ProofMap.ProvenModulo([])) != cls(ProofMap.Proven)
    && cls(ProofMap.ProvenModulo([])) != cls(ProofMap.Incomplete),
  );
};

/* The label carries the asterisk and the pending count, singular or
 * plural. */
let test_status_labels = () => {
  let (_, _, g) = repro_group();
  let pending = List.filter(Obligation.is_pending, g.proof);
  str_check("Proven", "proven true", ObligationsPanel.status_label(Proven));
  str_check("Refuted", "disproven", ObligationsPanel.status_label(Refuted));
  str_check(
    "Incomplete",
    "incomplete",
    ObligationsPanel.status_label(Incomplete),
  );
  str_check(
    "one pending",
    "proven* (1 pending obligation)",
    ObligationsPanel.status_label(ProvenModulo(pending)),
  );
  str_check(
    "two pending",
    "proven* (2 pending obligations)",
    ObligationsPanel.status_label(ProvenModulo(pending @ pending)),
  );
};

/* End to end: the repro theorem's goal reaches `true` but one obligation
 * is pending, so the chip is the ProvenModulo one — while the legacy
 * bool status (which `Theorems.get_score` still grades on) is unchanged
 * at `Some(true)`. */
let test_repro_status_is_proven_modulo = () => {
  let (pm, proof, _) = repro_group();
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProvenModulo(obs) =>
    int_check("exactly one pending", 1, List.length(obs))
  | Proven => Alcotest.fail("expected ProvenModulo, got Proven")
  | Refuted => Alcotest.fail("expected ProvenModulo, got Refuted")
  | Incomplete => Alcotest.fail("expected ProvenModulo, got Incomplete")
  };
  Alcotest.check(
    Alcotest.option(bool),
    "legacy status is untouched",
    Some(true),
    ProofMap.status_of_proof(pm, proof),
  );
};

/* A proof with no obligations at all is plain Proven; a hole is
 * Incomplete. */
let test_status_proven_and_incomplete = () => {
  let (pm, proof) =
    run({|theorem t = 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end in t|});
  bool_check(
    "obligation-free proof is Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
  let (pm, proof) = run({|theorem t = 1 + 1 == 2 proof ? in t|});
  bool_check(
    "hole proof is Incomplete",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Incomplete,
  );
};

/* --- grouping (proof tree vs definitions) ---------------------------- */

let test_group_proof_obligations = () => {
  let (_, _, g) = repro_group();
  int_check("three obligations in the proof", 3, List.length(g.proof));
  let classes = List.map(ObligationsPanel.discharge_class, g.proof);
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "pending first, then the receipts in proof order",
    ["pending", "evaluated", "remote"],
    classes,
  );
  switch (g.proof) {
  | [pending, evaluated, remote] =>
    check_exp("pending goal", "z != 0", pending.display_goal);
    check_exp("evaluated goal", "2 != 0", evaluated.display_goal);
    check_exp("remote goal", "y != 0", remote.display_goal);
  | _ => Alcotest.fail("unexpected obligation count")
  };
};

/* Definition-time obligations are keyed by the FUNCTION's id, so
 * `obligations_of_proof` never sees them; they show up in the
 * definitions group, and only them — no proof-step entry leaks in. */
let test_group_definition_obligations = () => {
  let (_, _, g) = repro_group();
  int_check("one definition obligation", 1, List.length(g.definitions));
  switch (g.definitions) {
  | [ob] =>
    check_exp("g's body condition", "w != 0", ob.display_goal);
    bool_check("and it is pending", true, Obligation.is_pending(ob));
  | _ => Alcotest.fail("unexpected definition obligation count")
  };
  /* Disjointness: nothing appears in both groups. */
  bool_check(
    "the two groups are disjoint",
    false,
    List.exists(
      (d: Obligation.t) =>
        List.exists(
          (p: Obligation.t) => Exp.fast_equal(p.goal, d.goal),
          g.proof,
        ),
      g.definitions,
    ),
  );
  bool_check(
    "group is non-empty",
    false,
    ObligationsPanel.group_is_empty(g),
  );
  bool_check(
    "the empty group is empty",
    true,
    ObligationsPanel.group_is_empty(ObligationsPanel.empty_group),
  );
};

/* Excluding the proof from the definition walk is what keeps a proof
 * step out of the definitions group: with no proofs excluded the shape
 * filter alone still finds exactly the definition entry here, and
 * excluding the theorem's proof does not remove it (it is not a proof
 * step). */
let test_definition_walk_excludes_proof_ids = () => {
  let (pm, proof, _) = repro_group();
  int_check(
    "shape filter alone",
    1,
    List.length(ProofMap.definition_obligations(pm)),
  );
  int_check(
    "with the proof excluded",
    1,
    List.length(ProofMap.definition_obligations(~proofs=[proof], pm)),
  );
  bool_check(
    "every id of the proof tree is a proof id",
    true,
    List.mem(Proof.rep_id(proof), ProofMap.rep_ids_of_proof(proof)),
  );
};

/* --- display ordering ----------------------------------------------- */

let mk_ob = (src: string, discharge): Obligation.t => {
  let goal = parse_exp(src);
  Obligation.{
    origin: Id.mk(),
    bindings: [],
    goal,
    display_goal: goal,
    discharge,
  };
};

let test_sort_pending_first_stable = () => {
  let obs = [
    mk_ob("1 != 0", Obligation.Evaluated),
    mk_ob("2 != 0", Obligation.Pending),
    mk_ob("3 != 0", Obligation.Remote(Id.mk())),
    mk_ob("4 != 0", Obligation.Pending),
  ];
  let sorted = ObligationsPanel.sort_for_display(obs);
  switch (sorted) {
  | [a, b, c, d] =>
    check_exp("first pending", "2 != 0", a.goal);
    check_exp("second pending", "4 != 0", b.goal);
    check_exp("then the receipts, in order", "1 != 0", c.goal);
    check_exp("then the receipts, in order", "3 != 0", d.goal);
  | _ => Alcotest.fail("sorting changed the obligation count")
  };
  int_check(
    "sorting preserves the count",
    List.length(obs),
    List.length(sorted),
  );
};

/* --- receipts (§4.2) ------------------------------------------------- */

/* A Remote discharge recovers its covering fact from the obligation's own
 * recorded bindings — the receipt the panel shows next to the goal. */
let test_remote_receipt_is_the_covering_fact = () => {
  let (_, _, g) = repro_group();
  let remote =
    List.find(
      (ob: Obligation.t) => ObligationsPanel.discharge_class(ob) == "remote",
      g.proof,
    );
  switch (Obligation.remote_fact(remote)) {
  | Some((name, fact)) =>
    str_check("the binder that covers it", "where", name);
    check_exp("the fact's statement", "y != 0", fact);
    str_check(
      "and the label names the source in prose",
      "discharged by a `where` restriction",
      ObligationsPanel.discharge_label(remote),
    );
  | None => Alcotest.fail("no receipt recovered for a Remote discharge")
  };
  /* Non-Remote discharges have no fact, and say why they are silent. */
  List.iter(
    (ob: Obligation.t) =>
      switch (ObligationsPanel.discharge_class(ob)) {
      | "evaluated" =>
        bool_check(
          "evaluated has no fact",
          true,
          Obligation.remote_fact(ob) == None,
        );
        str_check(
          "evaluated label",
          "discharged by evaluation",
          ObligationsPanel.discharge_label(ob),
        );
      | "pending" =>
        bool_check(
          "pending has no fact",
          true,
          Obligation.remote_fact(ob) == None,
        );
        str_check(
          "pending label",
          "pending — nothing in scope covers this",
          ObligationsPanel.discharge_label(ob),
        );
      | _ => ()
      },
    g.proof,
  );
};

/* --- readable goals (Obligation.display_goal) ----------------------- */

/* The documented friction: `incur_obligation` env-substitutes the goal
 * before running the discharge channels, so an obligation whose text
 * mentions a let-bound function ends up carrying that function's whole
 * body — a closure dump where the user wrote `d(y) != 0`.
 * `display_goal` keeps what was written. Both are recorded, and only the
 * latter is rendered. */
let test_display_goal_is_not_env_inlined = () => {
  let src = {|let d = fun q -> q + 1 in
theorem t = forall y -> 1 == 1
proof assume d(y) != 0 => axiom refl_eq at 0 on 1 == 1 end
in t|};
  let (pm, proof) = run(src);
  let obs = ProofMap.obligations_of_proof(pm, proof);
  int_check("one assumption obligation", 1, List.length(obs));
  switch (obs) {
  | [ob] =>
    check_exp(
      "display goal is what was written",
      "d(y) != 0",
      ob.display_goal,
    );
    bool_check(
      "the semantic goal has been env-substituted, so it differs",
      false,
      Exp.fast_equal(ob.goal, ob.display_goal),
    );
    /* Size is the whole point: the inlined goal is strictly bigger. */
    bool_check(
      "the printed form is smaller than the semantic one",
      true,
      String.length(Exp.show(ob.display_goal))
      < String.length(Exp.show(ob.goal)),
    );
  | _ => ()
  };
};

/* An obligation with nothing to inline is unaffected: display and
 * semantic goals agree. */
let test_display_goal_agrees_when_nothing_to_inline = () => {
  let (_, _, g) = repro_group();
  List.iter(
    (ob: Obligation.t) =>
      bool_check(
        "closed/parameter-only goals are printed as-is",
        true,
        Exp.fast_equal(ob.goal, ob.display_goal),
      ),
    g.proof,
  );
};

let tests = (
  "ObligationsPanel",
  [
    test_case(
      "the four statuses map to four distinct chips",
      `Quick,
      test_status_classes_distinct,
    ),
    test_case(
      "ProvenModulo's label is asterisked and counted",
      `Quick,
      test_status_labels,
    ),
    test_case(
      "one pending obligation makes a proven theorem ProvenModulo",
      `Quick,
      test_repro_status_is_proven_modulo,
    ),
    test_case(
      "obligation-free and hole proofs are Proven / Incomplete",
      `Quick,
      test_status_proven_and_incomplete,
    ),
    test_case(
      "proof-tree obligations, pending first",
      `Quick,
      test_group_proof_obligations,
    ),
    test_case(
      "definition-time obligations group separately",
      `Quick,
      test_group_definition_obligations,
    ),
    test_case(
      "the definition walk excludes proof-step ids",
      `Quick,
      test_definition_walk_excludes_proof_ids,
    ),
    test_case(
      "display ordering is pending-first and stable",
      `Quick,
      test_sort_pending_first_stable,
    ),
    test_case(
      "a Remote discharge shows its covering fact",
      `Quick,
      test_remote_receipt_is_the_covering_fact,
    ),
    test_case(
      "display_goal is the pre-substitution goal",
      `Quick,
      test_display_goal_is_not_env_inlined,
    ),
    test_case(
      "display_goal agrees with goal when nothing inlines",
      `Quick,
      test_display_goal_agrees_when_nothing_to_inline,
    ),
  ],
);
