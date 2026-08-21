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

let contains_substring = (haystack: string, needle: string): bool => {
  let hl = String.length(haystack);
  let nl = String.length(needle);
  let rec go = i =>
    i + nl <= hl && (String.sub(haystack, i, nl) == needle || go(i + 1));
  nl == 0 || go(0);
};

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

/* --- the (!) action menu (§3.3) -------------------------------------- */

/* Availability is computed from the obligation's own data plus the
 * theorem's syntax, so these tests run the real checker and read the real
 * float target. (The patch/round-trip side is Test_EditorTransform's: ids
 * are per-parse, so patches must be applied to the zipper they were built
 * from.) */

let stmt_of = (elab: Exp.t): option(Exp.t) => {
  let found = ref(None);
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Theorem(_, stmt, _, _) when found^ == None =>
      found := Some(stmt);
      e;
    | _ => continue(e)
    };
  let _ = TermBase.Exp.map_term(~f_exp, elab);
  found^;
};

let run_with_ctx =
    (src: string): (ProofMap.t, Proof.t, ObligationsPanel.action_ctx) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let proof = proof_of(elab);
  (
    EvaluatorState.get_proof_map(state),
    proof,
    ObligationsPanel.{
      stmt: stmt_of(elab),
      proof: Some(proof),
    },
  );
};

let pending_of = (pm, proof): list(Obligation.t) =>
  ProofMap.obligations_of_proof(pm, proof)
  |> List.filter(Obligation.is_pending);

let one_pending = (msg, pm, proof): Obligation.t =>
  switch (pending_of(pm, proof)) {
  | [ob] => ob
  | obs =>
    Alcotest.fail(
      msg
      ++ ": expected exactly one pending obligation, got "
      ++ string_of_int(List.length(obs)),
    )
  };

/* A pending obligation over a THEOREM binder floats: the target is that
 * binder, and all three exits are live. */
let test_float_available_at_theorem_binder = () => {
  let (pm, proof, ctx) = run_with_ctx(repro);
  let ob = one_pending("repro", pm, proof);
  switch (ObligationsPanel.float_target_of(~ctx, ob)) {
  | FloatTo(b) =>
    str_check(
      "the float target is the binder of the goal's variable",
      "z",
      String.concat(",", b.vars),
    )
  | UnsoundAtCase => Alcotest.fail("expected FloatTo, got UnsoundAtCase")
  | NoBinder => Alcotest.fail("expected FloatTo, got NoBinder")
  };
  let actions = ObligationsPanel.actions_of(~ctx, ob);
  int_check("three exits offered", 3, List.length(actions));
  bool_check(
    "all three are enabled",
    true,
    List.for_all((a: ObligationsPanel.action) => a.patch != None, actions),
  );
  str_check(
    "labels, in menu order",
    "Add to statement|Prove here|Split on it",
    String.concat(
      "|",
      List.map((a: ObligationsPanel.action) => a.label, actions),
    ),
  );
};

/* The innermost mentioned binder wins (§3.3: "the float target is
 * computed, not chosen"). Both `a` and `b` are theorem binders; the goal
 * mentions only `b`, so the guard lands on `b`'s binder. */
let inner_binder_src = {|theorem t = forall a: Int -> forall b: Int -> a + 8 / b == a + 8 / b
proof axiom refl_eq at 0 on a + 8 / b == a + 8 / b end
in t|};

let test_float_target_is_innermost = () => {
  let (pm, proof, ctx) = run_with_ctx(inner_binder_src);
  let ob = one_pending("inner binder", pm, proof);
  switch (ObligationsPanel.float_target_of(~ctx, ob)) {
  | FloatTo(b) =>
    str_check("target is the inner binder", "b", String.concat(",", b.vars))
  | UnsoundAtCase => Alcotest.fail("expected FloatTo, got UnsoundAtCase")
  | NoBinder => Alcotest.fail("expected FloatTo, got NoBinder")
  };
};

/* §3.3's UNSOUND cell: the obligation arises inside an induction case and
 * mentions that case's own binder, so floating it would put a restriction
 * on a case and break exhaustiveness. The option must be OFFERED and
 * DISABLED, with the reason — never silently dropped, never enabled. */
let case_scoped_src = {|theorem t = forall n: Int -> 8 / n == 8 / n
proof induction n | 0 => ? | m => axiom refl_eq at 0 on 8 / m == 8 / m end end
in t|};

let test_float_disabled_at_case_binder = () => {
  let (pm, proof, ctx) = run_with_ctx(case_scoped_src);
  let obs = pending_of(pm, proof);
  let ob =
    switch (
      List.find_opt(
        (ob: Obligation.t) =>
          ProofRule.occurs_free_any(["m"], Obligation.display_goal_of(ob)),
        obs,
      )
    ) {
    | Some(ob) => ob
    | None =>
      Alcotest.fail(
        "expected a pending obligation mentioning the case binder `m`, got "
        ++ string_of_int(List.length(obs)),
      )
    };
  bool_check(
    "the float target is the unsound one",
    true,
    ObligationsPanel.float_target_of(~ctx, ob) == UnsoundAtCase,
  );
  switch (ObligationsPanel.actions_of(~ctx, ob)) {
  | [float, prove, split] =>
    bool_check("float is disabled", true, float.patch == None);
    bool_check(
      "and says why",
      true,
      contains_substring(float.title, "unsound"),
    );
    /* The other two exits stay available at a case (§3.3's table). */
    bool_check("prove here is available", true, prove.patch != None);
    bool_check("split is available", true, split.patch != None);
  | actions =>
    Alcotest.fail(
      "expected three actions, got " ++ string_of_int(List.length(actions)),
    )
  };
  /* And the wrapping region is INSIDE the case, so the emitted `have`'s
   * proposition can mention `m` at all. */
  switch (ObligationsPanel.region_of(~ctx, ob)) {
  | Some(region) =>
    bool_check(
      "the region is the case body, not the whole proof",
      false,
      Id.compare(Proof.rep_id(region), Proof.rep_id(proof)) == 0,
    );
    bool_check(
      "and it contains the incurring step",
      true,
      ObligationsPanel.proof_contains(~origin=ob.origin, region),
    );
  | None => Alcotest.fail("no wrapping region found for a case obligation")
  };
};

/* Discharged rows carry no menu: they are receipts. */
let test_no_actions_on_discharged_rows = () => {
  let (pm, proof, ctx) = run_with_ctx(repro);
  let discharged =
    ProofMap.obligations_of_proof(pm, proof)
    |> List.filter(ob => !Obligation.is_pending(ob));
  bool_check(
    "the repro has discharged rows to check",
    true,
    List.length(discharged) > 0,
  );
  List.iter(
    ob =>
      int_check(
        "no actions on a discharged obligation",
        0,
        List.length(ObligationsPanel.actions_of(~ctx, ob)),
      ),
    discharged,
  );
};

/* With no syntax to act on — the definition-time section, whose
 * obligations belong to no proof tree — float reports NoBinder and the
 * wrapping actions are unavailable rather than misdirected. */
let test_no_context_degrades = () => {
  let (pm, proof, _) = run_with_ctx(repro);
  let ob = one_pending("repro", pm, proof);
  let ctx = ObligationsPanel.no_action_ctx;
  bool_check(
    "no statement means nothing to float onto",
    true,
    ObligationsPanel.float_target_of(~ctx, ob) == NoBinder,
  );
  switch (ObligationsPanel.actions_of(~ctx, ob)) {
  | [float, prove, split] =>
    bool_check("float disabled", true, float.patch == None);
    bool_check("prove here disabled", true, prove.patch == None);
    bool_check("split disabled", true, split.patch == None);
  | actions =>
    Alcotest.fail(
      "expected three actions, got " ++ string_of_int(List.length(actions)),
    )
  };
};

let tests = (
  "ObligationsPanel",
  [
    test_case(
      "float is available at a theorem binder, all three exits live",
      `Quick,
      test_float_available_at_theorem_binder,
    ),
    test_case(
      "the float target is the innermost mentioned binder",
      `Quick,
      test_float_target_is_innermost,
    ),
    test_case(
      "float is greyed out (with reason) at an induction-case binder",
      `Quick,
      test_float_disabled_at_case_binder,
    ),
    test_case(
      "discharged rows carry no action menu",
      `Quick,
      test_no_actions_on_discharged_rows,
    ),
    test_case(
      "no syntax context degrades to disabled actions",
      `Quick,
      test_no_context_degrades,
    ),
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
