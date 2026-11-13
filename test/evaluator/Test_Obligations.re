open Alcotest;
open Language;
open Test_Evaluator_Prelude;

module Status = Web.Obligations.Status;
module Exits = Web.Obligations.Exits;

/* A theorem's obligations as its header and side-condition circles show
 * them: the status and its counter of open obligations, the exits that
 * discharge a pending one, and the goals as written.
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

/* A sample program with one obligation per discharge state: one theorem
 * with three obligations in its proof, and one definition-time obligation
 * from the unguarded `g`:
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
proof from f(y) + 4 / 2 + 8 / z == f(y) + 4 / 2 + 8 / z do axiom refl_eq end end
in t|};

/* --- status mapping ----------------------------------------------- */

/* The four statuses get four distinct chip classes; ProvenModulo is
 * neither the proven nor the incomplete one. */
let test_status_classes_distinct = () => {
  let cls = Status.class_;
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

/* ProvenModulo reads as proven: its class and the header's counter set
 * it apart. */
let test_status_labels = () => {
  str_check("Proven", "proven true", Status.label(Proven));
  str_check("ProvenModulo", "proven true", Status.label(ProvenModulo([])));
  str_check("Refuted", "disproven", Status.label(Refuted));
  str_check("Incomplete", "incomplete", Status.label(Incomplete));
};

/* The counter counts the proof's pending obligations only: the repro's
 * pending definition-time obligation, at `g`, isn't the theorem's. */
let test_open_count = () => {
  let (pm, proof) = run(repro);
  int_check("one open obligation", 1, Status.open_count(pm, proof));
  let (pm, proof) =
    run(
      {|theorem t = 1 == 1 proof from 1 == 1 do axiom refl_eq end end in t|},
    );
  int_check(
    "none in an obligation-free proof",
    0,
    Status.open_count(pm, proof),
  );
};

/* End to end: the repro theorem's goal reaches `true` but one obligation
 * is pending, so the chip is the ProvenModulo one — while the bool status
 * (which `Theorems.get_score` grades on) stays `Some(true)`. */
let test_repro_status_is_proven_modulo = () => {
  let (pm, proof) = run(repro);
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProvenModulo(obs) =>
    int_check("exactly one pending", 1, List.length(obs))
  | Proven => Alcotest.fail("expected ProvenModulo, got Proven")
  | Refuted => Alcotest.fail("expected ProvenModulo, got Refuted")
  | Incomplete => Alcotest.fail("expected ProvenModulo, got Incomplete")
  };
  Alcotest.check(
    Alcotest.option(bool),
    "bool status is untouched",
    Some(true),
    ProofMap.status_of_proof(pm, proof),
  );
};

/* A proof with no obligations at all is plain Proven; a hole is
 * Incomplete. */
let test_status_proven_and_incomplete = () => {
  let (pm, proof) =
    run(
      {|theorem t = 1 == 1 proof from 1 == 1 do axiom refl_eq end end in t|},
    );
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

/* --- covering facts ------------------------------------------------- */

/* A Remote discharge recovers its covering fact from the obligation's own
 * recorded bindings: an open closed circle's justification names it. */
let test_remote_fact_is_the_covering_fact = () => {
  let (pm, proof) = run(repro);
  let obs = ProofMap.obligations_of_proof(pm, proof);
  List.iter(
    (ob: Obligation.t) =>
      switch (ob.discharge, Obligation.remote_fact(ob)) {
      | (Remote(_), Some((name, fact))) =>
        str_check("the binder that covers it", "where", name);
        check_exp("the fact's statement", "y != 0", fact);
      | (Remote(_), None) =>
        Alcotest.fail("no covering fact recovered for a Remote discharge")
      | (Evaluated | Pending, fact) =>
        bool_check("only a Remote discharge has a fact", true, fact == None)
      },
    obs,
  );
  bool_check(
    "the repro has a Remote discharge",
    true,
    List.exists(
      (ob: Obligation.t) => Obligation.remote_fact(ob) != None,
      obs,
    ),
  );
};

/* --- readable goals ------------------------------------------------ */

/* An obligation's goal is kept as written, naming the function rather
 * than carrying its body. */
let test_goal_is_as_written = () => {
  let src = {|let d = fun q -> q + 1 in
theorem t = forall y -> 1 == 1
proof assume d(y) != 0 => from 1 == 1 do axiom refl_eq end end
in t|};
  let (pm, proof) = run(src);
  let obs = ProofMap.obligations_of_proof(pm, proof);
  int_check("one assumption obligation", 1, List.length(obs));
  switch (obs) {
  | [ob] => check_exp("the goal is what was written", "d(y) != 0", ob.goal)
  | _ => ()
  };
};

/* --- a step's exits ------------------------------------------------- */

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

let run_with_ctx = (src: string): (ProofMap.t, Proof.t, Exits.ctx) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let proof = proof_of(elab);
  (
    EvaluatorState.get_proof_map(state),
    proof,
    {
      stmt: stmt_of(elab),
      proof,
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

/* A pending obligation over a theorem binder floats: the target is that
 * binder, and all three exits are live. */
let test_float_available_at_theorem_binder = () => {
  let (pm, proof, ctx) = run_with_ctx(repro);
  let ob = one_pending("repro", pm, proof);
  switch (Exits.float_target_of(~ctx, ob)) {
  | FloatTo(b) =>
    str_check(
      "the float target is the binder of the goal's variable",
      "z",
      String.concat(",", b.vars),
    )
  | UnsoundAtCase => Alcotest.fail("expected FloatTo, got UnsoundAtCase")
  | NoBinder => Alcotest.fail("expected FloatTo, got NoBinder")
  };
  let actions = Exits.for_step(~ctx, ob);
  int_check("three exits offered", 3, List.length(actions));
  bool_check(
    "all three are enabled",
    true,
    List.for_all((a: Exits.t) => a.patch != None, actions),
  );
  str_check(
    "labels, in menu order",
    "Add to statement|Prove here|Split on it",
    String.concat("|", List.map((a: Exits.t) => a.label, actions)),
  );
};

/* The innermost mentioned binder wins: the float target is computed, not
 * chosen. Both `a` and `b` are theorem binders; the goal mentions only
 * `b`, so the guard lands on `b`'s binder. */
let inner_binder_src = {|theorem t = forall a: Int -> forall b: Int -> a + 8 / b == a + 8 / b
proof from a + 8 / b == a + 8 / b do axiom refl_eq end end
in t|};

let test_float_target_is_innermost = () => {
  let (pm, proof, ctx) = run_with_ctx(inner_binder_src);
  let ob = one_pending("inner binder", pm, proof);
  switch (Exits.float_target_of(~ctx, ob)) {
  | FloatTo(b) =>
    str_check("target is the inner binder", "b", String.concat(",", b.vars))
  | UnsoundAtCase => Alcotest.fail("expected FloatTo, got UnsoundAtCase")
  | NoBinder => Alcotest.fail("expected FloatTo, got NoBinder")
  };
};

/* The unsound float: the obligation arises inside an induction case and
 * mentions that case's own binder, so floating it would put a restriction
 * on a case and break exhaustiveness. The option must be offered and
 * disabled, with the reason — never silently dropped, never enabled. */
let case_scoped_src = {|theorem t = forall n: Int -> 8 / n == 8 / n
proof induction n | 0 => ? | m => from 8 / m == 8 / m do axiom refl_eq end end end
in t|};

let test_float_disabled_at_case_binder = () => {
  let (pm, proof, ctx) = run_with_ctx(case_scoped_src);
  let obs = pending_of(pm, proof);
  let ob =
    switch (
      List.find_opt(
        (ob: Obligation.t) => ProofRule.occurs_free_any(["m"], ob.goal),
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
    Exits.float_target_of(~ctx, ob) == UnsoundAtCase,
  );
  switch (Exits.for_step(~ctx, ob)) {
  | [float, prove, split] =>
    bool_check("float is disabled", true, float.patch == None);
    bool_check(
      "and says why",
      true,
      contains_substring(float.title, "unsound"),
    );
    /* The other two exits stay available at a case. */
    bool_check("prove here is available", true, prove.patch != None);
    bool_check("split is available", true, split.patch != None);
  | actions =>
    Alcotest.fail(
      "expected three actions, got " ++ string_of_int(List.length(actions)),
    )
  };
  /* And the wrapping region is inside the case, so the emitted `have`'s
   * proposition can mention `m` at all. */
  switch (Exits.region_of(~ctx, ob)) {
  | Some(region) =>
    bool_check(
      "the region is the case body, not the whole proof",
      false,
      Id.compare(Proof.rep_id(region), Proof.rep_id(proof)) == 0,
    );
    bool_check(
      "and it contains the incurring step",
      true,
      Exits.proof_contains(~origin=ob.origin, region),
    );
  | None => Alcotest.fail("no wrapping region found for a case obligation")
  };
};

/* A discharged obligation has no exits. */
let test_no_exits_when_discharged = () => {
  let (pm, proof, ctx) = run_with_ctx(repro);
  let discharged =
    ProofMap.obligations_of_proof(pm, proof)
    |> List.filter(ob => !Obligation.is_pending(ob));
  bool_check(
    "the repro has discharged obligations to check",
    true,
    List.length(discharged) > 0,
  );
  List.iter(
    ob =>
      int_check(
        "no exits on a discharged obligation",
        0,
        List.length(Exits.for_step(~ctx, ob)),
      ),
    discharged,
  );
};

/* Against a theorem whose statement has no binder and whose proof
 * doesn't hold the incurring step, float reports NoBinder and the wrapping
 * exits are disabled, with a reason, rather than misdirected. */
let test_exits_disabled_outside_the_theorem = () => {
  let (pm, proof, _) = run_with_ctx(repro);
  let ob = one_pending("repro", pm, proof);
  let ctx: Exits.ctx = {
    stmt: None,
    proof: Proof.fresh(EmptyHole),
  };
  bool_check(
    "no statement means nothing to float onto",
    true,
    Exits.float_target_of(~ctx, ob) == NoBinder,
  );
  switch (Exits.for_step(~ctx, ob)) {
  | [float, prove, split] =>
    bool_check("float disabled", true, float.patch == None);
    bool_check("prove here disabled", true, prove.patch == None);
    bool_check("split disabled", true, split.patch == None);
    bool_check(
      "and says why",
      true,
      contains_substring(prove.title, "Unavailable"),
    );
  | actions =>
    Alcotest.fail(
      "expected three actions, got " ++ string_of_int(List.length(actions)),
    )
  };
};

/* --- exit 1 end to end: float, reparse, re-check -------------------
 *
 * The unit tests above only inspect the patch; this one drives the whole
 * loop — check → patch → print → reparse → check — and asserts that the
 * floated restriction discharges the obligation, not just that the binder
 * was edited.
 *
 * The patch target ids come from the zipper (per-parse ids), so the
 * exits' ctx is built from the zipper's own term; the obligation is the
 * real one from a checker run on the same source, so its display_goal is
 * exactly what the browser would embed. */

let zipper_of = (s: string): Haz3lcore.Zipper.t =>
  switch (Haz3lcore.Parser.to_zipper(s, ~root=Exp)) {
  | Some(z) => z
  | None => Alcotest.fail("failed to parse zipper: " ++ s)
  };

let serialize = (z: Haz3lcore.Zipper.t): string =>
  Haz3lcore.Printer.of_zipper(~holes="?", z);

let zipper_term = (z: Haz3lcore.Zipper.t): Exp.t =>
  Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term;

let ctx_of_zipper = (z: Haz3lcore.Zipper.t): Exits.ctx => {
  let term = zipper_term(z);
  {
    stmt: stmt_of(term),
    proof:
      switch (Test_ProofMap.find_theorem_proof(term)) {
      | Some(p) => p
      | None => Alcotest.fail("no theorem proof in the zipper term")
      },
  };
};

/* `8 / z` incurs a pending `z != 0`; floating it restricts `z`'s binder. */
let float_e2e_src = {|let f = fun x where x != 0 -> 100 / x in
let g = fun w -> 100 / w in
theorem t = forall y where y != 0 -> forall z -> f(y) + 4 / 2 + 8 / z == f(y) + 4 / 2 + 8 / z
proof from f(y) + 4 / 2 + 8 / z == f(y) + 4 / 2 + 8 / z do axiom refl_eq end end
in 0|};

let test_float_discharges_after_recheck = () => {
  /* 1. check the original: one pending obligation, `z != 0`. */
  let (pm, proof, _) = run_with_ctx(float_e2e_src);
  let ob = one_pending("the repro", pm, proof);
  check_exp("the pending obligation is z != 0", "z != 0", ob.goal);

  /* 2. build and apply the float patch against the editor's own zipper. */
  let z = zipper_of(float_e2e_src);
  let ctx = ctx_of_zipper(z);
  let patch =
    switch (Exits.float_patch(~ctx, ob)) {
    | Some(p) => p
    | None => Alcotest.fail("float patch unavailable for the repro")
    };
  let out = Haz3lcore.EditorTransform.apply_patch(z, patch) |> serialize;
  bool_check(
    "the binder carries a where-restriction — got:\n" ++ out,
    true,
    contains_substring(out, "where"),
  );

  /* 3. re-check the patched program: the obligation must be Remote. */
  let (pm', proof') = run(out);
  let obs' = ProofMap.obligations_of_proof(pm', proof');
  let pendings = obs' |> List.filter(Obligation.is_pending) |> List.length;
  int_check(
    "no pending obligations after the float — patched program:\n"
    ++ out
    ++ "\nobligations: "
    ++ String.concat(
         ", ",
         List.map(
           (o: Obligation.t) =>
             Obligation.show_discharge(o.discharge)
             ++ " "
             ++ Test_ProofMap.print_exp(o.goal),
           obs',
         ),
       ),
    0,
    pendings,
  );
  bool_check(
    "the floated condition is discharged Remote",
    true,
    List.exists(
      (o: Obligation.t) =>
        switch (o.discharge) {
        | Obligation.Remote(_) => true
        | _ => false
        },
      obs',
    ),
  );
  switch (ProofMap.full_status_of_proof(pm', proof')) {
  | ProofMap.Proven => ()
  | other =>
    Alcotest.fail(
      "expected the theorem chip fully Proven, got: "
      ++ ProofMap.show_full_status(other)
      ++ "\npatched program:\n"
      ++ out,
    )
  };
};

/* --- a contract's exit ----------------------------------------------- */

/* Definition-time obligations sit at the function's id, in an entry with
 * no goals and no marks. */
let contract_obligations = (pm: ProofMap.t): list(Obligation.t) =>
  Id.Map.fold(
    (_, entry: ProofMap.entry, acc) =>
      switch (entry) {
      | {incoming: None, outgoing: None, marks: [], obligations, _} =>
        acc @ obligations
      | _ => acc
      },
    pm,
    [],
  );

/* The first `fun` in the term. */
let first_fun = (e: Exp.t): option(Exp.t) => {
  let found = ref(None);
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Fun(_) when found^ == None =>
      found := Some(e);
      e;
    | _ => continue(e)
    };
  let _ = TermBase.Exp.map_term(~f_exp, e);
  found^;
};

let contract_src = {|let g = fun w -> 100 / w in
theorem t = 1 == 1 proof from 1 == 1 do axiom refl_eq end end
in 0|};

let one_contract = (src: string): Obligation.t => {
  let (pm, _) = run(src);
  switch (contract_obligations(pm)) {
  | [ob] => ob
  | obs =>
    Alcotest.fail(
      "expected one contract obligation, got "
      ++ string_of_int(List.length(obs)),
    )
  };
};

/* Without the function's syntax (it is in another cell), the one exit
 * shows disabled, saying why. */
let test_contract_exit_needs_the_function = () => {
  let ob = one_contract(contract_src);
  switch (Exits.for_contract(~func=None, ob)) {
  | [exit] =>
    str_check("the exit", "Add to contract", exit.label);
    bool_check("disabled", true, exit.patch == None);
    bool_check(
      "and says why",
      true,
      contains_substring(exit.title, "Unavailable"),
    );
  | exits =>
    Alcotest.fail(
      "expected one exit, got " ++ string_of_int(List.length(exits)),
    )
  };
};

/* Check, patch with Add to contract, reparse, check again: the guard
 * covers the body, so no contract obligation is left. */
let test_contract_exit_discharges_after_recheck = () => {
  let ob = one_contract(contract_src);
  check_exp("the contract's goal", "w != 0", ob.goal);
  let z = zipper_of(contract_src);
  let patch =
    switch (Exits.for_contract(~func=first_fun(zipper_term(z)), ob)) {
    | [{patch: Some(p), _}] => p
    | _ => Alcotest.fail("Add to contract unavailable")
    };
  let out = Haz3lcore.EditorTransform.apply_patch(z, patch) |> serialize;
  bool_check(
    "the parameter carries the guard — got:\n" ++ out,
    true,
    contains_substring(out, "fun w where w != 0"),
  );
  let (pm', _) = run(out);
  int_check(
    "no contract obligation after the patch — patched program:\n" ++ out,
    0,
    List.length(contract_obligations(pm')),
  );
};

let tests = (
  "Obligations",
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
      "a discharged obligation has no exits",
      `Quick,
      test_no_exits_when_discharged,
    ),
    test_case(
      "exits outside the theorem are disabled, with a reason",
      `Quick,
      test_exits_disabled_outside_the_theorem,
    ),
    test_case(
      "Add to contract is disabled without the function's syntax",
      `Quick,
      test_contract_exit_needs_the_function,
    ),
    test_case(
      "Add to contract: no contract obligation after reparse and re-check",
      `Quick,
      test_contract_exit_discharges_after_recheck,
    ),
    test_case(
      "the four statuses map to four distinct chips",
      `Quick,
      test_status_classes_distinct,
    ),
    test_case(
      "ProvenModulo's label reads as proven",
      `Quick,
      test_status_labels,
    ),
    test_case(
      "the counter counts the proof's pending obligations",
      `Quick,
      test_open_count,
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
      "a Remote discharge has its covering fact",
      `Quick,
      test_remote_fact_is_the_covering_fact,
    ),
    test_case(
      "an obligation's goal is as written",
      `Quick,
      test_goal_is_as_written,
    ),
    test_case(
      "float patch: obligation discharges after reparse and re-check",
      `Quick,
      test_float_discharges_after_recheck,
    ),
  ],
);
