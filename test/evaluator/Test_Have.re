open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;

/* The `have` proof form, `have <exp> proof <subproof> => <body>`: the
 * cut rule, and the obligations panel's "prove here" exit.
 *
 * Semantics under test (`ProofCheck.check_have`). Have is a two-premise
 * rule: with incoming goal G it incurs two obligations on its own node,
 * one per premise, each retired by its own attached proof.
 *   - premise 1's goal is <exp>, checked in the enclosing scope (it
 *     can't cite the have's own hypothesis). Dropped when the subproof
 *     closes it (`true`, clean subtree); otherwise it is discharged if it
 *     can be, so an already-known <exp> goes Remote/Evaluated with no
 *     subproof written;
 *   - premise 2's goal is G, in the scope extended with <exp> under the
 *     name "have". Dropped when the body closes it; otherwise Pending,
 *     never discharged by lookup, which would trivially succeed against
 *     the new `have` whenever G equals <exp>;
 *   - that hypothesis is installed whether or not the subproof is done,
 *     so an obligation incurred by a step inside the body discharges
 *     Remote by lookup as soon as the wrapper is written;
 *   - outgoing is the body's, and mark-free: an open premise is an
 *     incomplete proof, not a broken step. */

let int_check = (msg, expected, actual) =>
  Alcotest.check(Alcotest.int, msg, expected, actual);

let bool_check = (msg, expected, actual) =>
  Alcotest.check(Alcotest.bool, msg, expected, actual);

let dump_obligations = (obs: list(Obligation.t)): string =>
  obs
  |> List.map((ob: Obligation.t) =>
       "  "
       ++ String.map(c => c == '\n' ? ' ' : c, print_exp(ob.goal))
       ++ " -> "
       ++ Obligation.show_discharge(ob.discharge)
     )
  |> String.concat("\n");

/* The shared shape: a theorem whose statement leaves `x` unrestricted, so
 * a `have` about `x` has an open goal, which closed evaluation can't
 * discharge. That is what makes the difference between a
 * proven and an unproven subproof visible in the obligation list. */
let with_have = (subproof: string): string =>
  "theorem t = forall x: Int -> x == x proof have x == x proof "
  ++ subproof
  ++ " => from x == x do rewrite refl end end in t";

/* --- 1. a proven subproof discharges the have's obligation ----------- */

let test_proven_subproof_discharges = () => {
  let (pm, proof) = run(with_have("from x == x do rewrite refl end end"));
  let obs = ProofMap.obligations_of_proof(pm, proof);
  if (obs != []) {
    Alcotest.fail(
      "a proven `have` subproof should leave NO obligation, got:\n"
      ++ dump_obligations(obs),
    );
  };
  check_mark_free("have with a proven subproof", pm, proof);
  bool_check(
    "the theorem is plain Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- 2. an incomplete subproof leaves the obligation pending --------- */

let test_incomplete_subproof_pending = () => {
  let (pm, proof) = run(with_have("?"));
  let obs = ProofMap.obligations_of_proof(pm, proof);
  int_check(
    "exactly one obligation — the have's own:\n" ++ dump_obligations(obs),
    1,
    List.length(obs),
  );
  bool_check(
    "and it is pending:\n" ++ dump_obligations(obs),
    true,
    List.for_all(Obligation.is_pending, obs),
  );
  /* Mark-free: an unfinished subproof is an incomplete proof, not a
   * broken step. */
  check_mark_free("have with a hole subproof", pm, proof);
  /* The goal reached `true` granted the pending obligation. */
  bool_check(
    "status is ProvenModulo",
    true,
    switch (ProofMap.full_status_of_proof(pm, proof)) {
    | ProvenModulo(_) => true
    | Proven
    | Refuted
    | Incomplete => false
    },
  );
};

/* --- 3. the hypothesis is citable from the body ---------------------- */

/* The point of the "prove here" action: an obligation incurred inside the
 * body discharges against the have's hypothesis by lookup even while
 * the subproof is still a hole. `8 / z` in the goal makes the axiom step
 * incur `z != 0`, and nothing else in scope covers it. */
let citable_src = {|theorem t = forall z: Int -> 8 / z == 8 / z
proof have z != 0 proof ? => from 8 / z == 8 / z do rewrite refl end end
in t|};

let test_hypothesis_citable_in_body = () => {
  let (pm, proof) = run(citable_src);
  let obs = ProofMap.obligations_of_proof(pm, proof);
  let dump = dump_obligations(obs);
  /* Two obligations: the body's `z != 0` (Remote, against the have) and
   * the have's own `z != 0` (Pending, until the subproof is written). */
  int_check("two obligations:\n" ++ dump, 2, List.length(obs));
  let remotes =
    List.filter(
      (ob: Obligation.t) =>
        switch (ob.discharge) {
        | Remote(_) => true
        | _ => false
        },
      obs,
    );
  int_check(
    "the body's obligation is discharged Remote:\n" ++ dump,
    1,
    List.length(remotes),
  );
  /* The receipt names the hypothesis the checker installed. */
  switch (remotes) {
  | [ob] =>
    switch (Obligation.remote_fact(ob)) {
    | Some((name, _)) =>
      bool_check(
        "receipt names a `have` hypothesis, got " ++ name,
        true,
        String.length(name) >= 4 && String.sub(name, 0, 4) == "have",
      )
    | None => Alcotest.fail("Remote discharge with no recoverable receipt")
    }
  | _ => ()
  };
  int_check(
    "the have itself still carries one pending obligation:\n" ++ dump,
    1,
    List.length(List.filter(Obligation.is_pending, obs)),
  );
  check_mark_free("have citable in body", pm, proof);
};

/* --- 4. the subproof may not cite the have's own hypothesis ---------- */

/* Soundness guard: were the subproof checked in the extended scope, `have
 * P proof <cite P> => ...` would prove anything. The subproof is checked
 * in the enclosing scope, so citing `have` there is an unknown fact. */
let self_cite_src = {|theorem t = forall x: Int -> x == x
proof have x == 1 proof from x do rewrite have end end => from x == x do rewrite refl end end
in t|};

let test_subproof_cannot_self_cite = () => {
  let (pm, proof) = run(self_cite_src);
  bool_check(
    "citing the have's own hypothesis inside its subproof does not prove it",
    false,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
  let obs = ProofMap.obligations_of_proof(pm, proof);
  bool_check(
    "the have's obligation is still pending:\n" ++ dump_obligations(obs),
    true,
    List.exists(Obligation.is_pending, obs),
  );
};

/* --- 5. round-trip: the form survives print/parse -------------------- */

let test_form_roundtrips = () => {
  let src = with_have("?");
  let printed = print_exp(parse_exp(src));
  bool_check(
    "printed program still contains the have form: " ++ printed,
    true,
    contains(printed, "have"),
  );
  /* Re-parsing the printed text yields the same proof term. */
  let p1 = proof_of(parse_exp(src));
  let p2 = proof_of(parse_exp(printed));
  bool_check(
    "reparse is proof-equal:\n" ++ printed,
    true,
    Proof.fast_equal(p1, p2),
  );
};

let split_probe_src = {|theorem t = forall z: Int -> 8 / z == 8 / z
proof induction z != 0 | true => from 8 / z == 8 / z do rewrite refl end end | false => ? end
in t|};

/* The obligations panel's split exit, end to end: in the `true` branch
 * the case equation `(z != 0) == true` covers the branch's `z != 0`
 * obligation by lookup (modulo `ProofCheck.Discharge.strip_eq_true`). The `false`
 * branch is left to the user, so the theorem is Incomplete. */
let test_split_discharges_in_true_branch = () => {
  let (pm, proof) = run(split_probe_src);
  let obs = ProofMap.obligations_of_proof(pm, proof);
  let dump = dump_obligations(obs);
  int_check("one obligation:\n" ++ dump, 1, List.length(obs));
  bool_check(
    "discharged against the case equation:\n" ++ dump,
    true,
    List.for_all(
      (ob: Obligation.t) =>
        switch (ob.discharge) {
        | Remote(_) => true
        | Evaluated
        | Pending => false
        },
      obs,
    ),
  );
  switch (obs) {
  | [ob] =>
    switch (Obligation.remote_fact(ob)) {
    | Some((name, _)) =>
      bool_check(
        "the receipt names the case equation, got " ++ name,
        true,
        String.length(name) >= 7 && String.sub(name, 0, 7) == "case_eq",
      )
    | None => Alcotest.fail("Remote discharge with no recoverable receipt")
    }
  | _ => ()
  };
};

/* The (first) `have` node in a proof term. The theorem's proof root is
 * not necessarily the have itself (binder peeling, `;` chains), and the
 * obligation's `origin` is the have's own rep_id. */
let rec find_have = (p: Proof.t): option(Proof.t) =>
  switch (p.term) {
  | Have(_, _, _) => Some(p)
  | Seq(a, b) =>
    switch (find_have(a)) {
    | Some(_) as r => r
    | None => find_have(b)
    }
  | Forall(_, b)
  | Funext(_, b)
  | Assume(_, _, b)
  | Alias(_, _, b)
  | Generalize(_, b)
  | Revert(_, _, b)
  | Parens(b) => find_have(b)
  | Induction(_, _, cases) =>
    List.fold_left(
      (acc, (_, b)) =>
        switch (acc) {
        | Some(_) => acc
        | None => find_have(b)
        },
      None,
      cases,
    )
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | AxiomStep(_)
  | AlgebriteStep(_)
  | EvalStep
  | Contradiction(_) => None
  | Focus({body, _}) => find_have(body)
  };

/* --- 6. both premises open: two obligations on the have node --------- */

/* Have is a two-premise rule, so `have P proof ? => ?` on goal G carries
 * both premises as obligations of the have step itself, not only as
 * nested proof incompleteness. `P` here is `x == 2`, which is neither in
 * the theorem's context nor closed, so neither lookup nor closed
 * evaluation can discharge premise 1. */
let two_holes_src = {|theorem t = forall x: Int -> x == x
proof have x == 2 proof ? => ?
in t|};

let test_two_open_premises = () => {
  let (pm, proof) = run(two_holes_src);
  let obs = ProofMap.obligations_of_proof(pm, proof);
  let dump = dump_obligations(obs);
  int_check(
    "two obligations, one per premise:\n" ++ dump,
    2,
    List.length(obs),
  );
  /* Both are recorded on the have node, so the obligations panel groups
   * them under this step, as it does a two-condition axiom's. */
  let have_id =
    switch (find_have(proof)) {
    | Some(h) => Proof.rep_id(h)
    | None => Alcotest.fail("no `have` node in the proof term")
    };
  bool_check(
    "both obligations originate at the have step:\n" ++ dump,
    true,
    List.for_all(
      (ob: Obligation.t) => Id.compare(ob.origin, have_id) == 0,
      obs,
    ),
  );
  bool_check(
    "both are pending:\n" ++ dump,
    true,
    List.for_all(Obligation.is_pending, obs),
  );
  /* Premise 1 is `<exp>`; premise 2 is the outer goal. */
  bool_check(
    "one obligation is the proposition:\n" ++ dump,
    true,
    List.exists(
      (ob: Obligation.t) =>
        Exp.fast_equal(ob.goal, elaborated_exp("x == 2")),
      obs,
    ),
  );
  bool_check(
    "the other is the incoming goal:\n" ++ dump,
    true,
    List.exists(
      (ob: Obligation.t) =>
        Exp.fast_equal(ob.goal, elaborated_exp("x == x")),
      obs,
    ),
  );
  /* Premise 2's local context is the extended one: the panel must show
   * the `have` hypothesis the premise is allowed to use. */
  switch (
    List.find_opt(
      (ob: Obligation.t) =>
        Exp.fast_equal(ob.goal, elaborated_exp("x == x")),
      obs,
    )
  ) {
  | Some(ob) =>
    bool_check(
      "the body premise's bindings carry the `have` hypothesis",
      true,
      List.exists(
        (e: Ctx.entry) =>
          switch (e) {
          | TheoremEntry({name, _}) => name == "have"
          | _ => false
          },
        ob.bindings,
      ),
    )
  | None => ()
  };
  check_mark_free("have with two hole premises", pm, proof);
  /* Not ProvenModulo: with the body open the goal never reaches `true`,
   * so a pending second premise cannot masquerade as "proven modulo". */
  bool_check(
    "status is Incomplete",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Incomplete,
  );
};

/* An open body premise with a closed subproof still leaves premise 2. */
let test_body_premise_alone = () => {
  let (pm, proof) =
    run(
      {|theorem t = forall x: Int -> x == x
proof have 1 == 1 proof from 1 == 1 do rewrite refl end end => ?
in t|},
    );
  let obs = ProofMap.obligations_of_proof(pm, proof);
  let dump = dump_obligations(obs);
  int_check("only the body premise remains:\n" ++ dump, 1, List.length(obs));
  bool_check(
    "and it is the incoming goal:\n" ++ dump,
    true,
    switch (obs) {
    | [ob] => Exp.fast_equal(ob.goal, elaborated_exp("x == x"))
    | _ => false
    },
  );
};

let tests = (
  "Evaluator.Have",
  [
    test_case(
      "proven subproof discharges the obligation",
      `Quick,
      test_proven_subproof_discharges,
    ),
    test_case(
      "two open premises are two obligations on the have",
      `Quick,
      test_two_open_premises,
    ),
    test_case(
      "a closed subproof still leaves the body premise",
      `Quick,
      test_body_premise_alone,
    ),
    test_case(
      "incomplete subproof leaves it pending",
      `Quick,
      test_incomplete_subproof_pending,
    ),
    test_case(
      "hypothesis is citable in the body",
      `Quick,
      test_hypothesis_citable_in_body,
    ),
    test_case(
      "subproof cannot cite the have itself",
      `Quick,
      test_subproof_cannot_self_cite,
    ),
    test_case(
      "form round-trips through the printer",
      `Quick,
      test_form_roundtrips,
    ),
    test_case(
      "split discharges the obligation in the true branch",
      `Quick,
      test_split_discharges_in_true_branch,
    ),
  ],
);
