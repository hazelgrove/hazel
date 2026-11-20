open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Poly;

module Status = Web.Obligations.Status;
module Exits = Web.Obligations.Exits;

/* A theorem's obligations as its header and side-condition circles show
 * them: the status, with its count of open conditions, the exits that
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
    i
    + nl <= hl
    && (String.sub(haystack, ~pos=i, ~len=nl) == needle || go(i + 1));
  nl == 0 || go(0);
};

let str_check = (msg, expected, actual) =>
  Alcotest.check(Alcotest.string, msg, expected, actual);

/* A sample program with two obligations in its proof:
 *
 *   z != 0   Pending    (open; nothing in scope covers it)
 *   2 != 0   Evaluated  (ground; closed evaluation, channel 2)
 */
let repro = {|let f = fun x -> x + 1 in
theorem t = forall y: Int -> forall z: Int -> f(y) + 4 / 2 + 8 / z > 0 || true
proof rewrite or_true end
in t|};

/* --- status mapping ----------------------------------------------- */

/* The header's status: proven; proven relying on the unfinished theorems
 * a proof cites, which aren't counted as gaps; or incomplete, with a
 * count of its open conditions. Only proven scores. */
let test_status = () => {
  let status = src => {
    let (pm, proof) = Test_ProofMap.Harness.run_named("t", src);
    Status.(label(of_proof(pm, proof)));
  };
  str_check(
    "proven",
    "proven",
    status(
      {|theorem t = 1 == 1 proof from 1 == 1 do rewrite refl end end in t|},
    ),
  );
  str_check(
    "relying",
    "proven*",
    status(
      {|theorem lemma = forall n: Int -> n + 0 == n proof ? in theorem t = 2 + 0 == 2 proof from 2 + 0 do rewrite lemma end end; rewrite refl end in t|},
    ),
  );
  let (pm, proof) =
    Test_ProofMap.Harness.run_named(
      "t",
      {|theorem lemma = forall n: Int -> n + 0 == n proof ? in theorem t = 2 + 0 == 2 proof from 2 + 0 do rewrite lemma end end; rewrite refl end in t|},
    );
  bool_check(
    "relying names what it assumes",
    true,
    Status.of_proof(pm, proof) == Status.Relying(["lemma"]),
  );
  str_check(
    "an open condition",
    "incomplete · 1 open",
    status(
      {|theorem t = forall z: Int -> 8 / z > 0 || true proof rewrite or_true end in t|},
    ),
  );
  str_check(
    "a hole",
    "incomplete",
    status({|theorem t = 1 + 1 == 2 proof ? in t|}),
  );
  str_check(
    "refuted",
    "refuted",
    status(
      {|theorem t = 1 + 1 == 3 proof from 1 + 1 do step end; step in t|},
    ),
  );
  let score = src =>
    Web.Theorems.Model.get_score(
      Test_StepperView.Fixture.mk(src).cell.theorems,
    );
  let score_t = Alcotest.(option(pair(float(0.), float(0.))));
  Alcotest.check(
    score_t,
    "an open condition scores 0",
    Some((0., 1.)),
    score(repro),
  );
  /* Both theorems count; neither is proven outright. */
  Alcotest.check(
    score_t,
    "relying on an unfinished lemma scores 0",
    Some((0., 2.)),
    score(
      {|theorem lemma = forall n: Int -> n + 0 == n proof ? in theorem t = 2 + 0 == 2 proof from 2 + 0 do rewrite lemma end end; rewrite refl end in t|},
    ),
  );
};

/* End to end: the repro theorem's goal reaches `true` but one obligation
 * is pending, so the chip is the ProvenModulo one, while the bool status
 * stays `Some(true)`. */
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
      {|theorem t = 1 == 1 proof from 1 == 1 do rewrite refl end end in t|},
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
  let (pm, proof) =
    run(
      {|theorem t = forall y: Int -> y != 0 ==> 8 / y > 0 || true proof assume y != 0 => rewrite or_true end in t|},
    );
  let obs = ProofMap.obligations_of_proof(pm, proof);
  List.iter(
    ~f=
      (ob: Obligation.t) =>
        switch (ob.discharge, Obligation.remote_fact(ob)) {
        | (Remote(_), Some((name, fact))) =>
          str_check("the hypothesis that covers it", "assume", name);
          check_exp("the fact's statement", "y != 0", fact);
        | (Remote(_), None) =>
          Alcotest.fail("no covering fact recovered for a Remote discharge")
        | (Evaluated | Pending, fact) =>
          bool_check("only a Remote discharge has a fact", true, fact == None)
        },
    obs,
  );
  bool_check(
    "the program has a Remote discharge",
    true,
    List.exists(
      ~f=(ob: Obligation.t) => Obligation.remote_fact(ob) != None,
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
proof have d(y) != 0 proof ? => from 1 == 1 do rewrite refl end end
in t|};
  let (pm, proof) = run(src);
  let obs = ProofMap.obligations_of_proof(pm, proof);
  int_check("one premise obligation", 1, List.length(obs));
  switch (obs) {
  | [ob] => check_exp("the goal is what was written", "d(y) != 0", ob.goal)
  | _ => ()
  };
};

/* --- a step's exits ------------------------------------------------- */

/* Availability is computed from the obligation's own data plus the
 * theorem's syntax, so these tests run the real checker. (The
 * patch/round-trip side is Test_EditorTransform's: ids are per-parse, so
 * patches must be applied to the zipper they were built from.) */

let run_with_ctx = (src: string): (ProofMap.t, Proof.t, Exits.ctx) => {
  let (state, info_map, elab) = src |> parse_exp |> eval_with_proof;
  let proof = proof_of(elab);
  let proof_map = EvaluatorState.get_proof_map(state);
  (
    proof_map,
    proof,
    {
      proof,
      proof_map,
      info_map,
    },
  );
};

let pending_of = (pm, proof): list(Obligation.t) =>
  ProofMap.obligations_of_proof(pm, proof)
  |> List.filter(~f=Obligation.is_pending);

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

/* The obligation arises inside an induction case and mentions that
 * case's own binder. */
let case_scoped_src = {|theorem t = forall n: Int -> 8 / n > 0 || true
proof induction n | 0 => ? | m => rewrite or_true end end
in t|};

let test_exits_at_case_binder = () => {
  let (pm, proof, ctx) = run_with_ctx(case_scoped_src);
  let obs = pending_of(pm, proof);
  let ob =
    switch (
      List.find(
        ~f=(ob: Obligation.t) => Exp.occurs_free_any(["m"], ob.goal),
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
  switch (Exits.for_step(~ctx, ob)) {
  | [prove, split] =>
    bool_check("prove here is available", true, prove.patch != None);
    bool_check("split is available", true, split.patch != None);
  | actions =>
    Alcotest.fail(
      "expected two actions, got " ++ string_of_int(List.length(actions)),
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
    |> List.filter(~f=ob => !Obligation.is_pending(ob));
  bool_check(
    "the repro has discharged obligations to check",
    true,
    List.length(discharged) > 0,
  );
  List.iter(
    ~f=
      ob =>
        int_check(
          "no exits on a discharged obligation",
          0,
          List.length(Exits.for_step(~ctx, ob)),
        ),
    discharged,
  );
};

/* Against a theorem whose proof doesn't hold the incurring step, the
 * exits are disabled, with a reason, rather than misdirected. */
let test_exits_disabled_outside_the_theorem = () => {
  let (pm, proof, _) = run_with_ctx(repro);
  let ob = one_pending("repro", pm, proof);
  let ctx: Exits.ctx = {
    proof: Proof.fresh(EmptyHole),
    proof_map: ProofMap.empty,
    info_map: Statics.Map.empty,
  };
  switch (Exits.for_step(~ctx, ob)) {
  | [prove, split] =>
    bool_check("prove here disabled", true, prove.patch == None);
    bool_check("split disabled", true, split.patch == None);
    bool_check(
      "and says why",
      true,
      contains_substring(prove.title, "Unavailable"),
    );
  | actions =>
    Alcotest.fail(
      "expected two actions, got " ++ string_of_int(List.length(actions)),
    )
  };
};

/* An exit that would break the proof shows disabled. Splitting on a
 * condition the goal mentions rewrites the goal of the steps below it. */
let test_split_disabled_when_it_breaks_a_step = () => {
  let (pm, proof, ctx) =
    run_with_ctx(
      "theorem t = forall x: Int -> x == 1 proof have x == 1 proof ? => from x do rewrite have end end; from 1 == 1 do rewrite refl end end in t",
    );
  let ob = one_pending("split", pm, proof);
  switch (Exits.for_step(~ctx, ob)) {
  | [prove, split] =>
    bool_check("prove here enabled", true, prove.patch != None);
    bool_check("split disabled", true, split.patch == None);
    str_check(
      "and says why",
      "Unavailable: it would break a step of the proof",
      split.title,
    );
  | exits => Alcotest.fail(string_of_int(List.length(exits)) ++ " exits")
  };
};

let tests = (
  "Obligations",
  [
    test_case(
      "an exit that would break a step is disabled, with a reason",
      `Quick,
      test_split_disabled_when_it_breaks_a_step,
    ),
    test_case(
      "the exits at an induction case wrap the case's body",
      `Quick,
      test_exits_at_case_binder,
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
      "the header reads proven, proven* or incomplete with a count",
      `Quick,
      test_status,
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
  ],
);
