open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;

/* Definedness gates.
 *
 * Structural totality (Totality.re) refuses possibly-divergent
 * instantiations/scrutinees with marks; the domain scan
 * (DomainConditions.re) turns partial-primitive applications into
 * ordinary boolean obligations. */

let check_count = (msg, expected, obs) =>
  Alcotest.check(Alcotest.int, msg, expected, List.length(obs));

let is_divergent_scrutinee: ProofMark.t => bool =
  fun
  | ProofMark.PossiblyDivergentScrutinee => true
  | _ => false;

let is_float_algebrite: ProofMark.t => bool =
  fun
  | ProofMark.FloatAlgebrite => true
  | _ => false;

/* --- Guarded division lemma, end to end ------------------------------ */

/* Using `inv : forall x where x != 0 -> x / x == 1` at a closed nonzero
 * value: the instantiated guard `2 != 0` discharges by closed
 * evaluation; the instantiation `x := 2` itself scans to nothing. */
let test_guarded_division_closed = () => {
  let src = {|theorem inv = forall x where x != 0 -> x / x == 1 proof ? in theorem u = 2 / 2 == 1 proof from 2 / 2 do axiom inv end end; from 1 == 1 do axiom refl_eq end end in u|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let use_proof = proof_of_named("u", elab);
  let obs = ProofMap.obligations_of_proof(pm, use_proof);
  check_count("exactly the guard obligation", 1, obs);
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

/* An open instantiation containing division: `x := y / z` scans to the
 * denominator condition `z != 0`, which stays Pending (open goals are
 * never evaluated). */
let test_open_division_instantiation_pending = () => {
  let src = {|theorem u = forall y -> forall z -> y / z == y / z proof from y / z == y / z do axiom refl_eq end end in u|};
  let (pm, proof, obs) = obligations(src);
  check_count("one scanned obligation", 1, obs);
  switch (obs) {
  | [ob] =>
    check_exp("the obligation is the denominator", "z != 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "the obligation is pending",
      true,
      Obligation.is_pending(ob),
    );
  | _ => ()
  };
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProvenModulo([_]) => ()
  | other =>
    Alcotest.fail(
      "expected ProvenModulo with one obligation, got: "
      ++ ProofMap.show_full_status(other),
    )
  };
};

/* A conditional rule instantiated at a term containing division emits
 * both the instantiated guard and the scanned denominator condition. */
let test_conditional_rule_plus_domain_scan = () => {
  let src = {|theorem inv = forall x where x != 0 -> x / x == 1 proof ? in theorem u = forall a -> forall b -> (a / b) / (a / b) == 1 proof from (a / b) / (a / b) do axiom inv end end; from 1 == 1 do axiom refl_eq end end in u|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let use_proof = proof_of_named("u", elab);
  let obs = ProofMap.obligations_of_proof(pm, use_proof);
  check_count("guard + scanned denominator", 2, obs);
  let has_goal = (expected: string) =>
    List.exists(
      (ob: Obligation.t) =>
        Exp.fast_equal(elaborated_exp(expected), ob.goal),
      obs,
    );
  Alcotest.check(
    Alcotest.bool,
    "instantiated guard a / b != 0 is incurred",
    true,
    has_goal("a / b != 0"),
  );
  Alcotest.check(
    Alcotest.bool,
    "scanned denominator b != 0 is incurred",
    true,
    has_goal("b != 0"),
  );
};

/* --- A `where` restriction covers the scanned domain condition -------
 *
 * What the obligations panel's "Add to statement" exit produces, written
 * by hand. `8 / z` scans to `z != 0` (DomainConditions.neq_zero), and
 * the statement's own `where z != 0` is installed as a hypothesis by
 * `ProofCheck.peel_stmt_binders`, so `Discharge.lookup` must discharge it
 * Remote. If this fails, the panel's exit can't work either.
 *
 * Both sides must be elaborated (see `Occurrence.nth_env_target`). */
let test_where_covers_scanned_condition = () => {
  let src = {|theorem t = forall z where z != 0 -> 8 / z == 8 / z proof from 8 / z == 8 / z do axiom refl_eq end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("exactly the scanned denominator condition", 1, obs);
  switch (obs) {
  | [ob] =>
    check_exp("the obligation is z != 0", "z != 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "discharged Remote against the where-restriction — got: "
      ++ Obligation.show_discharge(ob.discharge),
      true,
      switch (ob.discharge) {
      | Obligation.Remote(_) => true
      | _ => false
      },
    );
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

/* The same, with the restriction extended by `&&`, as the panel's exit
 * does when the binder already carries a guard. */
let test_where_conjunct_covers_scanned_condition = () => {
  let src = {|theorem t = forall z where z > 0 && z != 0 -> 8 / z == 8 / z proof from 8 / z == 8 / z do axiom refl_eq end end in t|};
  let (_pm, _proof, obs) = obligations(src);
  check_count("exactly the scanned denominator condition", 1, obs);
  switch (obs) {
  | [ob] =>
    Alcotest.check(
      Alcotest.bool,
      "discharged Remote against the conjoined restriction — got: "
      ++ Obligation.show_discharge(ob.discharge),
      true,
      switch (ob.discharge) {
      | Obligation.Remote(_) => true
      | _ => false
      },
    )
  | _ => ()
  };
};

/* A parenthesized inner binder. `EditorTransform` wraps a spliced
 * sub-term in `Parens` whenever it shares a segment level with
 * siblings, so "Add to statement" on the inner binder of
 * `forall y where ... -> forall z -> ...` emits
 * `forall y where ... -> (forall z where z != 0 -> ...)`. Binder peeling
 * must look through the parens, or the restriction is never installed
 * and the floated condition stays Pending. */
let test_parenthesized_where_binder_still_installs = () => {
  let src = {|theorem t = forall y -> (forall z where z != 0 -> y + 8 / z == y + 8 / z) proof from y + 8 / z == y + 8 / z do axiom refl_eq end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("exactly the scanned denominator condition", 1, obs);
  switch (obs) {
  | [ob] =>
    Alcotest.check(
      Alcotest.bool,
      "a parenthesised binder's restriction still discharges — got: "
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

/* --- Instantiations that need nothing ------------------------------- */

/* The common instantiations, a quantified variable or a sum of them,
 * emit no marks and no obligations. */
let test_instantiation_exemption_var = () => {
  let src = {|theorem t = forall x -> x == x proof from x == x do axiom refl_eq end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("x := x emits zero obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "no divergence mark",
    false,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

let test_instantiation_exemption_total_ops = () => {
  let src = {|theorem t = forall a -> forall b -> a + b == a + b proof from a + b == a + b do axiom refl_eq end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("x := a + b emits zero obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- Totality refusal -------------------------------------------------- */

/* Instantiating at a call of a general-recursive function is refused:
 * divergence is never an obligation. */
let test_divergent_instantiation_refused = () => {
  let src = {|let f = fun n -> if n > 0 then f(n - 1) else 0 in theorem t = f(1) == f(1) proof from f(1) == f(1) do axiom refl_eq end end in t|};
  let (pm, proof, obs) = obligations(src);
  Alcotest.check(
    Alcotest.bool,
    "PossiblyDivergentInstantiation mark is emitted",
    true,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
  check_count("a refused step incurs no obligations", 0, obs);
  Alcotest.check(
    Alcotest.option(bool),
    "the refused proof is not proven",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
};

/* The same recursive call inside an eval step is fine: eval steps
 * preserve denotation and have no gates. */
let test_recursive_call_in_eval_step_ungated = () => {
  let src = {|let f = fun n -> if n > 0 then f(n - 1) else 0 in theorem t = f(1) == 0 proof from f(1) do eval end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("eval step incurs no obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "no divergence mark on an eval step",
    false,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
};

/* --- Split / induction gate -------------------------------------------- */

/* Structural induction on a bare quantified variable emits nothing. The
 * binder is annotated so this tests the divergence gate alone: an
 * unannotated binder is refused separately, for having an unknown
 * scrutinee type (tests below). */
let test_induction_bare_variable_no_gate = () => {
  let src = {|theorem t = forall b: Bool -> b == b proof induction b | true => from true == true do axiom refl_eq end end | false => from false == false do axiom refl_eq end end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("bare-variable induction emits zero obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "no PossiblyDivergentScrutinee mark",
    false,
    has_mark_kind(pm, proof, is_divergent_scrutinee),
  );
};

/* A computed bool scrutinee (split) is domain-scanned: splitting on
 * `x / y == 1` emits `y != 0` on the induction node, path-insensitively,
 * before any branch is entered. */
let test_split_computed_scrutinee_emits_condition = () => {
  let src = {|theorem t = forall x -> forall y -> x / y == x / y proof induction x / y == 1 | true => ? | false => ? end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("split emits the denominator condition", 1, obs);
  switch (obs) {
  | [ob] =>
    check_exp("the condition is y != 0", "y != 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "the condition is pending",
      true,
      Obligation.is_pending(ob),
    );
  | _ => ()
  };
  Alcotest.check(
    Alcotest.bool,
    "a total computed scrutinee is not refused",
    false,
    has_mark_kind(pm, proof, is_divergent_scrutinee),
  );
};

/* A computed scrutinee that calls a recursive function is refused. */
let test_split_divergent_scrutinee_refused = () => {
  let src = {|let f = fun n -> if n > 0 then f(n - 1) else 0 in theorem t = forall x -> x == x proof induction f(1) == 0 | true => ? | false => ? end in t|};
  let (pm, proof, _obs) = obligations(src);
  Alcotest.check(
    Alcotest.bool,
    "PossiblyDivergentScrutinee mark is emitted",
    true,
    has_mark_kind(pm, proof, is_divergent_scrutinee),
  );
};

/* --- Induction exhaustiveness: the scrutinee's type must be known ----- */

/* A case split reduces `forall x -> P(x)` to one obligation per case,
 * which is only valid if the cases exhaust what `x` ranges over, so the
 * check needs to know the scrutinee's type. When it is `Unknown` the
 * induction is refused (`InductionScrutineeUntyped`) rather than counted
 * as vacuously covered, as the edit-time statics do with the same
 * `Coverage.check`. */

let is_induction_untyped: ProofMark.t => bool =
  fun
  | ProofMark.InductionScrutineeUntyped => true
  | _ => false;

let is_induction_not_exhaustive: ProofMark.t => bool =
  fun
  | ProofMark.InductionNotExhaustive => true
  | _ => false;

let status_of = (src: string): ProofMap.full_status => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  ProofMap.full_status_of_proof(pm, proof_of_named("t", elab));
};

let check_not_proven = (msg, src) =>
  Alcotest.check(
    Alcotest.bool,
    msg,
    true,
    switch (status_of(src)) {
    | Incomplete => true
    | Proven
    | Refuted
    | ProvenModulo(_) => false
    },
  );

let check_mark = (msg, src, pred) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  Alcotest.check(
    Alcotest.bool,
    msg,
    true,
    has_mark_kind(pm, proof_of_named("t", elab), pred),
  );
};

/* (1) The scrutinee `z != 0` is a comparison, so it is `Bool`-typed even
 * though `z` is an unannotated binder: an ordinary missing-`false`-case
 * failure. */
let user_repro = {|let f = fun x where x != 0 -> 100 / x in
let g = fun w -> 100 / w in
theorem t = forall y where y != 0 -> forall z -> f(y) + 4 / 2 + 8 / z == f(y) + 4 / 2 + 8 / z
proof induction z != 0 | true => from f(y) + 4 / 2 + 8 / z == f(y) + 4 / 2 + 8 / z do axiom refl_eq end end end
in 0|};

let test_user_repro_not_proven = () => {
  check_not_proven("the reported repro does not certify", user_repro);
  check_mark(
    "and it is flagged inexhaustive (Bool scrutinee, `false` case missing)",
    user_repro,
    is_induction_not_exhaustive,
  );
};

/* An untyped scrutinee, a bare unannotated `forall` binder. Non-strict
 * `Coverage.check` reports a single-case split on an `Unknown` column as
 * exhaustive, which would certify these false theorems; each must be
 * refused with `InductionScrutineeUntyped`. */
let untyped_witnesses = [
  (
    "forall b -> b == true",
    {|theorem t = forall b -> b == true proof induction b | true => from true == true do axiom refl_eq end end end in t|},
  ),
  (
    "forall n -> n == 0",
    {|theorem t = forall n -> n == 0 proof induction n | 0 => from 0 == 0 do axiom refl_eq end end end in t|},
  ),
  (
    "forall xs -> xs == []",
    {|theorem t = forall xs -> xs == [] proof induction xs | [] => from [] == [] do axiom refl_eq end end end in t|},
  ),
];

let test_untyped_scrutinee_witnesses_refused = () =>
  untyped_witnesses
  |> List.iter(((name, src)) => {
       check_not_proven("false theorem does not certify: " ++ name, src);
       check_mark(
         "InductionScrutineeUntyped on: " ++ name,
         src,
         is_induction_untyped,
       );
     });

/* (2) Annotated scrutinee, `true` case only: refused by the ordinary
 * exhaustiveness path (the type is known, a case is missing). The control
 * confirming the bool-split constraints reach Coverage. */
let test_annotated_true_only_not_proven = () => {
  let src = {|theorem t = forall b: Bool -> b == b proof induction b | true => from true == true do axiom refl_eq end end end in t|};
  check_not_proven("annotated Bool, `true` only, does not certify", src);
  check_mark(
    "flagged InductionNotExhaustive, not Untyped",
    src,
    is_induction_not_exhaustive,
  );
};

/* An unannotated `funext` binder is bound at the function equality's
 * domain, so `induction` on it knows its type even though the statics
 * leave the binder `Unknown`. A missing case is then an ordinary
 * inexhaustive split, not an untyped scrutinee. */
let check_funext_binder_typed = (src: string, ()) =>
  check_mark("inexhaustive, not untyped", src, is_induction_not_exhaustive);

let funext_ascribed_definition = {|let sum: [Int] -> Int = fun xs -> case xs
| [] => 0
| x::xs => x + sum(xs)
end
in
theorem t = sum == sum proof funext xs => induction xs | [] => ? end in t|};

let funext_annotated_lambdas = {|theorem t = (fun xs:[Int] -> 0) == (fun xs:[Int] -> 0) proof funext ys => induction ys | [] => ? end in t|};

/* (3) An annotated scrutinee with both cases certifies. */
let test_annotated_both_cases_proven = () => {
  let src = {|theorem t = forall b: Bool -> b == b proof induction b | true => from true == true do axiom refl_eq end end | false => from false == false do axiom refl_eq end end end in t|};
  Alcotest.check(
    Alcotest.bool,
    "annotated Bool with both cases is Proven",
    true,
    status_of(src) == ProofMap.Proven,
  );
};

/* (4) An untyped scrutinee with both bool cases is refused too.
 * `| true | false` only reduces `forall b -> P(b)` to
 * `P(true) && P(false)` if `b` really is a boolean; with the type
 * unknown, `b` may range over `Int`. The checker doesn't infer `Bool`
 * from the case patterns. */
let test_untyped_both_cases_still_refused = () => {
  let src = {|theorem t = forall b -> b == b proof induction b | true => from true == true do axiom refl_eq end end | false => from false == false do axiom refl_eq end end end in t|};
  check_not_proven("untyped scrutinee with both bool cases is refused", src);
  check_mark("with the untyped mark", src, is_induction_untyped);
};

/* Over-refusal guards. Refusing `Unknown` columns must not refuse these. */

/* A single wildcard/variable case is a genuine catch-all: it covers any
 * type, known or not, so it certifies even on an untyped scrutinee. */
let test_untyped_wildcard_case_still_proven = () => {
  let src = {|theorem t = forall b -> b == b proof induction b | x => from x == x do axiom refl_eq end end end in t|};
  Alcotest.check(
    Alcotest.bool,
    "catch-all case on an untyped scrutinee is Proven",
    true,
    status_of(src) == ProofMap.Proven,
  );
};

/* `Unknown` in a position coverage never consults is fine: splitting a
 * `[?]` scrutinizes the list spine, not the element type. */
let test_unknown_element_type_still_proven = () => {
  let src = {|theorem t = forall xs: [?] -> xs == xs proof induction xs | [] => from [] == [] do axiom refl_eq end end | y :: ys => from y :: ys == y :: ys do axiom refl_eq end end end in t|};
  Alcotest.check(
    Alcotest.bool,
    "list-of-unknown-element split is Proven",
    true,
    status_of(src) == ProofMap.Proven,
  );
};

/* --- Algebrite gate ----------------------------------------------------- */

/* A rewrite whose sides contain division emits the denominator
 * obligation (deduplicated across the two sides). */
let test_algebrite_division_obligation = () => {
  let src = {|theorem t = forall a -> forall b -> a / b == a / b proof from a / b do rewrite a / b end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("one deduplicated denominator obligation", 1, obs);
  switch (obs) {
  | [ob] => check_exp("the condition is b != 0", "b != 0", ob.goal)
  | _ => ()
  };
  Alcotest.check(
    Alcotest.bool,
    "integer rewrite is not refused as float",
    false,
    has_mark_kind(pm, proof, is_float_algebrite),
  );
};

/* Float-typed Algebrite rewrites are refused: CAS field laws are false
 * for IEEE floats. */
let test_algebrite_float_refused = () => {
  let src = {|theorem t = forall x -> x +. 1. ==. x +. 1. proof from x +. 1. do rewrite 1. +. x end end in t|};
  let (pm, proof, obs) = obligations(src);
  Alcotest.check(
    Alcotest.bool,
    "FloatAlgebrite mark is emitted",
    true,
    has_mark_kind(pm, proof, is_float_algebrite),
  );
  check_count("a refused rewrite incurs no obligations", 0, obs);
};

/* --- Float division is total ----------------------------------------- */

/* Float division is IEEE-total: `1. /. 0.` is the value infinity.
 * Instantiating at it emits no obligation and no refusal. */
let test_float_division_is_total = () => {
  let src = {|theorem t = 1. /. 0. == 1. /. 0. proof from 1. /. 0. == 1. /. 0. do axiom refl_eq end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("float division emits zero obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "no divergence mark",
    false,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- Eval steps stay free --------------------------------------------- */

let test_eval_step_division_no_gate = () => {
  let src = {|theorem t = 4 / 2 == 2 proof from 4 / 2 do eval end; from 2 == 2 do eval end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("eval steps over division incur nothing", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- Scan coverage: builtins ------------------------------------------- */

/* `int_mod(a, b)` emits `b != 0`. */
let test_mod_builtin_condition = () => {
  let src = {|theorem t = forall a -> forall b -> int_mod(a, b) == int_mod(a, b) proof from int_mod(a, b) == int_mod(a, b) do axiom refl_eq end end in t|};
  let (_pm, _proof, obs) = obligations(src);
  check_count("one mod obligation", 1, obs);
  switch (obs) {
  | [ob] => check_exp("the condition is b != 0", "b != 0", ob.goal)
  | _ => ()
  };
};

/* Int power with a variable exponent emits `y >= 0` (NegativeExponent
 * is the error; Nat power would be total). */
let test_power_exponent_condition = () => {
  let src = {|theorem t = forall x -> forall y -> x ** y == x ** y proof from x ** y == x ** y do axiom refl_eq end end in t|};
  let (_pm, _proof, obs) = obligations(src);
  check_count("one exponent obligation", 1, obs);
  switch (obs) {
  | [ob] => check_exp("the condition is y >= 0", "y >= 0", ob.goal)
  | _ => ()
  };
};

/* `int_of_float` at a closed finite float: the `is_finite` condition is
 * emitted and discharges by closed evaluation. */
let test_of_float_is_finite_condition = () => {
  let src = {|theorem t = int_of_float(2.5) == int_of_float(2.5) proof from int_of_float(2.5) == int_of_float(2.5) do axiom refl_eq end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("one is_finite obligation", 1, obs);
  Alcotest.check(
    Alcotest.bool,
    "discharged by closed evaluation",
    true,
    switch (obs) {
    | [ob] => ob.discharge == Obligation.Evaluated
    | _ => false
    },
  );
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

let tests = (
  "Evaluator.Definedness",
  [
    test_case(
      "guarded division lemma at a closed value (Evaluated)",
      `Quick,
      test_guarded_division_closed,
    ),
    test_case(
      "open instantiation with division stays Pending",
      `Quick,
      test_open_division_instantiation_pending,
    ),
    test_case(
      "conditional rule adds its guard on top of the domain scan",
      `Quick,
      test_conditional_rule_plus_domain_scan,
    ),
    test_case(
      "instantiation at a quantified variable emits nothing",
      `Quick,
      test_instantiation_exemption_var,
    ),
    test_case(
      "instantiation at a total-op composition emits nothing",
      `Quick,
      test_instantiation_exemption_total_ops,
    ),
    test_case(
      "recursive-call instantiation is refused",
      `Quick,
      test_divergent_instantiation_refused,
    ),
    test_case(
      "recursive call inside an eval step is ungated",
      `Quick,
      test_recursive_call_in_eval_step_ungated,
    ),
    test_case(
      "bare-variable induction emits no gate traffic",
      `Quick,
      test_induction_bare_variable_no_gate,
    ),
    test_case(
      "bool split on a computed scrutinee emits its domain condition",
      `Quick,
      test_split_computed_scrutinee_emits_condition,
    ),
    test_case(
      "bool split on a divergent scrutinee is refused",
      `Quick,
      test_split_divergent_scrutinee_refused,
    ),
    test_case(
      "induction on a comparison missing the false case does not certify",
      `Quick,
      test_user_repro_not_proven,
    ),
    test_case(
      "untyped scrutinee cannot certify a false theorem",
      `Quick,
      test_untyped_scrutinee_witnesses_refused,
    ),
    test_case(
      "annotated scrutinee, true case only, is inexhaustive",
      `Quick,
      test_annotated_true_only_not_proven,
    ),
    test_case(
      "an ascribed definition types a funext binder",
      `Quick,
      check_funext_binder_typed(funext_ascribed_definition),
    ),
    test_case(
      "annotated lambdas type a funext binder",
      `Quick,
      check_funext_binder_typed(funext_annotated_lambdas),
    ),
    test_case(
      "annotated scrutinee with both cases is proven",
      `Quick,
      test_annotated_both_cases_proven,
    ),
    test_case(
      "untyped scrutinee with both bool cases is still refused",
      `Quick,
      test_untyped_both_cases_still_refused,
    ),
    test_case(
      "catch-all case on an untyped scrutinee still certifies",
      `Quick,
      test_untyped_wildcard_case_still_proven,
    ),
    test_case(
      "unknown element type does not block a list split",
      `Quick,
      test_unknown_element_type_still_proven,
    ),
    test_case(
      "algebrite rewrite emits denominator obligations",
      `Quick,
      test_algebrite_division_obligation,
    ),
    test_case(
      "float-typed algebrite rewrite is refused",
      `Quick,
      test_algebrite_float_refused,
    ),
    test_case(
      "float division is IEEE-total: no obligation, no refusal",
      `Quick,
      test_float_division_is_total,
    ),
    test_case(
      "eval steps over division stay free",
      `Quick,
      test_eval_step_division_no_gate,
    ),
    test_case("mod builtin emits b != 0", `Quick, test_mod_builtin_condition),
    test_case(
      "int power emits exponent >= 0",
      `Quick,
      test_power_exponent_condition,
    ),
    test_case(
      "int_of_float emits is_finite, closed-evaluated",
      `Quick,
      test_of_float_is_finite_condition,
    ),
    test_case(
      "a where-restriction discharges the scanned domain condition",
      `Quick,
      test_where_covers_scanned_condition,
    ),
    test_case(
      "an AND-extended where-restriction discharges it too",
      `Quick,
      test_where_conjunct_covers_scanned_condition,
    ),
    test_case(
      "a parenthesized where-binder still installs its restriction",
      `Quick,
      test_parenthesized_where_binder_still_installs,
    ),
  ],
);
