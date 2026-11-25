open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;
open Poly;

/* Definedness conditions.
 *
 * A cited rule's instantiations and a split's computed scrutinee have
 * to be standard. The standardness routine reduces that to the domain
 * conditions of the partial primitives in them, ordinary boolean
 * obligations, or else leaves `!!e` to prove. The built-in `refl` needs
 * only the domain conditions (DomainConditions.re). */

let check_count = (msg, expected, obs) =>
  Alcotest.check(Alcotest.int, msg, expected, List.length(obs));

let is_float_algebrite: ProofMark.t => bool =
  fun
  | ProofMark.FloatAlgebrite => true
  | _ => false;

/* An open instantiation containing division: citing a rule at `y / z`
 * needs the denominator condition `z != 0`, which stays Pending (open
 * goals are never evaluated). The built-in `refl` would need nothing. */
let test_open_division_instantiation_pending = () => {
  let src =
    refl_lemma
    ++ {|theorem u = forall y: Int -> forall z: Int -> y / z == y / z proof from y / z == y / z do rewrite same end end in u|};
  let (pm, proof, obs) = obligations_named("u", src);
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

/* A parenthesized inner binder. `EditorTransform` wraps a spliced
 * sub-term in `Parens` whenever it shares a segment level with
 * siblings. Binder peeling must look through the parens, or the inner
 * binder is never peeled. */
let test_parenthesized_binder_still_peels = () => {
  let src = {|theorem t = forall y -> (forall z -> y + z == y + z) proof from y + z == y + z do rewrite refl end end in t|};
  let (pm, proof, _) = obligations(src);
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
  let src = {|theorem t = forall x -> x == x proof from x == x do rewrite refl end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("x := x emits zero obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

let test_instantiation_exemption_total_ops = () => {
  let src = {|theorem t = forall a -> forall b -> a + b == a + b proof from a + b == a + b do rewrite refl end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("x := a + b emits zero obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- Standardness left to prove ---------------------------------------- */

/* Instantiating at a call of a general-recursive function leaves its
 * standardness to prove. */
let test_divergent_instantiation_left_to_prove = () => {
  let src =
    refl_lemma
    ++ {|let f = fun n -> if n == 0 then 0 else f(n - 1) in theorem t = f(1) == f(1) proof from f(1) == f(1) do rewrite same end end in t|};
  let (pm, proof, obs) = obligations_named("t", src);
  check_mark_free("nothing is refused", pm, proof);
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "f(1)'s standardness is left to prove",
    ["!! f(1)"],
    standardness_goals(obs),
  );
};

/* The same recursive call inside an eval step is fine: eval steps
 * preserve denotation and have no gates. */
let test_recursive_call_in_eval_step_ungated = () => {
  let src = {|let f = fun n -> if n == 0 then 0 else f(n - 1) in theorem t = f(1) == 0 proof from f(1) do step end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("eval step incurs no obligations", 0, obs);
  check_mark_free("no mark on an eval step", pm, proof);
};

/* --- Split / induction gate -------------------------------------------- */

/* Structural induction on a bare quantified variable emits nothing. The
 * binder is annotated: an unannotated binder is refused separately, for
 * having an unknown scrutinee type (tests below). */
let test_induction_bare_variable_no_gate = () => {
  let src = {|theorem t = forall b: Bool -> b == b proof induction b | true => from true == true do rewrite refl end end | false => from false == false do rewrite refl end end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("bare-variable induction emits zero obligations", 0, obs);
  check_mark_free("no marks", pm, proof);
};

/* A computed bool scrutinee has to be standard: splitting on
 * `x / y == 1` emits `y != 0` on the induction node, path-insensitively,
 * before any branch is entered. */
let test_split_computed_scrutinee_emits_condition = () => {
  let src = {|theorem t = forall x: Int -> forall y: Int -> x / y == x / y proof induction x / y == 1 | true => ? | false => ? end in t|};
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
  check_mark_free("nothing is refused", pm, proof);
};

/* A computed scrutinee that calls a non-structural recursive function
 * leaves its standardness to prove. */
let test_split_divergent_scrutinee_left_to_prove = () => {
  let src = {|let f = fun n -> if n == 0 then 0 else f(n - 1) in theorem t = forall x -> x == x proof induction f(1) == 0 | true => ? | false => ? end in t|};
  let (pm, proof, obs) = obligations(src);
  check_mark_free("nothing is refused", pm, proof);
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "the scrutinee's standardness is left to prove",
    ["!! (f(1) == 0)"],
    standardness_goals(obs),
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
let user_repro = {|let f = fun x -> 100 / x in
let g = fun w -> 100 / w in
theorem t = forall y -> forall z -> f(y) + 4 / 2 + 8 / z == f(y) + 4 / 2 + 8 / z
proof induction z != 0 | true => from f(y) + 4 / 2 + 8 / z == f(y) + 4 / 2 + 8 / z do rewrite refl end end end
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
    {|theorem t = forall b -> b == true proof induction b | true => from true == true do rewrite refl end end end in t|},
  ),
  (
    "forall n -> n == 0",
    {|theorem t = forall n -> n == 0 proof induction n | 0 => from 0 == 0 do rewrite refl end end end in t|},
  ),
  (
    "forall xs -> xs == []",
    {|theorem t = forall xs -> xs == [] proof induction xs | [] => from [] == [] do rewrite refl end end end in t|},
  ),
];

let test_untyped_scrutinee_witnesses_refused = () =>
  untyped_witnesses
  |> List.iter(~f=((name, src)) => {
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
  let src = {|theorem t = forall b: Bool -> b == b proof induction b | true => from true == true do rewrite refl end end end in t|};
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
  let src = {|theorem t = forall b: Bool -> b == b proof induction b | true => from true == true do rewrite refl end end | false => from false == false do rewrite refl end end end in t|};
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
  let src = {|theorem t = forall b -> b == b proof induction b | true => from true == true do rewrite refl end end | false => from false == false do rewrite refl end end end in t|};
  check_not_proven("untyped scrutinee with both bool cases is refused", src);
  check_mark("with the untyped mark", src, is_induction_untyped);
};

/* Over-refusal guards. Refusing `Unknown` columns must not refuse these. */

/* A single wildcard/variable case is a genuine catch-all: it covers any
 * type, known or not, so it certifies even on an untyped scrutinee. */
let test_untyped_wildcard_case_still_proven = () => {
  let src = {|theorem t = forall b -> b == b proof induction b | x => from x == x do rewrite refl end end end in t|};
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
  let src = {|theorem t = forall xs: [?] -> xs == xs proof induction xs | [] => from [] == [] do rewrite refl end end | y :: ys => from y :: ys == y :: ys do rewrite refl end end end in t|};
  Alcotest.check(
    Alcotest.bool,
    "list-of-unknown-element split is Proven",
    true,
    status_of(src) == ProofMap.Proven,
  );
};

/* --- Algebrite gate ----------------------------------------------------- */

/* A rewrite whose sides contain division emits the denominator
 * obligation (deduplicated across the two sides). Division is outside
 * what the CAS verifies, so the step's equation is an obligation too. */
let test_algebrite_division_obligation = () => {
  let src = {|theorem t = forall a -> forall b -> a / b == a / b proof from a / b do algebra a / b end end in t|};
  let (pm, proof, obs) = obligations(src);
  let obs =
    List.filter(~f=(ob: Obligation.t) => ob.kind == Definedness, obs);
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
  let src = {|theorem t = forall x -> x +. 1. ==. x +. 1. proof from x +. 1. do algebra 1. +. x end end in t|};
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
  let src = {|theorem t = 1. /. 0. == 1. /. 0. proof from 1. /. 0. == 1. /. 0. do rewrite refl end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("float division emits zero obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- Eval steps stay free --------------------------------------------- */

let test_eval_step_division_no_gate = () => {
  let src = {|theorem t = 4 / 2 == 2 proof from 4 / 2 do step end; from 2 == 2 do step end in t|};
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
  let src =
    refl_lemma
    ++ {|theorem t = forall a: Int -> forall b: Int -> int_mod(a, b) == int_mod(a, b) proof from int_mod(a, b) == int_mod(a, b) do rewrite same end end in t|};
  let (_pm, _proof, obs) = obligations_named("t", src);
  check_count("one mod obligation", 1, obs);
  switch (obs) {
  | [ob] => check_exp("the condition is b != 0", "b != 0", ob.goal)
  | _ => ()
  };
};

/* Int power with a variable exponent emits `y >= 0` (NegativeExponent
 * is the error; Nat power would be total). */
let test_power_exponent_condition = () => {
  let src =
    refl_lemma
    ++ {|theorem t = forall x: Int -> forall y: Int -> x ** y == x ** y proof from x ** y == x ** y do rewrite same end end in t|};
  let (_pm, _proof, obs) = obligations_named("t", src);
  check_count("one exponent obligation", 1, obs);
  switch (obs) {
  | [ob] => check_exp("the condition is y >= 0", "y >= 0", ob.goal)
  | _ => ()
  };
};

/* `int_of_float` at a closed finite float: the `is_finite` condition is
 * emitted and discharges by closed evaluation. */
let test_of_float_is_finite_condition = () => {
  let src =
    refl_lemma
    ++ {|theorem t = int_of_float(2.5) == int_of_float(2.5) proof from int_of_float(2.5) == int_of_float(2.5) do rewrite same end end in t|};
  let (pm, proof, obs) = obligations_named("t", src);
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

/* Closed evaluation of a goal that hits a dynamic error ends at the
 * error, not at `true`; a goal with no error reaches its value. The step
 * count is capped, so that a loop that doesn't stop at the error fails at
 * once instead of running its fuel out. */
let test_closed_evaluation_stops_at_an_error = () => {
  let cap = 20;
  let evaluate = (src: string): (string, int) => {
    let steps = ref(0);
    let step: ProofCheck.step_fn =
      (~env, ~at, e) => {
        incr(steps);
        steps^ > cap ? None : Evaluator.Proofs.step(~env, ~at, e);
      };
    let result =
      ProofCheck.Discharge.evaluate(
        ~step,
        ~env=Builtins.env_init,
        elaborate(parse_exp(src)),
      );
    (print_exp(result), steps^);
  };
  List.iter(
    ~f=
      src => {
        let (result, steps) = evaluate(src);
        Alcotest.check(
          Alcotest.bool,
          src ++ " isn't true",
          false,
          result == "true",
        );
        Alcotest.check(
          Alcotest.bool,
          src ++ " stops at the error",
          true,
          steps <= 5,
        );
      },
    [
      "1 / 0 == 1",
      "(fun (x : Int) -> true)(1 / 0)",
      {|int_of_string("abc") == 1|},
    ],
  );
  List.iter(
    ~f=
      ((src, expected)) =>
        Alcotest.check(Alcotest.string, src, expected, fst(evaluate(src))),
    [
      ("if false then 1 / 0 == 1 else true", "true"),
      ("2 * 3 == 6", "true"),
      ("6 / 2 == 3", "true"),
      ("2 * 3 == 7", "false"),
    ],
  );
};

let tests = (
  "Evaluator.Definedness",
  [
    test_case(
      "closed evaluation stops at a dynamic error",
      `Quick,
      test_closed_evaluation_stops_at_an_error,
    ),
    test_case(
      "open instantiation with division stays Pending",
      `Quick,
      test_open_division_instantiation_pending,
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
      "a recursive-call instantiation is left to prove",
      `Quick,
      test_divergent_instantiation_left_to_prove,
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
      "a bool split on a divergent scrutinee is left to prove",
      `Quick,
      test_split_divergent_scrutinee_left_to_prove,
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
      "a parenthesized binder still peels",
      `Quick,
      test_parenthesized_binder_still_peels,
    ),
  ],
);
