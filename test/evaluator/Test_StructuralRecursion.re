open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Phase 4a: structural-recursion detection (tier 2 of the two-tier
 * divergence gate, docs/prover-obligations.md §4.1).
 *
 * StructuralRecursion.re recognizes fixpoints whose every recursive
 * call passes, in one fixed argument position, a strict subterm of
 * that position's parameter; Totality.re then lets such fixpoints (and
 * their applications to total arguments) through the instantiation
 * gate. Non-structural recursion must STILL be refused. Reuses the
 * Test_ProofMap harness, like Test_Definedness. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of = Test_ProofMap.proof_of;
let has_mark_kind = Test_ProofMap.has_mark_kind;

let obligations = (src: string): (ProofMap.t, Proof.t, list(Obligation.t)) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  (pm, proof, ProofMap.obligations_of_proof(pm, proof));
};

let is_divergent_instantiation: ProofMark.t => bool =
  fun
  | ProofMark.PossiblyDivergentInstantiation(_) => true
  | _ => false;

/* The instantiation passes the gate silently: no divergence mark, no
 * obligations, fully Proven. */
let expect_pass = (src: string): unit => {
  let (pm, proof, obs) = obligations(src);
  Alcotest.check(
    Alcotest.bool,
    "no PossiblyDivergentInstantiation mark",
    false,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
  Alcotest.check(Alcotest.int, "zero obligations", 0, List.length(obs));
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* The instantiation is refused: divergence mark, no obligations, not
 * proven. */
let expect_refused = (src: string): unit => {
  let (pm, proof, obs) = obligations(src);
  Alcotest.check(
    Alcotest.bool,
    "PossiblyDivergentInstantiation mark is emitted",
    true,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
  Alcotest.check(
    Alcotest.int,
    "a refused step incurs no obligations",
    0,
    List.length(obs),
  );
  Alcotest.check(
    Alcotest.option(bool),
    "the refused proof is not proven",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
};

/* --- The headline unlock: a Term-ADT decision procedure ---------------- */

/* `size` recurses on constructor payloads — the classic metatheory
 * shape (Phase 3a refused this; 4a must pass it). */
let test_term_adt_size_passes = () =>
  expect_pass(
    {|type Term = +Var(Int)+Lam(Term)+Ap(Term, Term) in let size = fun e -> case e | Var(n) => 1 | Lam(b) => 1 + size(b) | Ap(e1, e2) => 1 + size(e1) + size(e2) end in theorem t = forall x -> size(x) == size(x) proof axiom refl_eq at 0 on size(x) == size(x) end in t|},
  );

/* The same procedure written with surface `fix` (recursive `let`s
 * elaborate to the same FixF; both spellings must pass). */
let test_explicit_fix_passes = () =>
  expect_pass(
    {|type Term = +Var(Int)+Lam(Term)+Ap(Term, Term) in let size = fix f -> fun e -> case e | Var(n) => 1 | Lam(b) => 1 + f(b) | Ap(e1, e2) => 1 + f(e1) + f(e2) end in theorem t = forall x -> size(x) == size(x) proof axiom refl_eq at 0 on size(x) == size(x) end in t|},
  );

/* --- List recursion (the Ex_ReverseReverse shapes) ---------------------- */

/* `snoc` recurses on the tail through `h :: t`; `rev` calls `snoc`
 * (a nested, itself-structural fix after substitution) around its own
 * structural call. */
let test_snoc_rev_pass = () =>
  expect_pass(
    {|let snoc = fun xs -> fun y -> case xs | [] => [y] | h :: t => h :: snoc(t)(y) end in let rev = fun l -> case l | [] => [] | h :: t => snoc(rev(t))(h) end in theorem t = forall x -> rev(x) == rev(x) proof axiom refl_eq at 0 on rev(x) == rev(x) end in t|},
  );

/* --- Depth: strict subterms through nested pattern layers -------------- */

/* `Ap(Lam(b), a)` binds `b` two constructor layers down; tracking is
 * transitive, so `h(b)` passes. */
let test_nested_pattern_depth_passes = () =>
  expect_pass(
    {|type Term = +Var(Int)+Lam(Term)+Ap(Term, Term) in let h = fun e -> case e | Ap(Lam(b), a) => h(b) | Ap(e1, e2) => 0 | Lam(b) => 0 | Var(n) => 0 end in theorem t = forall x -> h(x) == h(x) proof axiom refl_eq at 0 on h(x) == h(x) end in t|},
  );

/* Casing on an already-strict variable: bindings stay strict
 * (subterm-of-subterm), one case at a time. */
let test_two_step_case_passes = () =>
  expect_pass(
    {|type Term = +Var(Int)+Lam(Term)+Ap(Term, Term) in let h = fun e -> case e | Ap(l, a) => case l | Lam(b) => h(b) | Var(n) => 0 | Ap(e1, e2) => h(e1) end | Lam(b) => 0 | Var(n) => 0 end in theorem t = forall x -> h(x) == h(x) proof axiom refl_eq at 0 on h(x) == h(x) end in t|},
  );

/* --- Non-structural recursion must STILL be refused --------------------- */

/* Phase 3a regression: `n - 1` is arithmetic, not a subterm. */
let test_arithmetic_recursion_refused = () =>
  expect_refused(
    {|let f = fun n -> if n > 0 then f(n - 1) else 0 in theorem t = f(1) == f(1) proof axiom refl_eq at 0 on f(1) == f(1) end in t|},
  );

/* `f(e)` passes the parameter itself — not strict. */
let test_self_argument_refused = () =>
  expect_refused(
    {|let f = fun e -> f(e) in theorem t = f(1) == f(1) proof axiom refl_eq at 0 on f(1) == f(1) end in t|},
  );

/* A RE-CONSTRUCTED argument (`Ap(e1, e1)` builds a new term of the
 * same size) is not a subterm. */
let test_reconstructed_argument_refused = () =>
  expect_refused(
    {|type Term = +Var(Int)+Lam(Term)+Ap(Term, Term) in let f = fun e -> case e | Var(n) => 0 | Lam(b) => f(b) | Ap(e1, e2) => f(Ap(e1, e1)) end in theorem t = f(Var(1)) == f(Var(1)) proof axiom refl_eq at 0 on f(Var(1)) == f(Var(1)) end in t|},
  );

/* Each call decreases SOMEWHERE, but not in one fixed position. */
let test_inconsistent_positions_refused = () =>
  expect_refused(
    {|let g = fun a -> fun b -> case a | [] => 0 | h :: t => case b | [] => g(t)(b) | j :: u => g(a)(u) end end in theorem t = g([1])([2]) == g([1])([2]) proof axiom refl_eq at 0 on g([1])([2]) == g([1])([2]) end in t|},
  );

/* Shadowing kills the strict-subterm status: `t` is rebound before
 * the call. */
let test_shadowed_subterm_refused = () =>
  expect_refused(
    {|let s = fun l -> case l | [] => 0 | h :: t => let t = [] in s(t) end in theorem t = s([1]) == s([1]) proof axiom refl_eq at 0 on s([1]) == s([1]) end in t|},
  );

let tests = (
  "Evaluator.StructuralRecursion",
  [
    test_case(
      "Term-ADT size passes the instantiation gate",
      `Quick,
      test_term_adt_size_passes,
    ),
    test_case(
      "explicit fix spelling passes",
      `Quick,
      test_explicit_fix_passes,
    ),
    test_case("snoc/rev list recursion passes", `Quick, test_snoc_rev_pass),
    test_case(
      "strict subterm through nested pattern layers passes",
      `Quick,
      test_nested_pattern_depth_passes,
    ),
    test_case(
      "casing on an already-strict variable stays strict",
      `Quick,
      test_two_step_case_passes,
    ),
    test_case(
      "arithmetic recursion is still refused (3a regression)",
      `Quick,
      test_arithmetic_recursion_refused,
    ),
    test_case(
      "passing the parameter itself is refused",
      `Quick,
      test_self_argument_refused,
    ),
    test_case(
      "passing a re-constructed term is refused",
      `Quick,
      test_reconstructed_argument_refused,
    ),
    test_case(
      "inconsistent decreasing positions are refused",
      `Quick,
      test_inconsistent_positions_refused,
    ),
    test_case(
      "rebinding the subterm variable defeats the call",
      `Quick,
      test_shadowed_subterm_refused,
    ),
  ],
);
