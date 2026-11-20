open Alcotest;
open Test_ProofMap.Harness;

/* Structural-recursion detection.
 *
 * StructuralRecursion.re recognizes fixpoints whose every recursive
 * call passes, in one fixed argument position, a strict subterm of
 * that position's parameter. The standardness routine judges such a
 * fixpoint applied to a standard argument standard, so a rule cited at
 * the call needs nothing of it. Any other recursion leaves `!!e` to
 * prove. Each test cites `refl_lemma` at the call. */

let expect_standard = (src: string): unit => {
  let (pm, proof, obs) = obligations_named("t", refl_lemma ++ src);
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "no obligations",
    [],
    List.map(~f=show_obligation, obs),
  );
  check_proven("Proven", pm, proof);
};

let expect_unknown = (call: string, src: string): unit => {
  let (_, _, obs) = obligations_named("t", refl_lemma ++ src);
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "the call's standardness is left to prove",
    ["!! " ++ call],
    standardness_goals(obs),
  );
};

/* --- A Term-ADT decision procedure ------------------------------------ */

/* `size` recurses on constructor payloads, the classic metatheory
 * shape. */
let test_term_adt_size_passes = () =>
  expect_standard(
    {|type Term = +Var(Int)+Lam(Term)+Ap(Term, Term) in let size = fun e -> case e | Var(n) => 1 | Lam(b) => 1 + size(b) | Ap(e1, e2) => 1 + size(e1) + size(e2) end in theorem t = forall x: Term -> size(x) == size(x) proof from size(x) == size(x) do rewrite same end end in t|},
  );

/* The same procedure written with surface `fix` (recursive `let`s
 * elaborate to the same FixF; both spellings must be standard). */
let test_explicit_fix_passes = () =>
  expect_standard(
    {|type Term = +Var(Int)+Lam(Term)+Ap(Term, Term) in let size = fix f -> fun e -> case e | Var(n) => 1 | Lam(b) => 1 + f(b) | Ap(e1, e2) => 1 + f(e1) + f(e2) end in theorem t = forall x: Term -> size(x) == size(x) proof from size(x) == size(x) do rewrite same end end in t|},
  );

/* --- List recursion (the Ex_ReverseReverse shapes) ---------------------- */

/* `snoc` recurses on the tail through `h :: t`; `rev` calls `snoc`
 * (a nested, itself-structural fix after substitution) around its own
 * structural call. The routine reads `snoc`'s result type, which `rev`'s
 * needs, off the parameters' annotations. */
let test_snoc_rev_pass = () =>
  expect_standard(
    {|let snoc = fun (xs: [Int]) -> fun (y: Int) -> case xs | [] => [y] | h :: t => h :: snoc(t)(y) end in let rev = fun (l: [Int]) -> case l | [] => [] | h :: t => snoc(rev(t))(h) end in theorem t = forall x: [Int] -> rev(x) == rev(x) proof from rev(x) == rev(x) do rewrite same end end in t|},
  );

/* --- Depth: strict subterms through nested pattern layers -------------- */

/* `Ap(Lam(b), a)` binds `b` two constructor layers down; tracking is
 * transitive, so `h(b)` passes. */
let test_nested_pattern_depth_passes = () =>
  expect_standard(
    {|type Term = +Var(Int)+Lam(Term)+Ap(Term, Term) in let h = fun e -> case e | Ap(Lam(b), a) => h(b) | Ap(e1, e2) => 0 | Lam(b) => 0 | Var(n) => 0 end in theorem t = forall x: Term -> h(x) == h(x) proof from h(x) == h(x) do rewrite same end end in t|},
  );

/* Casing on an already-strict variable: bindings stay strict
 * (subterm-of-subterm), one case at a time. */
let test_two_step_case_passes = () =>
  expect_standard(
    {|type Term = +Var(Int)+Lam(Term)+Ap(Term, Term) in let h = fun e -> case e | Ap(l, a) => case l | Lam(b) => h(b) | Var(n) => 0 | Ap(e1, e2) => h(e1) end | Lam(b) => 0 | Var(n) => 0 end in theorem t = forall x: Term -> h(x) == h(x) proof from h(x) == h(x) do rewrite same end end in t|},
  );

/* --- Non-structural recursion is left to prove ---------------------- */

/* `n - 1` is arithmetic, not a subterm. */
let test_arithmetic_recursion_unknown = () =>
  expect_unknown(
    "f(1)",
    {|let f = fun n -> if n > 0 then f(n - 1) else 0 in theorem t = f(1) == f(1) proof from f(1) == f(1) do rewrite same end end in t|},
  );

/* `f(e)` passes the parameter itself, which is not strict. */
let test_self_argument_unknown = () =>
  expect_unknown(
    "f(1)",
    {|let f = fun e -> f(e) in theorem t = f(1) == f(1) proof from f(1) == f(1) do rewrite same end end in t|},
  );

/* A reconstructed argument (`Ap(e1, e1)` builds a new term of the
 * same size) is not a subterm. */
let test_reconstructed_argument_unknown = () =>
  expect_unknown(
    "f(Var(1))",
    {|type Term = +Var(Int)+Lam(Term)+Ap(Term, Term) in let f = fun e -> case e | Var(n) => 0 | Lam(b) => f(b) | Ap(e1, e2) => f(Ap(e1, e1)) end in theorem t = f(Var(1)) == f(Var(1)) proof from f(Var(1)) == f(Var(1)) do rewrite same end end in t|},
  );

/* Each call decreases somewhere, but not in one fixed position. */
let test_inconsistent_positions_unknown = () =>
  expect_unknown(
    "g([1])([2])",
    {|let g = fun a -> fun b -> case a | [] => 0 | h :: t => case b | [] => g(t)(b) | j :: u => g(a)(u) end end in theorem t = g([1])([2]) == g([1])([2]) proof from g([1])([2]) == g([1])([2]) do rewrite same end end in t|},
  );

/* Shadowing kills the strict-subterm status: `t` is rebound before
 * the call. */
let test_shadowed_subterm_unknown = () =>
  expect_unknown(
    "s([1])",
    {|let s = fun l -> case l | [] => 0 | h :: t => let t = [] in s(t) end in theorem t = s([1]) == s([1]) proof from s([1]) == s([1]) do rewrite same end end in t|},
  );

let tests = (
  "Evaluator.StructuralRecursion",
  [
    test_case("Term-ADT size is standard", `Quick, test_term_adt_size_passes),
    test_case(
      "the explicit fix spelling is standard",
      `Quick,
      test_explicit_fix_passes,
    ),
    test_case(
      "snoc/rev list recursion is standard",
      `Quick,
      test_snoc_rev_pass,
    ),
    test_case(
      "a strict subterm through nested pattern layers is standard",
      `Quick,
      test_nested_pattern_depth_passes,
    ),
    test_case(
      "casing on an already-strict variable stays strict",
      `Quick,
      test_two_step_case_passes,
    ),
    test_case(
      "arithmetic recursion is left to prove",
      `Quick,
      test_arithmetic_recursion_unknown,
    ),
    test_case(
      "passing the parameter itself is left to prove",
      `Quick,
      test_self_argument_unknown,
    ),
    test_case(
      "passing a re-constructed term is left to prove",
      `Quick,
      test_reconstructed_argument_unknown,
    ),
    test_case(
      "inconsistent decreasing positions are left to prove",
      `Quick,
      test_inconsistent_positions_unknown,
    ),
    test_case(
      "rebinding the subterm variable defeats the call",
      `Quick,
      test_shadowed_subterm_unknown,
    ),
  ],
);
