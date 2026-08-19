/* Built-in equational axioms available to every proof (docs/
 * prover-obligations.md §2.1). Applied through the ordinary axiom-step
 * machinery — no dedicated checker logic; the rewriting architecture is
 * the proof calculus.
 *
 * Soundness criterion (§1.3): every equation must hold under the
 * symmetric (strong) Kleene reading of the connectives over the flat
 * domain {false, err/⊥ (= U), true}. In particular:
 *   - `(a && false) == false` IS valid: Kleene ∧ is symmetric-strict in
 *     false (U ∧ F = F);
 *   - excluded middle `(a || !a) == true` is NOT valid (U ∨ U = U) and
 *     is deliberately absent, as is anything else requiring definedness.
 *
 * NOTE: keep the name list in sync with `Statics.initial_hypotheses`. */

let var = (x: string): Exp.t => Var(x) |> Exp.fresh;
let tt: Exp.t = Atom(Bool(true)) |> Exp.fresh;
let ff: Exp.t = Atom(Bool(false)) |> Exp.fresh;
let band = (a: Exp.t, b: Exp.t): Exp.t =>
  BinOp(Bool(And), a, b) |> Exp.fresh;
let bor = (a: Exp.t, b: Exp.t): Exp.t => BinOp(Bool(Or), a, b) |> Exp.fresh;
let (&&&) = band;
let (|||) = bor;
let (==>>) = (a: Exp.t, b: Exp.t): Exp.t =>
  BinOp(Bool(Implies), a, b) |> Exp.fresh;
let neg = (a: Exp.t): Exp.t => UnOp(Bool(Not), a) |> Exp.fresh;
let eq = (a: Exp.t, b: Exp.t): Exp.t =>
  BinOp(Poly(Equals), a, b) |> Exp.fresh;
let forall = (x: string, body: Exp.t): Exp.t =>
  Forall(Var(x) |> Pat.fresh, body) |> Exp.fresh;

/* forall-wrap `body` in each of `xs` (innermost binder last in list). */
let foralls = (xs: list(string), body: Exp.t): Exp.t =>
  List.fold_right(forall, xs, body);

let a = var("a");
let b = var("b");
let c = var("c");

let axioms: list((string, Exp.t)) = [
  /* Reflexivity of equality (predates this set). */
  ("refl_eq", forall("x", eq(eq(var("x"), var("x")), tt))),
  /* Definition of implication. Kleene: ==> is *defined* as !a || b. */
  ("impl_def", foralls(["a", "b"], eq(a ==>> b, neg(a) ||| b))),
  /* Commutativity / associativity: Kleene ∧/∨ are symmetric. */
  ("and_comm", foralls(["a", "b"], eq(a &&& b, b &&& a))),
  ("or_comm", foralls(["a", "b"], eq(a ||| b, b ||| a))),
  /* NOTE: the custom infix ops inherit OCaml associativity (`&&&` is
   * right-assoc, `|||` left-assoc), which is a formatter/paren trap —
   * build the associativity laws with the prefix constructors instead. */
  (
    "and_assoc",
    foralls(
      ["a", "b", "c"],
      eq(band(band(a, b), c), band(a, band(b, c))),
    ),
  ),
  (
    "or_assoc",
    foralls(["a", "b", "c"], eq(bor(bor(a, b), c), bor(a, bor(b, c)))),
  ),
  /* De Morgan: valid in strong Kleene (¬U = U on both sides). */
  (
    "demorgan_and",
    foralls(["a", "b"], eq(neg(a &&& b), neg(a) ||| neg(b))),
  ),
  (
    "demorgan_or",
    foralls(["a", "b"], eq(neg(a ||| b), neg(a) &&& neg(b))),
  ),
  /* Units / annihilators. U ∧ T = U, U ∧ F = F, U ∨ T = T, U ∨ F = U. */
  ("and_true", forall("a", eq(a &&& tt, a))),
  ("and_false", forall("a", eq(a &&& ff, ff))),
  ("or_true", forall("a", eq(a ||| tt, tt))),
  ("or_false", forall("a", eq(a ||| ff, a))),
  /* Double negation: ¬¬U = U. */
  ("not_not", forall("a", eq(neg(neg(a)), a))),
  /* Implication units. (U ==> T) = ¬U ∨ T = T; (T ==> a) = F ∨ a = a;
   * (F ==> a) = T ∨ a = T (even for a = U). */
  ("impl_true", forall("a", eq(a ==>> tt, tt))),
  ("true_impl", forall("a", eq(tt ==>> a, a))),
  ("false_impl", forall("a", eq(ff ==>> a, tt))),
];

let v: ProofCtx.t =
  List.fold_left(
    (ctx, (name, exp)) => ProofCtx.add_exp(name, exp, ctx),
    [],
    axioms,
  );
