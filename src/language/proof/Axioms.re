/* Built-in axioms available to every proof, applied through the ordinary
 * axiom step with no dedicated checker logic.
 *
 * Soundness: every equation must hold under the symmetric (strong) Kleene
 * reading of the connectives over the flat domain {false, err/⊥ (= U),
 * true}. So `(a && false) == false` is valid (U ∧ F = F), but excluded
 * middle `(a || !a) == true` is not (U ∨ U = U) and is absent, as is
 * anything else that needs definedness.
 *
 * Keep the name list in sync with `Statics.initial_theorems`. */

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

/* The innermost binder is last in `xs`. */
let foralls = (xs: list(string), body: Exp.t): Exp.t =>
  List.fold_right(forall, xs, body);

/* Closure-lemma builders. They use the operators user code parses to,
 * so the lemmas match user goals and the conditions DomainConditions
 * builds: `*`/`+`/`**`/`>`/`<`/`>=` parse to the Int class and `!=` to
 * Poly(NotEquals). There are no SInt or Nat versions of the lemmas. */
let mul = (a: Exp.t, b: Exp.t): Exp.t =>
  BinOp(Int(Times), a, b) |> Exp.fresh;
let add = (a: Exp.t, b: Exp.t): Exp.t =>
  BinOp(Int(Plus), a, b) |> Exp.fresh;
let pow = (a: Exp.t, b: Exp.t): Exp.t =>
  BinOp(Int(Power), a, b) |> Exp.fresh;
let neq0 = (a: Exp.t): Exp.t =>
  DomainConditions.neq_zero(DomainConditions.int_zero(), a);
let gt0 = (a: Exp.t): Exp.t =>
  BinOp(Int(GreaterThan), a, DomainConditions.int_zero()) |> Exp.fresh;
let lt0 = (a: Exp.t): Exp.t =>
  BinOp(Int(LessThan), a, DomainConditions.int_zero()) |> Exp.fresh;
let geq0 = DomainConditions.geq_zero_int;

let a = var("a");
let b = var("b");
let c = var("c");

let axioms: list((string, Exp.t)) = [
  /* Reflexivity of equality. */
  ("refl", forall("x", eq(eq(var("x"), var("x")), tt))),
  /* Definition of implication. Kleene: ==> is *defined* as !a || b. */
  ("impl_def", foralls(["a", "b"], eq(a ==>> b, neg(a) ||| b))),
  /* Commutativity / associativity: Kleene ∧/∨ are symmetric. */
  ("and_comm", foralls(["a", "b"], eq(a &&& b, b &&& a))),
  ("or_comm", foralls(["a", "b"], eq(a ||| b, b ||| a))),
  /* The custom infix ops inherit OCaml associativity (`&&&` is
   * right-assoc, `|||` left-assoc), so the associativity laws use the
   * prefix constructors to make the grouping explicit. */
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
  /* --- Closure lemmas ---------------------------------------------------
   *
   * Conditional rewrite rules with boolean conclusions. A cited rule
   * concluding `P` reads as `P == true` (`ProofRule.BoolReading`),
   * so rewriting `P` to `true` incurs the antecedents as obligations,
   * which are discharged like any other (applying `nonzero_mul` incurs
   * `a != 0` and `b != 0`).
   *
   * Soundness: a guard proven true has defined operands, so the
   * conclusion's arithmetic is defined and the (in)equality holds over
   * exact bigint arithmetic (`*`/`+` are total; `**` errors only on a
   * negative exponent, which `b >= 0` excludes; a > 0 and b >= 0 give
   * a ** b >= 1 > 0). With a guard undefined or false the obligation
   * can't be discharged, so the rule can't be applied.
   *
   * `b >= 0 ==> a ** b >= 0` is false for negative bases
   * ((-2) ** 3 == -8), hence the positive-base `pow_pos`. */
  (
    "nonzero_mul",
    foralls(["a", "b"], neq0(a) ==>> (neq0(b) ==>> neq0(mul(a, b)))),
  ),
  ("nonzero_of_pos", forall("a", gt0(a) ==>> neq0(a))),
  ("nonzero_of_neg", forall("a", lt0(a) ==>> neq0(a))),
  (
    "pos_mul",
    foralls(["a", "b"], gt0(a) ==>> (gt0(b) ==>> gt0(mul(a, b)))),
  ),
  (
    "pos_add",
    foralls(["a", "b"], gt0(a) ==>> (gt0(b) ==>> gt0(add(a, b)))),
  ),
  (
    "pow_pos",
    foralls(["a", "b"], gt0(a) ==>> (geq0(b) ==>> gt0(pow(a, b)))),
  ),
];

let v: ProofCtx.t =
  List.fold_left(
    (ctx, (name, exp)) => ProofCtx.add_exp(name, exp, ctx),
    [],
    axioms,
  );
