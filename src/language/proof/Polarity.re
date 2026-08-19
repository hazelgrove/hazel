open Util;
open OptUtil.Syntax;

/* POLARITY.re — variance of a position inside a goal
 * (docs/prover-obligations.md §5 "directed stepping", Phase 5).
 *
 * WHAT THIS IS FOR
 * ----------------
 * An *equational* rewrite (`a == b`) is sound at ANY position: equality
 * is a congruence, so replacing a subterm by an equal one never changes
 * the goal's value. A *directed* fact is different. Given
 *
 *     A ==> B          (B is "at least as true as" A)
 *     a <= b           (b is "at least as big as" a)
 *
 * replacing an occurrence of `A` by `B` (resp. `a` by `b`) inside a goal
 * `G` is sound only if `G` is MONOTONE at that position: only if the
 * whole goal can only get *truer* when the subterm gets truer/bigger.
 * A proof step then discharges `G[B]` and concludes `G[A]`, because
 * `G[A] <= G[B]` and the goal we must reach is `true` (the top of the
 * truth order). Under an odd number of order-reversing constructors the
 * implication runs the other way, which is exactly what `Contravariant`
 * records: there, the *reverse* rewrite (`B` by `A`, `b` by `a`) is the
 * sound one.
 *
 * This module computes, for a position identified by the `Id.t` of its
 * subterm, the composed variance of the whole root→position path, plus
 * the boolean SIDE CONDITIONS that the variance verdict is conditional
 * on (the sign facts for `*`). It does NOT incur those conditions as
 * obligations and does not rewrite anything — it only COMPUTES. The
 * caller (the future directed-step gate in ProofCheck) decides whether
 * the verdict permits its rewrite and feeds the returned conditions
 * through the existing obligation machinery (§4.2), exactly as
 * DomainConditions.scan's output is consumed today.
 *
 * THE ORDERS
 * ----------
 * Two orders are in play and the comparison operators are the bridge
 * between them:
 *   - booleans, under the Kleene truth order of §1.3: `false < unknown
 *     < true`. All four connectives are the lattice/De Morgan
 *     operations on that chain, hence monotone or antitone in each
 *     argument — this is why the boolean table below is exact rather
 *     than conservative.
 *   - Int / SInt / Nat, under the usual total arithmetic order.
 * A comparison `a <= b` maps the arithmetic order into the truth order
 * (antitone in `a`, monotone in `b`); everything above a comparison is
 * boolean reasoning, everything below it is arithmetic reasoning.
 *
 * WHAT IS DELIBERATELY NOT COVERED IN v1 (each `Unknown`, with reason)
 * -------------------------------------------------------------------
 *   - user-function application, constructors, `case` scrutinees and
 *     branches, holes, quantifier bodies, `let`, closures;
 *   - `/` and `**`;
 *   - all Float positions (IEEE `<=.` is not a total order: NaN is
 *     incomparable, so no monotonicity argument survives without
 *     NaN-freeness side conditions — and §1.5/Phase 3 already refuse
 *     float-typed rewrites for the same family of reasons).
 * `Unknown` is the conservative answer, never a soundness risk: the
 * caller refuses the directed rewrite. Every `Unknown` carries a reason
 * string, both for the user-facing refusal message and so this module
 * doubles as a checklist of what a v2 could tighten.
 *
 * KNOWN SOUNDNESS CAVEAT — SInt OVERFLOW (raise at the design round)
 * -----------------------------------------------------------------
 * `Int` and `Nat` are Bigint: unbounded, so the arithmetic rows below
 * are exact for them. `SInt` is OCaml's native `int` with NO overflow
 * check (Operators.re: `SInt(Plus) => Defined(..., just((+)))`), so
 * `x + c` wraps at 2^62 and is therefore NOT monotone across that
 * boundary. Every SInt row below is consequently sound only modulo
 * no-overflow. Phase 5's scope decision put SInt in this cut, so the
 * rows are implemented as specified; the honest options are (a) return
 * `Unknown` for SInt arithmetic, (b) emit no-overflow side conditions
 * alongside the sign conditions, or (c) accept it as a documented
 * unsoundness on the grounds that SInt is a performance escape hatch.
 * This module does NOT decide that — it is the first open question for
 * the review. Nothing about the Int/Nat rows depends on the answer.
 */

/* Unknown carries its reason as a payload (design choice, recorded
 * here): the caller wants to *explain* a refusal, and threading a
 * parallel `option(string)` alongside the verdict is strictly worse.
 * `Invariant` carries none — its reason is always the same one ("the
 * path crosses an equality") and is recoverable from the goal. */
[@deriving (show({with_path: false}), eq)]
type polarity =
  | Covariant
  | Contravariant
  | Invariant
  | Unknown(string);

let reason: polarity => option(string) =
  fun
  | Unknown(r) => Some(r)
  | Covariant
  | Contravariant
  | Invariant => None;

/* A verdict a directed step can actually use. `Invariant`/`Unknown`
 * both mean "equalities only"; they differ only in what we can tell
 * the user. */
let is_directed: polarity => bool =
  fun
  | Covariant
  | Contravariant => true
  | Invariant
  | Unknown(_) => false;

/* Order-reversal. Antitone ∘ antitone = monotone; `Invariant` and
 * `Unknown` are fixed points (neither is an order-direction claim). */
let flip: polarity => polarity =
  fun
  | Covariant => Contravariant
  | Contravariant => Covariant
  | Invariant => Invariant
  | Unknown(r) => Unknown(r);

/* Composition along the path, applied outer-step-first:
 * `compose(so_far, next_step)`.
 *
 *   - Covariant is the identity: a monotone context preserves whatever
 *     variance holds below it (monotone ∘ monotone = monotone,
 *     monotone ∘ antitone = antitone).
 *   - Contravariant flips: antitone ∘ f = flip(f).
 *   - Invariant and Unknown ABSORB. Once the path has crossed a
 *     position that is not order-related to the goal's value at all,
 *     nothing below it can restore an order relation, whatever the
 *     inner constructors are.
 *
 * The outermost absorber wins (`compose(Invariant, Unknown(_))` is
 * `Invariant`): both verdicts grant the same permission — equational
 * rewriting only — so the choice is purely diagnostic, and the
 * outermost `==` is the more useful thing to report ("this position is
 * under an equality") than some inner `f(...)`. Recorded as a judgment
 * call; see the module's open questions in the Phase 5 review notes. */
let compose = (so_far: polarity, step: polarity): polarity =>
  switch (so_far) {
  | Unknown(_) => so_far
  | Invariant => Invariant
  | Covariant => step
  | Contravariant => flip(step)
  };

/* ===================== side-condition construction ===================== */

/* Class-matched zero literal, same pattern as DomainConditions.re: the
 * condition must be built at the operand's own numeric class or it will
 * neither typecheck nor match a user-written fact under
 * `Exp.fast_equal`. */
let zero_lit = (cls: Atom.cls): option(Exp.t) =>
  switch (cls) {
  | Int => Some(Exp.fresh(Atom(Int(Bigint.zero))))
  | SInt => Some(Exp.fresh(Atom(SInt(0))))
  | Nat => Some(Exp.fresh(Atom(Nat(Bigint.zero))))
  | Float
  | Bool
  | String => None
  };

/* `e >= 0` (nonneg=true) or `e <= 0` (nonneg=false), at `cls`. */
let sign_condition = (~cls: Atom.cls, ~nonneg: bool, e: Exp.t): option(Exp.t) => {
  let* z = zero_lit(cls);
  let+ op =
    Operators.numeric_bin_op(
      cls,
      nonneg ? GreaterThanOrEqual : LessThanOrEqual,
    );
  Exp.fresh(BinOp(op, e, z));
};

/* Peel transparent wrappers before reading an operand's shape (same
 * `unwrap` discipline as DomainConditions.re). */
let rec unwrap = (e: Exp.t): Exp.t =>
  switch (e |> Exp.term_of) {
  | Parens(inner)
  | Projector(_, inner)
  | Asc(inner, _)
  | Closure(_, inner) => unwrap(inner)
  | _ => e
  };

/* A manifestly non-negative / non-positive literal. Used to discharge
 * a `*` sign condition statically, so `2 * x` and `(-3) * x` get a bare
 * Covariant/Contravariant verdict with NO obligation attached. */
let nonneg_literal = (e: Exp.t): bool =>
  switch (unwrap(e) |> Exp.term_of) {
  | Atom(Int(n))
  | Atom(Nat(n)) => Bigint.(>=)(n, Bigint.zero)
  | Atom(SInt(i)) => i >= 0
  | _ => false
  };

let nonpos_literal = (e: Exp.t): bool =>
  switch (unwrap(e) |> Exp.term_of) {
  | Atom(Int(n))
  | Atom(Nat(n)) => Bigint.(<=)(n, Bigint.zero)
  | Atom(SInt(i)) => i <= 0
  /* `-3` parses as a unary negation, not a negative literal. */
  | UnOp(Int(Minus) | SInt(Minus) | Nat(Minus), inner) =>
    nonneg_literal(inner)
  | _ => false
  };

/* Nat is non-negative by typing, so a Nat-classed operand needs no sign
 * obligation at all. Checked both by the operator's own class and, for
 * an operand whose class the operator does not pin down, by its static
 * type in the info map. */
let nonneg_by_type = (~info_map: Statics.Map.t, e: Exp.t): bool =>
  switch (Statics.Map.ty_of(Exp.rep_id(e), info_map)) {
  | Some(ty) =>
    switch (Typ.term_of(ty)) {
    | Atom(Nat) => true
    | _ => false
    }
  | None => false
  };

/* One operand of a `*`, given the other. The ordered-ring law:
 *   other >= 0  ⟹  self ↦ self * other  is monotone
 *   other <= 0  ⟹  self ↦ self * other  is antitone
 * When the sign is statically evident we commit with no obligation;
 * otherwise v1 always takes the COVARIANT branch and hands back
 * `other >= 0` as a side condition. (Choosing the covariant reading is
 * a judgment call: the polarity pass does not know which orientation
 * the caller's directed fact has, and covariant-with-`>= 0` is the case
 * that arises in practice. The contravariant reading with `other <= 0`
 * is equally sound and is an obvious v2 knob — see open questions.) */
let times_step =
    (~info_map: Statics.Map.t, ~cls: Atom.cls, ~self: Exp.t, ~other: Exp.t)
    : (Exp.t, polarity, list(Exp.t)) =>
  /* Nat operands are non-negative by typing — no obligation needed. */
  if (Atom.equal_cls(cls, Atom.Nat)
      || nonneg_literal(other)
      || nonneg_by_type(~info_map, other)) {
    (self, Covariant, []);
  } else if (nonpos_literal(other)) {
    (self, Contravariant, []);
  } else {
    switch (sign_condition(~cls, ~nonneg=true, other)) {
    | Some(c) => (self, Covariant, [c])
    | None => (
        self,
        Unknown("multiplication at a class with no orderable zero"),
        [],
      )
    };
  };

let times_row =
    (~info_map: Statics.Map.t, ~cls: Atom.cls, a: Exp.t, b: Exp.t)
    : option(list((Exp.t, polarity, list(Exp.t)))) =>
  Some([
    times_step(~info_map, ~cls, ~self=a, ~other=b),
    times_step(~info_map, ~cls, ~self=b, ~other=a),
  ]);

/* ============================== the table ============================== */

/* One row of the table: for a node, the immediate children paired with
 * the variance of THAT ONE STEP and the side conditions that step's
 * verdict is conditional on.
 *
 * `None` means "this form is not in the v1 table" — `polarity_at` turns
 * that into `Unknown` with a form-named reason without having to
 * enumerate the form's children (see `polarity_at`).
 *
 * Every row carries its soundness justification. `≤` below is the
 * Kleene truth order `false < unknown < true` for booleans and the
 * arithmetic order for Int/SInt/Nat. */
let children =
    (~info_map: Statics.Map.t, e: Exp.t)
    : option(list((Exp.t, polarity, list(Exp.t)))) => {
  let cov = e1 => (e1, Covariant, []);
  let con = e1 => (e1, Contravariant, []);
  let inv = e1 => (e1, Invariant, []);
  let unk = (r, e1) => (e1, Unknown(r), []);
  switch (e |> Exp.term_of) {
  /* ---------- transparent wrappers ---------- */
  /* `(e)`, `e : t` and a projector decoration all DENOTE `e`; the
   * identity function is monotone. */
  | Parens(e1) => Some([cov(e1)])
  | Asc(e1, _) => Some([cov(e1)])
  | Projector(_, e1) => Some([cov(e1)])

  /* ---------- booleans (Kleene, §1.3) ---------- */
  /* ¬ is the order-reversing involution of the Kleene chain:
   * ¬false = true, ¬unknown = unknown, ¬true = false. */
  | UnOp(Bool(Not), e1) => Some([con(e1)])
  /* ∧ is the meet (min) and ∨ the join (max) of the chain; meet and
   * join are monotone in each argument in every lattice. Kleene's are
   * symmetric, so this holds for BOTH operands — the asymmetry of
   * evaluation's McCarthy short-circuit is idealized away by §1.3. */
  | BinOp(Bool(And | Or), a, b) => Some([cov(a), cov(b)])
  /* `a ==> b` is `!a || b` (the `impl_def` axiom of §2.1): join of an
   * antitone and a monotone argument. */
  | BinOp(Bool(Implies), a, b) => Some([con(a), cov(b)])
  /* Equality is not order-related to either operand: raising `a` in
   * `a == b` can make the result truer OR falser depending on `b`.
   * Only equational rewriting is licensed under it — which is exactly
   * what `Invariant` tells the caller. `!=` is `!(a == b)`, and
   * flip(Invariant) = Invariant. Float `==.`/`!=.` likewise (and are
   * additionally not even reflexive on NaN). */
  | BinOp(Poly(Equals | NotEquals), a, b)
  | BinOp(Float(Equals | NotEquals), a, b) => Some([inv(a), inv(b)])
  /* `if c then a else b` is `(c && a) || (!c && b)`: with `c` FIXED it
   * is a selection, monotone in each branch in any partial order (it
   * returns one of them unchanged). The condition occurs both
   * positively and negatively, so it is Invariant — v1 makes no attempt
   * at the refinement "if the branches are order-comparable, the
   * condition is covariant/contravariant"; see open questions. */
  | If(c, a, b) => Some([inv(c), cov(a), cov(b)])

  /* ---------- comparisons: the bridge into arithmetic ---------- */
  /* `a <= b` and `a < b` are antitone in `a`, monotone in `b`: raising
   * the left side can only make them falser, raising the right side
   * only truer. `>=`/`>` are the mirror image. Restricted to the
   * ordered integral classes; Float falls through to the Float row
   * below. */
  | BinOp(
      Int(LessThan | LessThanOrEqual) | SInt(LessThan | LessThanOrEqual) |
      Nat(LessThan | LessThanOrEqual),
      a,
      b,
    ) =>
    Some([con(a), cov(b)])
  | BinOp(
      Int(GreaterThan | GreaterThanOrEqual) |
      SInt(GreaterThan | GreaterThanOrEqual) |
      Nat(GreaterThan | GreaterThanOrEqual),
      a,
      b,
    ) =>
    Some([cov(a), con(b)])

  /* ---------- ordered arithmetic (Int / SInt / Nat) ---------- */
  /* NOTE: every SInt row in this section is sound only modulo
   * no-overflow — see the SInt caveat in the module header. Int and Nat
   * are Bigint and need no such proviso. */
  /* Translation invariance: `x <= y` implies `x + c <= y + c` and
   * `c + x <= c + y`. Holds in Nat too (Bigint, so no wraparound); for
   * SInt only where the sum does not wrap. */
  | BinOp(Int(Plus) | SInt(Plus) | Nat(Plus), a, b) =>
    Some([cov(a), cov(b)])
  /* Negation reverses the order: `x <= y` implies `-y <= -x`. */
  | UnOp(Int(Minus) | SInt(Minus), e1) => Some([con(e1)])
  /* `a - b` is `a + (-b)`: monotone left, antitone right. */
  | BinOp(Int(Minus) | SInt(Minus), a, b) => Some([cov(a), con(b)])
  /* Nat has no negation and no subtraction: Operators.re maps both
   * `Nat(Minus)` forms to `Undefined` ("Cannot negate a natural
   * number" / no `nat_minus` semantics). Such a node is ill-typed
   * rather than merely unanalyzed, so there is no order fact to state
   * about it. Refuse. */
  | UnOp(Nat(Minus), e1) =>
    Some([
      unk(
        "negation of a natural number is Undefined (Operators.re): no order fact to compose",
        e1,
      ),
    ])
  | BinOp(Nat(Minus), a, b) =>
    let r = "subtraction on Nat is Undefined (Operators.re): no order fact to compose";
    Some([unk(r, a), unk(r, b)]);
  /* Multiplication is the sign-conditional row. `x <= y` implies
   * `c * x <= c * y` when `c >= 0`, and `c * y <= c * x` when `c <= 0`
   * — the ordered-ring axiom. So each operand's variance is a function
   * of the OTHER operand's sign, and where that sign is not statically
   * evident we return the covariant reading together with the sign fact
   * as a side condition for the caller to incur. */
  | BinOp(Int(Times), a, b) => times_row(~info_map, ~cls=Atom.Int, a, b)
  | BinOp(SInt(Times), a, b) => times_row(~info_map, ~cls=Atom.SInt, a, b)
  | BinOp(Nat(Times), a, b) => times_row(~info_map, ~cls=Atom.Nat, a, b)
  /* Integer division: REFUSED in v1, both positions.
   * The denominator position is genuinely non-monotone — `c / x` jumps
   * across the discontinuity at 0 and is antitone only within a fixed
   * sign band, on top of already carrying a `x != 0` domain obligation
   * (DomainConditions §4.1). The numerator position is the interesting
   * case and is deliberately left on the table: truncation toward zero
   * (Operators.int_divide) is in fact monotone, so `x / c` is covariant
   * given `c > 0`. v1 refuses it for uniformity with the denominator
   * and because a strict-monotonicity reading of "directed" would be
   * wrong here (`1/2 == 2/2 == 0`, so the rewrite can be order-flat).
   * Flagged as an open question rather than a settled table row. */
  | BinOp(Int(Divide) | SInt(Divide) | Nat(Divide), a, b) =>
    let r = "integer division: denominator is non-monotone across zero; numerator monotonicity needs a divisor-sign condition (v1 refuses both)";
    Some([unk(r, a), unk(r, b)]);
  /* Power: REFUSED in v1, both positions. The base position `x ** n` is
   * monotone only for odd `n` (or non-negative `x`) — it needs parity
   * case analysis on the exponent; the exponent position `c ** x` is
   * monotone only for `c >= 1`, constant at `c` in {0,1} and
   * sign-alternating for `c < 0`. Two nested case splits, and Int/SInt
   * power additionally errors on a negative exponent. Out of scope for
   * the first cut. */
  | BinOp(Int(Power) | SInt(Power) | Nat(Power), a, b) =>
    let r = "power: base monotonicity needs exponent parity, exponent monotonicity needs base >= 1 (v1 refuses both)";
    Some([unk(r, a), unk(r, b)]);

  /* ---------- floats: refused wholesale (§1.5) ---------- */
  /* IEEE comparison is not a total order: NaN is incomparable with
   * everything (including itself), so `<=.` is not even reflexive and
   * no monotonicity law holds without a NaN-freeness hypothesis. Phase
   * 3 already refuses float-typed rewrites for the neighbouring reason
   * (CAS field laws are false for IEEE); this is the order-theoretic
   * analogue. */
  | BinOp(Float(_), a, b) =>
    let r = "float: IEEE comparison is not a total order (NaN is incomparable), so no monotonicity law holds";
    Some([unk(r, a), unk(r, b)]);
  | UnOp(Float(_), e1) =>
    Some([
      unk(
        "float: IEEE comparison is not a total order (NaN is incomparable), so no monotonicity law holds",
        e1,
      ),
    ])

  /* ---------- strings ---------- */
  /* `^` is monotone for the *prefix* order, which is not the order any
   * directed fact in this language is stated in. No string ordering
   * exists in v1. */
  | BinOp(String(_), a, b) =>
    let r = "string concatenation: the language has no ordering on strings for a directed fact to use";
    Some([unk(r, a), unk(r, b)]);

  /* ---------- everything else ---------- */
  /* Handled uniformly by `polarity_at`'s fallback, which names the form
   * in the reason. See `unhandled_reason`. */
  | _ => None
  };
};

/* The reason attached to a form that is not in the table. Each of these
 * is a CONSERVATIVE refusal, not a claim of invariance. */
let unhandled_reason = (e: Exp.t): string =>
  switch (e |> Exp.term_of) {
  /* The headline case of §5: nothing is known about a user function's
   * variance in its argument. `f` may be `fun b -> !b`. Inferring
   * per-argument variance from a definition (and from a contract's
   * guard) is the natural v2 and is the single most valuable extension
   * of this module. */
  | Ap(_)
  | DeferredAp(_)
  | TypAp(_) => "argument of a function application: user functions have unknown variance in their arguments"
  /* Constructors are the OTHER obvious v2 candidate: a datatype
   * constructor is injective and structurally monotone, so `C(x)` is
   * plausibly covariant in `x` — but only once an order on the datatype
   * is fixed, and v1 fixes none. Refuse. */
  | Constructor(_)
  | ListLit(_)
  | Cons(_)
  | ListConcat(_)
  | Tuple(_)
  | TupLabel(_)
  | TupleExtension(_)
  | Dot(_) => "inside a data constructor / tuple / list: v1 fixes no order on structured data (covariance is a v2 candidate)"
  /* A scrutinee's value selects a branch, so nothing about the match's
   * value is monotone in it; branch bodies are covariant in principle
   * but are stated in the branch pattern's bound variables, which the
   * caller's directed fact cannot mention. */
  | Match(_) => "inside a case expression: the scrutinee selects a branch and branch bodies bind their own variables"
  /* Binders. A `forall` body and a `let` body ARE covariant positions
   * semantically (∀ and substitution are both monotone), but the
   * position lives under a binder whose variables the incoming directed
   * fact — stated in the outer vocabulary — cannot refer to. Untangling
   * that is a scoping feature, not a polarity one. Refuse in v1. */
  | Fun(_)
  | FunWhere(_)
  | TypFun(_)
  | FixF(_)
  | Let(_)
  | Forall(_)
  | ForallWhere(_)
  | TyAlias(_)
  | Module(_)
  | ModuleExp(_) => "under a binder: the position's variables are not in the directed fact's vocabulary (v1 refuses)"
  /* A closure carries an environment; the subterm's denotation depends
   * on it, while the caller's fact is phrased outside. */
  | Closure(_) => "inside a closure: the subterm is read under a captured environment, not the goal's"
  | EmptyHole
  | MultiHole(_)
  | Invalid(_)
  | DynamicErrorHole(_)
  | Undefined
  | Deferral(_) => "inside a hole or invalid term: no meaning to be monotone in"
  | _ =>
    "unanalyzed form: " ++ Exp.show_cls(Exp.cls_of_term(e |> Exp.term_of))
  };

/* ============================== the fold ============================== */

/* The variance of the position identified by `target` inside `root`,
 * together with the side conditions that verdict is conditional on.
 *
 * `target` is an `Id.t`, matched by `Exp.rep_id` — the same way
 * ProofHacks.replace_exp_id and Exp.find_by_id address a position, so a
 * caller that located a rewrite site with `ProofHacks.nth_exp` can hand
 * the matched node's `rep_id` straight to this function.
 *
 * The empty path is `Covariant`: `G` itself is trivially monotone in
 * `G`. Composition then folds the table's one-step verdicts from the
 * root down, and side conditions accumulate in root→position order.
 *
 * Side conditions are returned ONLY for a directed verdict. Once the
 * verdict is `Invariant`/`Unknown` no directed rewrite will happen, so
 * emitting sign obligations for it would make the caller incur
 * obligations for a rewrite it is about to refuse.
 *
 * A `target` that does not occur in `root` yields `Unknown` — the same
 * refusal as an unanalyzable path, so a caller never has to special-case
 * it. */
let polarity_at =
    (~info_map: Statics.Map.t, root: Exp.t, target: Id.t)
    : (polarity, list(Exp.t)) => {
  let rec go =
          (so_far: polarity, conds: list(Exp.t), e: Exp.t)
          : option((polarity, list(Exp.t))) =>
    if (Id.equal(Exp.rep_id(e), target)) {
      Some((so_far, conds));
    } else {
      switch (children(~info_map, e)) {
      | Some(kids) =>
        List.fold_left(
          (acc, (child, step, step_conds)) =>
            switch (acc) {
            | Some(_) => acc
            | None =>
              let so_far' = compose(so_far, step);
              /* Stop accumulating once the verdict has been absorbed:
               * the conditions of a refused rewrite are noise. */
              let conds' = is_directed(so_far') ? conds @ step_conds : conds;
              go(so_far', conds', child);
            },
          None,
          kids,
        )
      | None =>
        /* Form not in the table. If the target is anywhere inside, the
         * answer is `Unknown` regardless of what lies below — `Unknown`
         * absorbs — so we need not enumerate this form's children. */
        switch (Exp.find_by_id(target, e)) {
        | Some(_) =>
          Some((compose(so_far, Unknown(unhandled_reason(e))), conds))
        | None => None
        }
      };
    };
  switch (go(Covariant, [], root)) {
  | Some((pol, conds)) => is_directed(pol) ? (pol, conds) : (pol, [])
  | None => (Unknown("position does not occur in the goal"), [])
  };
};
