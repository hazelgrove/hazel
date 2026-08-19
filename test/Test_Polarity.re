open Alcotest;
open Language;

/* TEST_POLARITY.re — the Phase 5 variance table (src/language/proof/
 * Polarity.re, docs/prover-obligations.md §5).
 *
 * Table-driven: each case names a goal, a position inside it (located
 * by the first occurrence of a distinguished variable), the expected
 * composed polarity, and the expected side conditions.
 *
 * Positions are given by variable name because that is how a reader
 * checks the case at a glance; the module's actual interface is an
 * `Id.t`, which `position` extracts. Each program uses a distinct name
 * for the position of interest so "first occurrence" is unambiguous. */

let parse_exp = (s: string) =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };

exception Found(Exp.t);

/* The first subterm satisfying `pred`, in traversal order. */
let find_sub = (pred: Exp.t => bool, e: Exp.t): Exp.t =>
  switch (
    Exp.map_term(
      ~f_exp=(cont, x) => pred(x) ? raise(Found(x)) : cont(x),
      e,
    )
  ) {
  | exception (Found(x)) => x
  | _ => Alcotest.fail("subterm not found")
  };

/* The id of the first `Var(name)` occurrence — the position under test. */
let position = (name: string, e: Exp.t): Id.t =>
  Exp.rep_id(
    find_sub(
      x =>
        switch (x |> Exp.term_of) {
        | Var(n) => n == name
        | _ => false
        },
      e,
    ),
  );

/* Polarity is compared by CONSTRUCTOR, not structurally: `Unknown`'s
 * reason string is a user-facing message we deliberately do not pin
 * down in tests (a reworded reason should not fail the suite). That a
 * reason is present at all is checked separately, once. */
let tag: Polarity.polarity => string =
  fun
  | Covariant => "Covariant"
  | Contravariant => "Contravariant"
  | Invariant => "Invariant"
  | Unknown(_) => "Unknown";

let exp_list: testable(list(Exp.t)) =
  testable(
    Fmt.using(
      cs => "[" ++ String.concat("; ", List.map(Exp.show, cs)) ++ "]",
      Fmt.string,
    ),
    List.equal(Exp.fast_equal),
  );

/* One table row. `conds` are given as source strings and parsed with
 * the same parser as the goal, so a row asserts that the module builds
 * a side condition a user could have written by hand (the
 * class-matched-zero-literal discipline of DomainConditions.re). */
let row =
    (
      ~info_map=Statics.Map.empty,
      ~conds: list(string)=[],
      name: string,
      src: string,
      at: string,
      expected: string,
    ) =>
  test_case(
    name,
    `Quick,
    () => {
      let goal = parse_exp(src);
      let (pol, actual_conds) =
        Polarity.polarity_at(~info_map, goal, position(at, goal));
      check(string, src ++ " @ " ++ at, expected, tag(pol));
      check(
        exp_list,
        src ++ " @ " ++ at ++ " side conditions",
        List.map(parse_exp, conds),
        actual_conds,
      );
    },
  );

/* Same, for goals built directly rather than parsed (used where the
 * surface syntax cannot pin the numeric class, e.g. Nat and Float
 * operators). */
let row_exp =
    (
      ~info_map=Statics.Map.empty,
      ~conds: list(Exp.t)=[],
      name: string,
      goal: Exp.t,
      at: string,
      expected: string,
    ) =>
  test_case(
    name,
    `Quick,
    () => {
      let (pol, actual_conds) =
        Polarity.polarity_at(~info_map, goal, position(at, goal));
      check(string, name, expected, tag(pol));
      check(exp_list, name ++ " side conditions", conds, actual_conds);
    },
  );

let var = (x: string): Exp.t => Exp.fresh(Var(x));

/* ==================== directly-built goals ==================== */

/* `a <= c * x` at the Nat class: Nat operands are non-negative by
 * typing, so the sign condition is discharged statically. */
let nat_times_goal =
  Exp.fresh(
    BinOp(
      Nat(LessThanOrEqual),
      var("a"),
      Exp.fresh(BinOp(Nat(Times), var("c"), var("x"))),
    ),
  );

/* `a <= b - x` at the Nat class — `Nat(Minus)` is Undefined in
 * Operators.re, so there is no order fact to compose. */
let nat_minus_goal =
  Exp.fresh(
    BinOp(
      Nat(LessThanOrEqual),
      var("a"),
      Exp.fresh(BinOp(Nat(Minus), var("b"), var("x"))),
    ),
  );

/* `a <=. x` — IEEE comparison, refused wholesale (§1.5). */
let float_leq_goal =
  Exp.fresh(BinOp(Float(LessThanOrEqual), var("a"), var("x")));

/* `a <= 2 * x` at the Int class, built directly so the literal's class
 * is unambiguous. */
let int_times_lit_goal =
  Exp.fresh(
    BinOp(
      Int(LessThanOrEqual),
      var("a"),
      Exp.fresh(
        BinOp(
          Int(Times),
          Exp.fresh(Atom(Int(Bigint.of_int(2)))),
          var("x"),
        ),
      ),
    ),
  );

/* `a <= (-2) * x`: a manifestly non-positive coefficient flips the
 * position and, like the positive case, needs no obligation. */
let int_times_neg_lit_goal =
  Exp.fresh(
    BinOp(
      Int(LessThanOrEqual),
      var("a"),
      Exp.fresh(
        BinOp(
          Int(Times),
          Exp.fresh(
            UnOp(Int(Minus), Exp.fresh(Atom(Int(Bigint.of_int(2))))),
          ),
          var("x"),
        ),
      ),
    ),
  );

/* `a <= c * x` at the Int class with an opaque coefficient: the row
 * that produces an obligation. */
let int_times_var_goal =
  Exp.fresh(
    BinOp(
      Int(LessThanOrEqual),
      var("a"),
      Exp.fresh(BinOp(Int(Times), var("c"), var("x"))),
    ),
  );

let c_geq_zero =
  Exp.fresh(
    BinOp(
      Int(GreaterThanOrEqual),
      var("c"),
      Exp.fresh(Atom(Int(Bigint.zero))),
    ),
  );

/* `(c * x) == y`: the multiplication row fires, but the path crosses an
 * equality, so the verdict absorbs to Invariant and the sign condition
 * must NOT be handed to the caller (it would be an obligation for a
 * rewrite that is about to be refused). */
let times_under_eq_goal =
  Exp.fresh(
    BinOp(
      Poly(Equals),
      Exp.fresh(BinOp(Int(Times), var("c"), var("x"))),
      var("y"),
    ),
  );

/* ==================== property-style sanity ==================== */

let double_negate = (e: Exp.t): Exp.t =>
  Exp.fresh(UnOp(Bool(Not), Exp.fresh(UnOp(Bool(Not), e))));

/* Wrapping any position in two negations must preserve its polarity
 * exactly and add no side conditions: `!` is an order-reversing
 * involution on the Kleene chain, so flip ∘ flip = id, and negation
 * carries no sign hypothesis. This is the composition law stated as a
 * property rather than as another table row. */
let double_negation_preserves = (src: string, at: string) =>
  test_case(
    "double negation preserves " ++ src ++ " @ " ++ at,
    `Quick,
    () => {
      let goal = parse_exp(src);
      let target = position(at, goal);
      let info_map = Statics.Map.empty;
      let (pol, conds) = Polarity.polarity_at(~info_map, goal, target);
      let wrapped = double_negate(goal);
      let (pol', conds') = Polarity.polarity_at(~info_map, wrapped, target);
      check(string, "polarity preserved: " ++ src, tag(pol), tag(pol'));
      check(exp_list, "no conditions added: " ++ src, conds, conds');
    },
  );

let tests = [
  (
    "Polarity",
    [
      /* ---------- identity / the empty path ---------- */
      row("the goal root itself is covariant", "p", "p", "Covariant"),
      row(
        "a transparent wrapper preserves polarity",
        "(p) && q",
        "p",
        "Covariant",
      ),
      /* ---------- negation ---------- */
      row("single negation flips", "!p", "p", "Contravariant"),
      row("double negation restores", "!(!p)", "p", "Covariant"),
      row("triple negation flips", "!(!(!p))", "p", "Contravariant"),
      /* ---------- && / || (Kleene meet and join) ---------- */
      row("&& is covariant on the left", "p && q", "p", "Covariant"),
      row("&& is covariant on the right", "q && p", "p", "Covariant"),
      row("|| is covariant on the left", "p || q", "p", "Covariant"),
      row("|| is covariant on the right", "q || p", "p", "Covariant"),
      row(
        "&& under a negation flips both sides",
        "!(p && q)",
        "q",
        "Contravariant",
      ),
      /* ---------- ==> ---------- */
      row(
        "==> is contravariant on the left",
        "p ==> q",
        "p",
        "Contravariant",
      ),
      row("==> is covariant on the right", "q ==> p", "p", "Covariant"),
      row(
        "nested ==> antecedent-of-antecedent is covariant",
        "(p ==> q) ==> r",
        "p",
        "Covariant",
      ),
      row(
        "nested ==> consequent-of-antecedent is contravariant",
        "(q ==> p) ==> r",
        "p",
        "Contravariant",
      ),
      /* ---------- == / != are invariant ---------- */
      row("== is invariant on the left", "p == q", "p", "Invariant"),
      row("== is invariant on the right", "q == p", "p", "Invariant"),
      row("!= is invariant", "p != q", "p", "Invariant"),
      row(
        "invariance absorbs everything below it",
        "(q && p) == r",
        "p",
        "Invariant",
      ),
      row(
        "negating an equality stays invariant",
        "!(q == p)",
        "p",
        "Invariant",
      ),
      row(
        "an equality above a monotone context still absorbs",
        "(q ==> p) == r",
        "p",
        "Invariant",
      ),
      /* ---------- if ---------- */
      row(
        "if condition is invariant (v1)",
        "if p then q else r",
        "p",
        "Invariant",
      ),
      row(
        "if then-branch is covariant",
        "if c then p else r",
        "p",
        "Covariant",
      ),
      row(
        "if else-branch is covariant",
        "if c then r else p",
        "p",
        "Covariant",
      ),
      row(
        "composition through an if branch: negation flips it",
        "!(if c then p else r)",
        "p",
        "Contravariant",
      ),
      row(
        "composition through an if branch into an implication",
        "!(if c then (q ==> p) else r)",
        "p",
        "Contravariant",
      ),
      /* ---------- comparisons: the arithmetic bridge ---------- */
      row("<= is contravariant on the left", "x <= b", "x", "Contravariant"),
      row("<= is covariant on the right", "a <= x", "x", "Covariant"),
      row("< is contravariant on the left", "x < b", "x", "Contravariant"),
      row("< is covariant on the right", "a < x", "x", "Covariant"),
      row(">= is covariant on the left", "x >= b", "x", "Covariant"),
      row(">= is contravariant on the right", "a >= x", "x", "Contravariant"),
      row("> is covariant on the left", "x > b", "x", "Covariant"),
      row("> is contravariant on the right", "a > x", "x", "Contravariant"),
      /* ---------- + and - ---------- */
      row("+ is covariant (right of <=)", "a <= b + x", "x", "Covariant"),
      row("+ is covariant on either operand", "a <= x + b", "x", "Covariant"),
      row(
        "+ left of <= is contravariant by composition",
        "b + x <= a",
        "x",
        "Contravariant",
      ),
      row(
        "binary - is covariant on the left",
        "a <= x - b",
        "x",
        "Covariant",
      ),
      row(
        "binary - is contravariant on the right",
        "a <= b - x",
        "x",
        "Contravariant",
      ),
      row("unary - flips", "a <= -x", "x", "Contravariant"),
      row(
        "double flip: subtrahend on the left of <=",
        "b - x <= a",
        "x",
        "Covariant",
      ),
      /* ---------- * : the sign-conditional row ---------- */
      row_exp(
        ~conds=[c_geq_zero],
        "* with an opaque coefficient is covariant given `c >= 0`",
        int_times_var_goal,
        "x",
        "Covariant",
      ),
      row_exp(
        "* by a non-negative literal needs no side condition",
        int_times_lit_goal,
        "x",
        "Covariant",
      ),
      row_exp(
        "* by a non-positive literal is contravariant, no side condition",
        int_times_neg_lit_goal,
        "x",
        "Contravariant",
      ),
      row_exp(
        "* at the Nat class needs no side condition",
        nat_times_goal,
        "x",
        "Covariant",
      ),
      row_exp(
        "side conditions are dropped when the verdict absorbs",
        times_under_eq_goal,
        "x",
        "Invariant",
      ),
      /* ---------- refused in v1 ---------- */
      row(
        "division is unknown in the numerator",
        "a <= x / c",
        "x",
        "Unknown",
      ),
      row(
        "division is unknown in the denominator",
        "a <= c / x",
        "x",
        "Unknown",
      ),
      row("power is unknown in the base", "a <= x ** c", "x", "Unknown"),
      row("power is unknown in the exponent", "a <= c ** x", "x", "Unknown"),
      row_exp(
        "Nat subtraction is unknown (Undefined in Operators.re)",
        nat_minus_goal,
        "x",
        "Unknown",
      ),
      row_exp(
        "float comparison is unknown (IEEE is not a total order)",
        float_leq_goal,
        "x",
        "Unknown",
      ),
      row(
        "an argument of a user function is unknown",
        "p ==> f(x)",
        "x",
        "Unknown",
      ),
      row("unknown absorbs everything below it", "f(a <= x)", "x", "Unknown"),
      row(
        "structured data is unknown in v1 (covariance is a v2 candidate)",
        "a <= [x]",
        "x",
        "Unknown",
      ),
      row(
        "a case scrutinee is unknown",
        "case x\n | _ => p\nend",
        "x",
        "Unknown",
      ),
      row("a quantifier body is unknown", "forall q => x", "x", "Unknown"),
      row("a let body is unknown", "let q = 1 in x", "x", "Unknown"),
      /* ---------- refusals carry a reason ---------- */
      test_case(
        "every Unknown carries a reason",
        `Quick,
        () => {
          let goal = parse_exp("p ==> f(x)");
          let (pol, _) =
            Polarity.polarity_at(
              ~info_map=Statics.Map.empty,
              goal,
              position("x", goal),
            );
          check(bool, "reason present", true, Polarity.reason(pol) != None);
        },
      ),
      test_case(
        "a position that does not occur in the goal is Unknown",
        `Quick,
        () => {
          let goal = parse_exp("p && q");
          let (pol, conds) =
            Polarity.polarity_at(~info_map=Statics.Map.empty, goal, Id.mk());
          check(string, "absent position", "Unknown", tag(pol));
          check(exp_list, "no conditions", [], conds);
        },
      ),
      /* ---------- deep nesting, mixed flips ---------- */
      row(
        "deep mix: contra ∘ contra ∘ contra ∘ co",
        "(!(a <= x) ==> q) ==> r",
        "x",
        "Contravariant",
      ),
      /* contra(==>L) ∘ contra(==>L) ∘ contra(==>L) ∘ contra(!) — four
       * flips, back to covariant. */
      row(
        "deep mix: two implication antecedents and a negation",
        "((!p ==> q) ==> r) ==> s",
        "p",
        "Covariant",
      ),
      /* contra(==>L) ∘ contra(!) ∘ contra(<=L) ∘ contra(-R). */
      row(
        "deep mix: arithmetic under two boolean flips",
        "!(b - x <= a) ==> q",
        "x",
        "Covariant",
      ),
      /* ---------- property-style sanity ---------- */
      double_negation_preserves("p && q", "p"),
      double_negation_preserves("!p", "p"),
      double_negation_preserves("p ==> q", "p"),
      double_negation_preserves("q ==> p", "p"),
      double_negation_preserves("p == q", "p"),
      double_negation_preserves("if c then p else r", "p"),
      double_negation_preserves("a <= b + x", "x"),
      double_negation_preserves("a <= b - x", "x"),
      double_negation_preserves("a <= x / c", "x"),
      double_negation_preserves("p ==> f(x)", "x"),
    ],
  ),
];
