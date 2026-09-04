/* Tests for derivation mode logic: ALFA / propositional logic surface syntax,
   abbreviation parsing, and rule verification against the PropositionalLogic
   rule_set. */

open Alcotest;
open Haz3lcore;
open Language;
open Util_web;

/* ----------------------- parsing helpers ----------------------- */

/* Parse a string in the `Drv(Exp)` sort and extract the underlying
   `Drv.Exp.t`. The parser only wraps content as `DrvQuote(Exp(_), _)` when
   the Exp-level parser sees drv-specific tokens that don't fit in Exp; for
   bare identifiers or uppercase tokens the Exp parser succeeds on its own.
   So we only use this helper for strings containing drv-specific syntax
   (e.g. `|-`, `/\`, `\/`, `==>`, `[ ]`). Fails the test otherwise. */
let parse_drv_exp = (code: string): Drv.Exp.t => {
  switch (Parser.to_zipper(~root=Drv(Exp), code)) {
  | None => Alcotest.failf("Parser.to_zipper failed for %S", code)
  | Some(z) =>
    let term = MakeTerm.from_zip_for_sem(z, ~root=Drv(Exp)).term;
    switch (IdTagged.term_of(term)) {
    | DrvQuote(Exp(d), _) => d
    | _ =>
      Alcotest.failf(
        "expected DrvQuote(Exp(_), _) for %S, got %s",
        code,
        Exp.show(term),
      )
    };
  };
};

/* Parse a propositional logic proposition by embedding it on the right-hand
   side of a drv entailment judgement (`[] |- <code>`) so that the drv parser
   is forced to engage. Returns just the proposition. */
let parse_prop = (code: string): Drv.Exp.t => {
  let entail = parse_drv_exp("[] |- " ++ code);
  switch (Drv.Exp.term_of(entail)) {
  | Entail(_, p) => p
  | other =>
    Alcotest.failf(
      "expected Entail, got %s",
      other |> DrvTerm.Exp.cls_of_term |> DrvTerm.Exp.show_cls,
    )
  };
};

/* ----------------------- Drv.Exp smart constructors ----------------------- */
/* Build derivation expressions directly, bypassing parsing. This keeps the
   rule-verification tests focused on the checking logic rather than tokenization
   details. */

let d_var = (x: string): Drv.Exp.t => Drv.Exp.fresh(Var(x));
let d_ctx = (es: list(Drv.Exp.t)): Drv.Exp.t => Drv.Exp.fresh(Ctx(es));
let d_entail = (g: Drv.Exp.t, p: Drv.Exp.t): Drv.Exp.t =>
  Drv.Exp.fresh(Entail(g, p));
let d_and = (a, b): Drv.Exp.t => Drv.Exp.fresh(And(a, b));
let d_or = (a, b): Drv.Exp.t => Drv.Exp.fresh(Or(a, b));
let d_impl = (a, b): Drv.Exp.t => Drv.Exp.fresh(Impl(a, b));
let d_truth: Drv.Exp.t = Drv.Exp.fresh(Truth);
let d_falsity: Drv.Exp.t = Drv.Exp.fresh(Falsity);

/* ----------------------- syntax tests ----------------------- */

let check_term_cls =
    (~msg, ~expected: DrvTerm.Exp.cls, actual: DrvTerm.Exp.term) => {
  let actual_cls = DrvTerm.Exp.cls_of_term(actual);
  check(
    string,
    msg,
    DrvTerm.Exp.show_cls(expected),
    DrvTerm.Exp.show_cls(actual_cls),
  );
};

let test_parse_truth = () => {
  let e = parse_prop("Truth");
  check_term_cls(~msg="Truth parses", ~expected=Truth, Drv.Exp.term_of(e));
};

let test_parse_falsity = () => {
  let e = parse_prop("Falsity");
  check_term_cls(
    ~msg="Falsity parses",
    ~expected=Falsity,
    Drv.Exp.term_of(e),
  );
};

/* Capitalized bare names in Drv(Exp) are propositional variables. */
let test_parse_var = () => {
  let e = parse_prop("A");
  switch (Drv.Exp.term_of(e)) {
  | Var("A") => ()
  | other =>
    Alcotest.failf(
      "expected Var(\"A\"), got %s",
      other |> DrvTerm.Exp.cls_of_term |> DrvTerm.Exp.show_cls,
    )
  };
};

let test_parse_and = () => {
  let e = parse_prop("A /\\ B");
  switch (Drv.Exp.term_of(e)) {
  | And(l, r) =>
    switch (Drv.Exp.term_of(l), Drv.Exp.term_of(r)) {
    | (Var("A"), Var("B")) => ()
    | _ => Alcotest.fail("unexpected children for A /\\ B")
    }
  | _ => Alcotest.fail("A /\\ B did not parse as And")
  };
};

let test_parse_or = () => {
  let e = parse_prop("A \\/ B");
  switch (Drv.Exp.term_of(e)) {
  | Or(_, _) => ()
  | _ => Alcotest.fail("A \\/ B did not parse as Or")
  };
};

let test_parse_impl = () => {
  let e = parse_prop("A ==> B");
  switch (Drv.Exp.term_of(e)) {
  | Impl(_, _) => ()
  | _ => Alcotest.fail("A ==> B did not parse as Impl")
  };
};

/* /\ binds tighter than \/, so `A /\ B \/ C` is `(A /\ B) \/ C`. */
let test_parse_and_or_precedence = () => {
  let e = parse_prop("A /\\ B \\/ C");
  switch (Drv.Exp.term_of(e)) {
  | Or(l, _) =>
    switch (Drv.Exp.term_of(l)) {
    | And(_, _) => ()
    | _ => Alcotest.fail("expected /\\ inside \\/, got flat structure")
    }
  | _ => Alcotest.fail("A /\\ B \\/ C did not parse with Or at the top")
  };
};

/* \/ binds tighter than ==>, so `A \/ B ==> C` is `(A \/ B) ==> C`. */
let test_parse_or_impl_precedence = () => {
  let e = parse_prop("A \\/ B ==> C");
  switch (Drv.Exp.term_of(e)) {
  | Impl(l, _) =>
    switch (Drv.Exp.term_of(l)) {
    | Or(_, _) => ()
    | _ => Alcotest.fail("expected \\/ inside ==>")
    }
  | _ => Alcotest.fail("A \\/ B ==> C did not parse with Impl at the top")
  };
};

/* `!A` in propositional logic is sugar for `A ==> Falsity`. */
let test_parse_not_sugar = () => {
  let e = parse_prop("!A");
  switch (Drv.Exp.term_of(e)) {
  | Impl(l, r) =>
    switch (Drv.Exp.term_of(l), Drv.Exp.term_of(r)) {
    | (Var("A"), Falsity) => ()
    | _ => Alcotest.fail("!A did not desugar to A ==> Falsity")
    }
  | _ => Alcotest.fail("!A did not parse as an Impl")
  };
};

/* `|-` is the lowest precedence, so `A /\ B |- C` parses with Entail at the
   top. */
let test_parse_entail = () => {
  let e = parse_drv_exp("A /\\ B |- C");
  switch (Drv.Exp.term_of(e)) {
  | Entail(l, r) =>
    switch (Drv.Exp.term_of(l), Drv.Exp.term_of(r)) {
    | (And(_, _), Var("C")) => ()
    | _ => Alcotest.fail("unexpected children for A /\\ B |- C")
    }
  | _ => Alcotest.fail("A /\\ B |- C did not parse as Entail")
  };
};

/* [A, B] is a Ctx literal with two elements. */
let test_parse_ctx_literal = () => {
  let e = parse_drv_exp("[A, B] |- A");
  switch (Drv.Exp.term_of(e)) {
  | Entail(l, _) =>
    switch (Drv.Exp.term_of(l)) {
    | Ctx([a, b]) =>
      switch (Drv.Exp.term_of(a), Drv.Exp.term_of(b)) {
      | (Var("A"), Var("B")) => ()
      | _ => Alcotest.fail("ctx elements wrong")
      }
    | _ => Alcotest.fail("left of |- was not a 2-element Ctx")
    }
  | _ => Alcotest.fail("did not parse as Entail")
  };
};

/* Empty context literal [] on the left of |-. */
let test_parse_empty_ctx = () => {
  let e = parse_drv_exp("[] |- Truth");
  switch (Drv.Exp.term_of(e)) {
  | Entail(l, r) =>
    switch (Drv.Exp.term_of(l), Drv.Exp.term_of(r)) {
    | (Ctx([]), Truth) => ()
    | _ => Alcotest.fail("unexpected children for [] |- Truth")
    }
  | _ => Alcotest.fail("did not parse as Entail")
  };
};

/* ----------------------- abbreviation parsing tests ----------------------- */

/* `$x` is a quoted variable (abbreviation reference), not a regular variable.
   This is the simplification that replaced the old `$` prefix operator. */
let test_parse_dollar_var_is_quote = () => {
  let e = parse_prop("$a");
  switch (Drv.Exp.term_of(e)) {
  | Quote("$a") => ()
  | other =>
    Alcotest.failf(
      "expected Quote(\"$a\"), got %s",
      other |> DrvTerm.Exp.cls_of_term |> DrvTerm.Exp.show_cls,
    )
  };
};

let test_parse_dollar_longer_name = () => {
  let e = parse_prop("$foo");
  switch (Drv.Exp.term_of(e)) {
  | Quote("$foo") => ()
  | _ => Alcotest.fail("expected Quote(\"$foo\")")
  };
};

/* Without the `$` prefix, a lowercase identifier parses as a regular
   variable (not a Quote). */
let test_parse_plain_var_is_not_quote = () => {
  let e = parse_prop("a");
  switch (Drv.Exp.term_of(e)) {
  | Var("a") => ()
  | Quote(_) => Alcotest.fail("plain identifier parsed as Quote")
  | _ => Alcotest.fail("plain identifier did not parse as Var")
  };
};

/* Abbreviations compose with infix operators without requiring a space
   (this was previously broken by the `$` prefix operator). */
let test_parse_dollar_in_conjunction = () => {
  let e = parse_prop("$a /\\ $b");
  switch (Drv.Exp.term_of(e)) {
  | And(l, r) =>
    switch (Drv.Exp.term_of(l), Drv.Exp.term_of(r)) {
    | (Quote("$a"), Quote("$b")) => ()
    | _ => Alcotest.fail("expected two Quote children under And")
    }
  | _ => Alcotest.fail("did not parse as And")
  };
};

/* ----------------------- rule verification helpers ----------------------- */

let verify_rule =
    (rule: Rule.t, concl: Drv.Exp.t, prems: list(Drv.Exp.t)): RuleVerify.res => {
  let spec = RuleSpec.of_spec(rule);
  RuleVerify.verify(spec, (concl, prems));
};

let is_correct = (res: RuleVerify.res) => List.length(res) == 0;

let check_correct = (~msg, rule, concl, prems) =>
  check(bool, msg, true, is_correct(verify_rule(rule, concl, prems)));

let check_incorrect = (~msg, rule, concl, prems) =>
  check(bool, msg, false, is_correct(verify_rule(rule, concl, prems)));

/* ----------------------- Truth_I ----------------------- */

let test_truth_i_correct = () =>
  check_correct(
    ~msg="[] |- Truth via Truth_I",
    Truth_I,
    d_entail(d_ctx([]), d_truth),
    [],
  );

let test_truth_i_wrong_concl = () =>
  check_incorrect(
    ~msg="[] |- A cannot be derived by Truth_I",
    Truth_I,
    d_entail(d_ctx([]), d_var("A")),
    [],
  );

let test_truth_i_extra_premise = () =>
  check_incorrect(
    ~msg="Truth_I with an extra premise is a mismatch",
    Truth_I,
    d_entail(d_ctx([]), d_truth),
    [d_entail(d_ctx([]), d_truth)],
  );

/* ----------------------- Assumption ----------------------- */

let test_assumption_single = () =>
  check_correct(
    ~msg="[A] |- A via Assumption",
    Assumption,
    d_entail(d_ctx([d_var("A")]), d_var("A")),
    [],
  );

let test_assumption_picks_member = () =>
  check_correct(
    ~msg="[A, B] |- B via Assumption",
    Assumption,
    d_entail(d_ctx([d_var("A"), d_var("B")]), d_var("B")),
    [],
  );

let test_assumption_empty_ctx = () =>
  check_incorrect(
    ~msg="[] |- A not derivable by Assumption",
    Assumption,
    d_entail(d_ctx([]), d_var("A")),
    [],
  );

let test_assumption_not_in_ctx = () =>
  check_incorrect(
    ~msg="[B] |- A not derivable by Assumption",
    Assumption,
    d_entail(d_ctx([d_var("B")]), d_var("A")),
    [],
  );

/* ----------------------- And_I / And_E_L / And_E_R ----------------------- */

let test_and_i_correct = () =>
  check_correct(
    ~msg="[] |- A /\\ B from [|- A, |- B]",
    And_I,
    d_entail(d_ctx([]), d_and(d_var("A"), d_var("B"))),
    [d_entail(d_ctx([]), d_var("A")), d_entail(d_ctx([]), d_var("B"))],
  );

let test_and_i_missing_premise = () =>
  check_incorrect(
    ~msg="And_I with one premise is a mismatch",
    And_I,
    d_entail(d_ctx([]), d_and(d_var("A"), d_var("B"))),
    [d_entail(d_ctx([]), d_var("A"))],
  );

let test_and_i_wrong_connective = () =>
  check_incorrect(
    ~msg="And_I cannot derive an Or conclusion",
    And_I,
    d_entail(d_ctx([]), d_or(d_var("A"), d_var("B"))),
    [d_entail(d_ctx([]), d_var("A")), d_entail(d_ctx([]), d_var("B"))],
  );

/* Premises and conclusion must share a single context variable. */
let test_and_i_mismatched_ctx = () =>
  check_incorrect(
    ~msg="And_I with mismatched contexts between conclusion and premises",
    And_I,
    d_entail(d_ctx([d_var("G")]), d_and(d_var("A"), d_var("B"))),
    [
      d_entail(d_ctx([d_var("G")]), d_var("A")),
      d_entail(d_ctx([d_var("H")]), d_var("B")),
    ],
  );

let test_and_e_l_correct = () =>
  check_correct(
    ~msg="[] |- A from [|- A /\\ B] via And_E_L",
    And_E_L,
    d_entail(d_ctx([]), d_var("A")),
    [d_entail(d_ctx([]), d_and(d_var("A"), d_var("B")))],
  );

let test_and_e_l_takes_right = () =>
  check_incorrect(
    ~msg="And_E_L cannot conclude the right conjunct",
    And_E_L,
    d_entail(d_ctx([]), d_var("B")),
    [d_entail(d_ctx([]), d_and(d_var("A"), d_var("B")))],
  );

let test_and_e_r_correct = () =>
  check_correct(
    ~msg="[] |- B from [|- A /\\ B] via And_E_R",
    And_E_R,
    d_entail(d_ctx([]), d_var("B")),
    [d_entail(d_ctx([]), d_and(d_var("A"), d_var("B")))],
  );

/* ----------------------- Or_I_L / Or_I_R ----------------------- */

let test_or_i_l_correct = () =>
  check_correct(
    ~msg="[] |- A \\/ B from [|- A] via Or_I_L",
    Or_I_L,
    d_entail(d_ctx([]), d_or(d_var("A"), d_var("B"))),
    [d_entail(d_ctx([]), d_var("A"))],
  );

let test_or_i_l_wrong_side = () =>
  check_incorrect(
    ~msg="Or_I_L cannot be used when the premise is the right disjunct",
    Or_I_L,
    d_entail(d_ctx([]), d_or(d_var("A"), d_var("B"))),
    [d_entail(d_ctx([]), d_var("B"))],
  );

let test_or_i_r_correct = () =>
  check_correct(
    ~msg="[] |- A \\/ B from [|- B] via Or_I_R",
    Or_I_R,
    d_entail(d_ctx([]), d_or(d_var("A"), d_var("B"))),
    [d_entail(d_ctx([]), d_var("B"))],
  );

/* ----------------------- Implies_I / Implies_E ----------------------- */

/* Implies_I discharges the hypothesis by extending the context by `a`. */
let test_implies_i_correct = () =>
  check_correct(
    ~msg="[] |- A ==> B from [[A] |- B] via Implies_I",
    Implies_I,
    d_entail(d_ctx([]), d_impl(d_var("A"), d_var("B"))),
    [d_entail(d_ctx([d_var("A")]), d_var("B"))],
  );

let test_implies_i_context_not_extended = () =>
  check_incorrect(
    ~msg="Implies_I fails when hypothesis is not added to the premise ctx",
    Implies_I,
    d_entail(d_ctx([]), d_impl(d_var("A"), d_var("B"))),
    [d_entail(d_ctx([]), d_var("B"))],
  );

let test_implies_e_correct = () =>
  check_correct(
    ~msg="Modus ponens: [] |- B from [|- A ==> B, |- A]",
    Implies_E,
    d_entail(d_ctx([]), d_var("B")),
    [
      d_entail(d_ctx([]), d_impl(d_var("A"), d_var("B"))),
      d_entail(d_ctx([]), d_var("A")),
    ],
  );

/* ----------------------- Falsity_E ----------------------- */

/* Ex falso: from a proof of False, anything follows. */
let test_falsity_e_correct = () =>
  check_correct(
    ~msg="[] |- A from [|- Falsity] via Falsity_E",
    Falsity_E,
    d_entail(d_ctx([]), d_var("A")),
    [d_entail(d_ctx([]), d_falsity)],
  );

/* ----------------------- rule_set-level tests ----------------------- */

/* Rules from other rule sets (e.g. ALFA typing) should not be dispatchable
   against the PropositionalLogic rule_set. */
let test_propositional_rule_set_only_includes_prop_logic = () => {
  check(
    bool,
    "Assumption is in PropositionalLogic",
    true,
    Option.is_some(RuleImage.to_rule(PropositionalLogic, Assumption)),
  );
  check(
    bool,
    "T_True is NOT in PropositionalLogic",
    true,
    Option.is_none(RuleImage.to_rule(PropositionalLogic, T_True)),
  );
  check(
    bool,
    "And_I is in PropositionalLogic",
    true,
    Option.is_some(RuleImage.to_rule(PropositionalLogic, And_I)),
  );
};

/* ----------------------- DerivationExercise manipulations ----------------------- */

let test_blank_spec_defaults = () => {
  let spec = Web.DerivationExercise.blank_spec(~title="t", ~module_name="M");
  check(string, "title", "t", spec.title);
  check(string, "module_name", "M", spec.module_name);
  check(int, "max_points default is 10", 10, spec.max_points);
  check(
    bool,
    "rule_set defaults to PropositionalLogic",
    true,
    spec.rule_set == PropositionalLogic,
  );
  check(int, "blank spec starts with one tree", 1, List.length(spec.trees));
};

/* The tree-manipulation helpers (`add_abbr`, `del_abbr`, ...) are defined
   over `p(Editor.t)` because they need to build fresh `Editor.t` nodes via
   `DerivationExercise.init`. We lift a blank spec into an `eds` so we can
   exercise them in tests. */
let eds_of_blank_spec = (): Web.DerivationExercise.eds => {
  let spec = Web.DerivationExercise.blank_spec(~title="t", ~module_name="M");
  Web.DerivationExercise.mapi(spec, (pos, z) =>
    Editor.Model.mk(z, ~root=Web.DerivationExercise.root_of_pos(pos))
  );
};

let test_add_abbr_grows_trees = () => {
  let eds = eds_of_blank_spec();
  let eds' = Web.DerivationExercise.add_abbr(eds, ~index=0);
  check(
    int,
    "add_abbr increments tree count",
    List.length(eds.trees) + 1,
    List.length(eds'.trees),
  );
};

let test_del_abbr_shrinks_trees = () => {
  let eds = eds_of_blank_spec();
  let eds' = Web.DerivationExercise.add_abbr(eds, ~index=0);
  let eds'' = Web.DerivationExercise.del_abbr(eds', ~index=0);
  check(
    int,
    "add_abbr then del_abbr returns to original tree count",
    List.length(eds.trees),
    List.length(eds''.trees),
  );
};

/* Adding an abbreviation at index 0 should renumber an existing
   `Abbr(Some(0))` reference inside later trees up to `Abbr(Some(1))`. */
let test_add_abbr_shifts_references = () => {
  let eds = eds_of_blank_spec();
  /* Add a premise to the first (and only) tree, then replace that premise
     with an abbreviation reference pointing at tree 0. */
  let with_prem =
    Web.DerivationExercise.add_premise(eds, ~pos=Trees(0, Value), ~index=0);
  let with_ref =
    Web.DerivationExercise.switch_abbr(
      with_prem,
      ~pos=Trees(0, Children(0, Value)),
      ~index=Some(0),
    );
  let grown = Web.DerivationExercise.add_abbr(with_ref, ~index=0);
  /* The original tree has shifted to position 1. Its single child should now
     be `Abbr(Some(1))`. */
  let tree = List.nth(grown.trees, 1);
  switch (Tree.nth(tree, Children(0, Value))) {
  | Abbr(Some(1)) => ()
  | Abbr(other) =>
    Alcotest.failf(
      "expected Abbr(Some(1)) after shift, got Abbr(%s)",
      switch (other) {
      | None => "None"
      | Some(n) => "Some(" ++ string_of_int(n) ++ ")"
      },
    )
  | Just(_) => Alcotest.fail("expected an Abbr node, got Just")
  };
};

/* ----------------------- test registration ----------------------- */

let tests = (
  "Derivation",
  [
    /* syntax */
    test_case("parse Truth", `Quick, test_parse_truth),
    test_case("parse Falsity", `Quick, test_parse_falsity),
    test_case("parse Var (capitalized)", `Quick, test_parse_var),
    test_case("parse A /\\ B", `Quick, test_parse_and),
    test_case("parse A \\/ B", `Quick, test_parse_or),
    test_case("parse A ==> B", `Quick, test_parse_impl),
    test_case(
      "parse /\\ binds tighter than \\/",
      `Quick,
      test_parse_and_or_precedence,
    ),
    test_case(
      "parse \\/ binds tighter than ==>",
      `Quick,
      test_parse_or_impl_precedence,
    ),
    test_case("parse ! as Impl(_, Falsity)", `Quick, test_parse_not_sugar),
    test_case("parse |- as Entail", `Quick, test_parse_entail),
    test_case("parse [A, B] as Ctx", `Quick, test_parse_ctx_literal),
    test_case("parse [] as empty Ctx", `Quick, test_parse_empty_ctx),
    /* abbreviations */
    test_case("parse $a as Quote", `Quick, test_parse_dollar_var_is_quote),
    test_case(
      "parse $foo (longer name) as Quote",
      `Quick,
      test_parse_dollar_longer_name,
    ),
    test_case(
      "parse plain a as Var (not Quote)",
      `Quick,
      test_parse_plain_var_is_not_quote,
    ),
    test_case(
      "parse $a /\\ $b without spaces around $",
      `Quick,
      test_parse_dollar_in_conjunction,
    ),
    /* rule verification */
    test_case("Truth_I correct", `Quick, test_truth_i_correct),
    test_case("Truth_I wrong conclusion", `Quick, test_truth_i_wrong_concl),
    test_case("Truth_I extra premise", `Quick, test_truth_i_extra_premise),
    test_case("Assumption [A] |- A", `Quick, test_assumption_single),
    test_case("Assumption [A, B] |- B", `Quick, test_assumption_picks_member),
    test_case("Assumption [] |- A fails", `Quick, test_assumption_empty_ctx),
    test_case(
      "Assumption [B] |- A fails",
      `Quick,
      test_assumption_not_in_ctx,
    ),
    test_case("And_I correct", `Quick, test_and_i_correct),
    test_case("And_I missing premise", `Quick, test_and_i_missing_premise),
    test_case("And_I wrong connective", `Quick, test_and_i_wrong_connective),
    test_case("And_I mismatched contexts", `Quick, test_and_i_mismatched_ctx),
    test_case("And_E_L correct", `Quick, test_and_e_l_correct),
    test_case("And_E_L wrong side", `Quick, test_and_e_l_takes_right),
    test_case("And_E_R correct", `Quick, test_and_e_r_correct),
    test_case("Or_I_L correct", `Quick, test_or_i_l_correct),
    test_case("Or_I_L wrong side", `Quick, test_or_i_l_wrong_side),
    test_case("Or_I_R correct", `Quick, test_or_i_r_correct),
    test_case("Implies_I correct", `Quick, test_implies_i_correct),
    test_case(
      "Implies_I ctx not extended",
      `Quick,
      test_implies_i_context_not_extended,
    ),
    test_case("Implies_E (modus ponens)", `Quick, test_implies_e_correct),
    test_case("Falsity_E correct", `Quick, test_falsity_e_correct),
    /* rule_set membership */
    test_case(
      "PropositionalLogic rule_set membership",
      `Quick,
      test_propositional_rule_set_only_includes_prop_logic,
    ),
    /* exercise manipulations */
    test_case("blank_spec defaults", `Quick, test_blank_spec_defaults),
    test_case("add_abbr grows trees", `Quick, test_add_abbr_grows_trees),
    test_case("del_abbr shrinks trees", `Quick, test_del_abbr_shrinks_trees),
    test_case(
      "add_abbr shifts existing refs",
      `Quick,
      test_add_abbr_shifts_references,
    ),
  ],
);
