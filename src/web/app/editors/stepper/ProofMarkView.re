open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Language;

/* PROOFMARKVIEW.re — the single renderer for proof-check error marks
   (ProofMark.t), shared by the cursor inspector and the stepper so the
   same failure reads identically everywhere it appears. */

/* Mirrors the cursor inspector's expression display settings: inline,
 * with function bodies folded and fixpoints hidden, since proof `eval`
 * steps can leave recursive definitions inlined inside the goal (see
 * `ProofHacks.nth_exp_env`). */
let code_view_settings: Haz3lcore.ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `Fold,
  hide_fixpoints: true,
  show_ascriptions: true,
  show_filters: false,
  show_unknown_as_hole: true,
  use_literal_lexemes: false,
  project_tables: false,
};

let view_exp = (~globals, exp: Exp.t) =>
  div(
    ~attrs=[clss(["code-box-container"])],
    [CodeViewable.view_exp(~globals, ~settings=code_view_settings, exp)],
  );

let view_any = (~globals, any: Any.t) =>
  div(
    ~attrs=[clss(["code-box-container"])],
    [CodeViewable.view_any(~globals, ~settings=code_view_settings, any)],
  );

/* The statics-side inexhaustiveness message (Mark.InexhaustiveMatch),
 * phrased like the cursor inspector's, with the checker's witness for
 * a missing pattern. */
let inexhaustive_message = (~globals, example: Any.t): list(Node.t) => [
  text("Cases are inexhaustive. An example of a missing pattern is "),
  view_any(~globals, example),
];

/* Render a single proof-check mark as a human-readable message. These
 * are emitted by ProofCheck at evaluation time (see ProofMark.t) and
 * describe ways a specific proof step went wrong. */
let message = (~globals, m: ProofMark.t): list(Node.t) => {
  let view_exp_box = view_exp(~globals);
  switch (m) {
  | MissingIncoming => [
      text("No incoming goal to act on (an earlier step failed)."),
    ]
  | MalformedProofTerm => [
      text("This step contains text that isn't a valid proof step."),
    ]
  | MalformedEqualityName => [
      text("Expected an equality name (a variable referring to an axiom)."),
    ]
  | UnknownEquality(name) => [text("Unknown equality \"" ++ name ++ "\".")]
  | RuleDoesNotApply({equality, direction}) => [
      text(
        "Equality \""
        ++ equality
        ++ "\" doesn't apply in the "
        ++ (
          switch ((direction: Direction.t)) {
          | Left => "left"
          | Right => "right"
          }
        )
        ++ " direction here.",
      ),
    ]
  | UnderdeterminedInstantiation({equality}) => [
      text(
        "Conditional rule \""
        ++ equality
        ++ "\" matched, but its side conditions mention variables the match did not determine.",
      ),
    ]
  | PossiblyDivergentInstantiation({equality, var}) => [
      text(
        "Applying \""
        ++ equality
        ++ "\" here instantiates "
        ++ var
        ++ " at an expression that may not terminate.",
      ),
    ]
  | FloatAlgebrite => [
      text("Algebraic rewrites are refused on float-typed expressions."),
    ]
  | MalformedIndex => [text("Expected a numeric index literal.")]
  | PatternNotFound({at_exp, at_idx}) => [
      text("Could not find occurrence #" ++ string_of_int(at_idx) ++ " of "),
      view_exp_box(at_exp),
      text(" in the goal."),
    ]
  | NothingToStep({at_exp}) => [
      text("Nothing to evaluate in "),
      view_exp_box(at_exp),
      text("."),
    ]
  | ExpectedForallGoal => [text("Expected a `forall` goal here.")]
  | MalformedGeneralize => [
      text("`generalize` expects a bare in-scope variable."),
    ]
  | InductionNotExhaustive => [
      text("Induction cases don't cover the scrutinee's type."),
    ]
  | PossiblyDivergentScrutinee => [
      text("The split scrutinee may not terminate."),
    ]
  | UnknownFactReverted => [text("No fact in scope matches this `revert`.")]
  | InductionEmptyCases => [text("Induction requires at least one case.")]
  };
};
