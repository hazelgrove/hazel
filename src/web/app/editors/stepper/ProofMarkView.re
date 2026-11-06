open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Language;

/* Proof-check marks (ProofMark.t) as messages, shared by the cursor
   inspector, the stepper and the obligations panel so a failure reads the
   same everywhere. */

/* How the inspector, the marks and the obligations show expressions:
 * inline, with function bodies folded and fixpoints hidden, since an
 * `eval` step can leave recursive definitions inlined in a goal. */
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
  hole_tiles: false,
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

/* The statics' inexhaustiveness message (Mark.InexhaustiveMatch), worded
 * like the cursor inspector's, with a missing pattern. */
let inexhaustive_message = (~globals, example: Any.t): list(Node.t) => [
  text("Cases are inexhaustive. An example of a missing pattern is "),
  view_any(~globals, example),
];

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
        "Rule \""
        ++ equality
        ++ "\" matched, but quantified variables remain undetermined; supply an explicit instantiation.",
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
  | ExpectedFunEqGoal => [
      text("Expected a goal of the form `f == g` between two functions."),
    ]
  | MalformedFunext => [
      text("`funext` needs a variable pattern to name the argument."),
    ]
  | MalformedGeneralize => [
      text("`generalize` expects a bare in-scope variable."),
    ]
  | InductionNotExhaustive => [
      text("Induction cases don't cover the scrutinee's type."),
    ]
  | InductionScrutineeUntyped => [
      text(
        "The scrutinee's type is unknown, so these cases cannot be shown to cover it. Annotate the binder.",
      ),
    ]
  | PossiblyDivergentScrutinee => [
      text("The split scrutinee may not terminate."),
    ]
  | UnknownFactReverted => [text("No fact in scope matches this `revert`.")]
  | UnknownFactContradicted => [
      text("No fact in scope matches this `contradiction`."),
    ]
  | ContradictionNotFalse => [
      text("This fact does not compute to `false` under this rewrite."),
    ]
  | ContradictionSubstitutionUnverified({var}) => [
      text("No fact in scope is an equation on `" ++ var ++ "`."),
    ]
  | UnknownInstantiationVar({equality, var}) => [
      text("`" ++ var ++ "` is not a binder of `" ++ equality ++ "`."),
    ]
  | RevertFactNotQuantified({var}) => [
      text("The reverted fact is not quantified over `" ++ var ++ "`."),
    ]
  | InductionEmptyCases => [text("Induction requires at least one case.")]
  | InductionUnsupportedWildcard => [
      span(
        ~attrs=[clss(["induction-wildcard-warning"])],
        [
          text(
            "Wildcard case patterns are not supported in proofs yet. No case of this split is checked.",
          ),
        ],
      ),
    ]
  };
};
