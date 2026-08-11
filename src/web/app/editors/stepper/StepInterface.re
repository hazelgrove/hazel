open Util;
open Language;

module type STEP = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus;

  let update: (~settings: Settings.t, action, model) => Updated.t(model);

  let can_undo: action => bool;

  let calculate:
    (
      ~settings: Calc.t(CoreSettings.t),
      ~hidden: Calc.saved(bool),
      ~exp: Calc.t(Exp.t),
      ~ctx: Calc.t(SemanticCtx.t),
      ~editor: Calc.t(CodeSelectable.Model.t),
      ~info_map: Calc.t(Statics.Map.t),
      /* Statics info map of the *whole theorem* (proof syntax included), as
       * opposed to `info_map` which is the goal-expression's. Steps that need
       * to surface a static error computed over the proof syntax (e.g. the
       * InductionStep exhaustiveness label) read it here. Empty for the
       * cell-level stepper. */
      ~proof_info_map: Calc.t(Statics.Map.t),
      ~ana: Calc.t(Typ.t),
      /* The proof sub-term this step renders (Some when inside a Theorem
       * proof; None in the cell-level result stepper). Steps that source
       * their display from syntax read parameters off this term. */
      ~proof: Calc.t(option(Proof.t)),
      /* Big-step proof-check results for the surrounding theorem (incoming /
       * outgoing expressions and marks, keyed by Proof.rep_id). Steps that
       * source their display from syntax read next_exp / validity off this
       * map instead of re-running ProofCheck locally. */
      ~proof_map: Calc.t(ProofMap.t),
      model
    ) =>
    option(
      (
        model,
        Calc.t(bool), // Hidden
        option(Calc.t(Exp.t)), // Next
        Calc.t(option(bool)) // Truth
      ),
    );

  let get_cursor_info:
    (~inject: action => Ui_effect.t(unit), ~focus: focus, model) =>
    Cursor.cursor(action);

  let view_justification:
    (
      ~globals: Globals.t,
      ~focus: option(focus),
      ~inject: action => Ui_effect.t(unit),
      ~take_focus: focus => Ui_effect.t(unit),
      ~hide_stepper: Ui_effect.t(unit),
      ~undo: option(Ui_effect.t(unit)),
      ~is_toplevel: bool,
      /* See note on ~proof / ~edit_syntax in view_content below. */
      ~proof: option(Proof.t),
      ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
      model
    ) =>
    WebUtil.Node.t;

  let view_content:
    (
      ~globals: Globals.t,
      ~focus: option(focus),
      ~inject: action => Ui_effect.t(unit),
      ~take_focus: focus => Ui_effect.t(unit),
      ~hide_stepper: Ui_effect.t(unit),
      ~undo: option(Ui_effect.t(unit)),
      ~is_toplevel: bool,
      /* The Proof.t sub-term this step renders (when inside a Theorem
       * proof). Step kinds whose interactive buttons mutate the proof
       * structure (e.g. InductionStep adding / removing cases) read
       * this to build patches targeting the right `Proof.rep_id`. */
      ~proof: option(Proof.t),
      /* Write channel for syntax-side edits emitted from step views.
       * Routed up through `StepperView` to the surrounding cell
       * editor; defaults to a no-op for cell-level steppers. */
      ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
      /* Capability handle on the main editor (model + inject), used by
       * step views that render slices of the surrounding syntax as
       * sub-editors (see SubEditor.re). None for cell-level steppers,
       * which have no backing syntax. */
      ~main_editor: option(CodeEditable.Channel.t),
      model
    ) =>
    list(WebUtil.Node.t);
};

module type STEPPER = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus;

  let init: model;

  let update: (~settings: Settings.t, action, model) => Updated.t(model);

  let can_undo: action => bool;

  let calculate:
    (
      ~settings: Calc.t(CoreSettings.t),
      ~exp: Calc.t(Exp.t),
      ~ctx: Calc.t(SemanticCtx.t),
      ~ana: Calc.t(Typ.t),
      /* See note on ~proof in STEP.calculate above. */
      ~proof: Calc.t(option(Proof.t)),
      /* See note on ~proof_map in STEP.calculate above. */
      ~proof_map: Calc.t(ProofMap.t),
      /* See note on ~proof_info_map in STEP.calculate above. Optional;
       * defaults to empty for cell-level steppers with no theorem statics. */
      ~proof_info_map: Calc.t(Statics.Map.t)=?,
      model
    ) =>
    (model, Calc.t(Exp.t), Calc.t(option(bool)) /* Truth */);

  let get_cursor_info:
    (~inject: action => Ui_effect.t(unit), ~focus: focus, model) =>
    Cursor.cursor(action);

  let view:
    (
      ~globals: Globals.t,
      ~take_focus: focus => Ui_effect.t(unit),
      ~inject: action => Ui_effect.t(unit),
      ~hide_stepper: Ui_effect.t(unit),
      ~focus: option(focus),
      ~is_toplevel: bool,
      /* Optional write channel for syntax-side edits emitted by step
       * views (e.g. MissingStep when the user picks an axiom). Defaults
       * to a no-op for the cell-level stepper / out-of-theorem uses. */
      ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=?,
      /* Optional main-editor capability handle for sub-editor views;
       * see the note on STEP.view_content above. Defaults to None. */
      ~main_editor: option(CodeEditable.Channel.t)=?,
      model
    ) =>
    list(WebUtil.Node.t);
};
