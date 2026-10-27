open Util;
open Language;
open StepInterface;

/* `== true by <justification> end` as a stepper row (FR-exists-1). A
 * TERMINAL step like `ContradictionStep`: no model, no actions, no nested
 * stepper. The justification lives in the proof syntax; the checker's
 * outgoing — for `witness v` the instantiated body P[v], for a fact name
 * `true` — is rendered by the surrounding row machinery as the next row,
 * where the user keeps stepping. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = unit;

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  |;

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  |;

module F =
       (Stepper: STEPPER)

         : (
           STEP with
             type model = model'(Stepper.model) and
             type action = action'(Stepper.action) and
             type focus = focus'(Stepper.focus)
       ) => {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = model'(Stepper.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = action'(Stepper.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = focus'(Stepper.focus);

  let update = (~settings as _: Settings.t, action: action, _model: model) =>
    switch (action) {
    | _ => .
    };

  let can_undo = _ => false;

  let calculate =
      (
        ~settings as _: Calc.t(CoreSettings.t),
        ~hidden as _: Calc.saved(bool),
        ~exp as _: Calc.t(Exp.t),
        ~ctx as _: Calc.t(SemanticCtx.t),
        ~editor as _: Calc.t(CodeSelectable.Model.t),
        ~info_map as _: Calc.t(Statics.Map.t),
        ~proof_info_map as _: Calc.t(Statics.Map.t),
        ~ana as _: Calc.t(Typ.t),
        ~proof as _: Calc.t(Proof.t),
        ~proof_map as _: Calc.t(ProofMap.t),
        model: model,
      ) =>
    Some(model);

  let get_cursor_info = (~inject as _, ~focus: focus, _model: model) =>
    switch (focus) {
    | _ => .
    };

  let view_justification =
      (
        ~globals as _: Globals.t,
        ~focus as _: option(focus),
        ~inject as _: action => Ui_effect.t(unit),
        ~take_focus as _: focus => Ui_effect.t(unit),
        ~hide_stepper as _: Ui_effect.t(unit),
        ~is_toplevel as _: bool,
        ~proof: option(Proof.t),
        ~edit_syntax as
          _: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        _m: model,
      ) => {
    let rec strip = (p: Proof.t) =>
      switch (p.term) {
      | Parens(p) => strip(p)
      | _ => p
      };
    WebUtil.Node.text(
      switch (Option.map(strip, proof)) {
      | Some({term: RelEqBy(_, j), _}) =>
        switch (strip(j).term) {
        | Witness(_) => "witness"
        | Cite(h) => "by " ++ h
        | _ => "relation"
        }
      | _ => "relation"
      },
    );
  };

  let view_content =
      (
        ~globals as _: Globals.t,
        ~focus as _: option(focus),
        ~inject as _: action => Ui_effect.t(unit),
        ~take_focus as _: focus => Ui_effect.t(unit),
        ~hide_stepper as _: Ui_effect.t(unit),
        ~is_toplevel as _: bool,
        ~proof as _: option(Proof.t),
        ~edit_syntax as
          _: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        ~main_editor as _: option(CodeEditable.Channel.t),
        _model: model,
      ) =>
    [];
};
