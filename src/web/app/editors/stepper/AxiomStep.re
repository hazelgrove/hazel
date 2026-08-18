open Util;
open Language;
open StepInterface;
open OptUtil.Syntax;
open Calc.Syntax;

/* Types are defined outside the functor to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  name: string,
  at_idx: int,
  at_exp: Exp.t,
  direction: Direction.t,
  equality: string,
  next_exp: Calc.saved(Exp.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  |;

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  |;

/* The methods in this file, like the other step files, are
   parameterized by a Stepper module that implements the
   stepper interface. This allows us to use steppers inside
   steps inside steppers. The lines below can be copied as
   boilerplate to other steps.*/
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
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~editor as _: Calc.t(CodeSelectable.Model.t),
        ~info_map,
        ~proof_info_map as _,
        ~ana as _,
        ~proof: Calc.t(Proof.t),
        ~proof_map: Calc.t(ProofMap.t),
        model: model,
      ) => {
    let {name, at_idx, at_exp, direction, equality, next_exp} = model;
    /* When a Proof.t sub-term is in scope (theorem-proof stepper),
     * derive the structural fields (name / at_idx / at_exp / direction /
     * equality) from syntax instead of stepper-local state. The
     * stepper-local copies stay populated for the cell-level stepper and
     * as a fallback while the rest of the kinds are migrated. */
    let (name, at_idx, at_exp, direction, equality) =
      switch (Calc.get_value(proof)) {
      | {
          term:
            AxiomStep({at_idx: ai, at_exp: ae, direction: dir, equality: eq}),
          _,
        } =>
        let idx = ProofCheck.exp_to_int(ai) |> Option.value(~default=at_idx);
        let eq_name =
          ProofCheck.exp_to_equality_name(eq)
          |> Option.value(~default=equality);
        (eq_name, idx, ae, dir, eq_name);
      | _ => (name, at_idx, at_exp, direction, equality)
      };
    let+ next_exp =
      next_exp
      |> Calc.map_saved(Option.some)
      |> {
        let.calc exp = exp
        and.calc ctx = ctx
        and.calc info_map = info_map
        and.calc proof = proof
        and.calc proof_map = proof_map;
        /* Source of truth for the rewritten expression:
         *   1. The big-step ProofMap entry for this proof sub-term, when
         *      we have one (already computed by the evaluator).
         *   2. Otherwise re-run the canonical axiom-step rewrite locally
         *      using the model fields (cell-level stepper / fallback). */
        switch (ProofMap.lookup(Proof.rep_id(proof), proof_map)) {
        | Some({outgoing: Some(_) as outgoing, _}) => outgoing
        | _ =>
          ProofCheck.axiom_step_outgoing(
            ~info_map,
            ~env=SemanticCtx.get_env(ctx),
            ~ctx=SemanticCtx.get_ctx(ctx),
            ~at_idx,
            ~at_exp,
            ~direction,
            ~equality,
            exp,
          )
        };
      }
      |> Calc.to_option;
    {
      name,
      at_idx,
      at_exp,
      direction,
      equality,
      next_exp: next_exp |> Calc.save,
    };
  };

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
        ~proof as _: option(Proof.t),
        ~edit_syntax as
          _: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        m: model,
      ) =>
    WebUtil.Node.text(m.name);

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
