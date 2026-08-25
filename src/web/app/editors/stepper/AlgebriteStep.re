open Util;
open Language;
open StepInterface;
open OptUtil.Syntax;
open Calc.Syntax;

/* Types are defined outside the functor to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  at_idx: int,
  at_exp: Exp.t,
  with_exp: Exp.t,
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
    let {at_idx, at_exp, with_exp, next_exp} = model;
    /* Mirror AxiomStep / EvalStep: when an `AlgebriteStep` proof node
     * is in scope, override the model fields from the syntax so the
     * placeholder inserted by `adapt_step_kind` immediately reflects
     * the freshly-patched proof. */
    let (at_idx, at_exp, with_exp) =
      switch (Calc.get_value(proof)) {
      | {term: AlgebriteStep({at_idx: ai, at_exp: ae, with_exp: we}), _} =>
        let idx = ProofCheck.exp_to_int(ai) |> Option.value(~default=at_idx);
        (idx, ae, we);
      | _ => (at_idx, at_exp, with_exp)
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
        /* Same source-of-truth order as AxiomStep:
         *   1. the big-step ProofMap entry for this proof sub-term,
         *   2. otherwise re-run the canonical rewrite locally.
         * Either way the target is located env-aware, so the row agrees
         * with the checker (and with the `at <n>` the overlay wrote
         * against the env-inlined display). */
        switch (ProofMap.lookup(Proof.rep_id(proof), proof_map)) {
        | Some({outgoing: Some(_) as outgoing, _}) => outgoing
        | _ =>
          ProofCheck.algebrite_step_outgoing(
            ~info_map,
            ~env=SemanticCtx.get_env(ctx),
            ~at_idx,
            ~at_exp,
            ~with_exp,
            exp,
          )
        };
      }
      |> Calc.to_option;
    {
      at_idx,
      at_exp,
      with_exp,
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
        _m: model,
      ) =>
    WebUtil.Node.text("algebra");

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
