open Util;
open Language;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cached_elab_subst: Calc.saved(Exp.t),
    root: StepperBase.step_model,
  };

  let init = {
    cached_elab_subst: Calc.Pending,
    root: StepperBase.init_step,
  };

  let get_validity = (m: t) => StepperBase.Stepper.get_validity(m.root);
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = StepperBase.step_action;

  let update = (~settings, action, model: Model.t) => {
    open Updated;
    let* root = StepperBase.Stepper.update(~settings, action, model.root);
    {
      ...model,
      root,
    };
  };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~ctx: Calc.t(SemanticCtx.t),
        elab: Calc.t(Exp.t),
        ~ana=Calc.OldValue(Typ.fresh(Unknown(SynSwitch))),
        /* The proof sub-term this stepper is rendering: `Some` when invoked
         * from the per-theorem proof view in `Theorems.re`, `None` for the
         * cell-level result stepper (which has no proof context). When
         * `Some`, proof-aware step kinds source their display from the
         * proof tree rather than from stepper-local state. */
        ~proof: Calc.t(option(Proof.t))=Calc.OldValue(None),
        /* Big-step proof-check results for the surrounding theorem; empty
         * for non-proof steppers. */
        ~proof_map: Calc.t(ProofMap.t)=Calc.OldValue(ProofMap.empty),
        /* Statics info map of the whole theorem (proof syntax included), so
         * proof-aware steps can read static errors (e.g. the InductionStep
         * exhaustiveness label). Empty for non-proof steppers. */
        ~proof_info_map: Calc.t(Statics.Map.t)=Calc.OldValue(Id.Map.empty),
        {cached_elab_subst, root}: Model.t,
      )
      : Model.t => {
    let elab_subst =
      cached_elab_subst
      |> {
        open Calc.Syntax;
        let.calc elab = elab;
        elab |> Substitution.in_exp(Builtins.env_init) |> Exp.replace_all_ids;
      };
    let (root, _, _) =
      StepperBase.Stepper.calculate(
        ~settings,
        ~ctx,
        ~exp=elab_subst,
        ~ana,
        ~proof,
        ~proof_map,
        ~proof_info_map,
        root,
      );
    {
      cached_elab_subst: elab_subst |> Calc.save,
      root,
    };
  };

  let can_undo = StepperBase.Stepper.can_undo;
};

module Focus = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = StepperBase.step_focus;

  open Cursor;

  let get_cursor_info =
      (~inject, ~focus: t, model: Model.t): cursor(Update.t) => {
    let+ ci =
      StepperBase.Stepper.get_cursor_info(~inject, ~focus, model.root);
    ci;
  };
};

module View = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type event =
    | MakeActive(Focus.t)
    | HideStepper;

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Focus.t),
        ~is_toplevel=true,
        /* Side-channel for proof-mode steppers to publish syntactic edits
         * back to the main editor. Emitting an `EditorTransform.patch`
         * here is how step views commit changes to the underlying
         * `Theorem`/`Proof.t` tree instead of mutating stepper-local
         * state. Default no-op for the cell-level result stepper, which
         * has no syntax tree behind it. */
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=_ =>
                                                                    Ui_effect.Ignore,
        /* Main-editor capability handle for sub-editor step views (see
         * SubEditor.re); None outside a cell with a backing editor. */
        ~main_editor: option(CodeEditable.Channel.t)=None,
        model: Model.t,
      ) => {
    let settings_modal =
      is_toplevel && globals.settings.core.evaluation.show_settings
        ? SettingsModal.view(
            ~inject=u => globals.inject_global(Set(u)),
            globals.settings.core.evaluation,
          )
        : [];
    settings_modal
    @ StepperBase.Stepper.view(
        ~globals,
        ~take_focus=f => signal(MakeActive(f)),
        ~hide_stepper=signal(HideStepper),
        ~inject=u => inject(u),
        ~is_toplevel,
        ~focus=selected,
        ~edit_syntax,
        ~main_editor,
        model.root,
      );
  };
};
