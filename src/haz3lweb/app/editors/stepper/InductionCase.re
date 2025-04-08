open Util;
open Haz3lcore;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('step) = {
    // Updated
    pattern: CodeEditable.Model.t,
    // Calculated
    elab_pattern: Calc.saved(Pat.t),
    inner_exp: Calc.saved(Exp.t),
    step: 'step,
    last_exp: Calc.saved(Exp.t),
    hypo_points: Calc.saved(list(Exp.t)),
  };

  let init = step => {
    pattern: CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
    elab_pattern: Calc.Pending,
    inner_exp: Calc.Pending,
    step,
    last_exp: Calc.Pending,
    hypo_points: Calc.Pending,
  };
};

module Update = {
  open Updated;
  // open Calc.Syntax;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('step) =
    | PatternUpdate(CodeEditable.Update.t)
    | StepUpdate('step);

  let update =
      (
        type step,
        type step_model,
        ~update_step,
        ~settings,
        action: t(step),
        model: Model.t(step_model),
      )
      : Updated.t(Model.t(step_model)) =>
    switch (action) {
    | PatternUpdate(a) =>
      let* new_pattern =
        CodeEditable.Update.update(~settings, a, model.pattern);
      {
        ...model,
        pattern: new_pattern,
      };
    | StepUpdate(a) =>
      let* new_step = update_step(~settings, a, model.step);
      {
        ...model,
        step: new_step,
      };
    };

  let calculate =
      (
        type step_model,
        ~calculate_step,
        ~settings,
        ~scrut_ty: Calc.t(Typ.t),
        ~elab_scrut: Calc.t(Exp.t),
        ctx,
        exp,
        state,
        Model.{
          pattern,
          elab_pattern,
          inner_exp,
          step: stepper,
          hypo_points,
          last_exp: _,
        }:
          Model.t(step_model),
      ) => {
    let pattern =
      CodeEditable.Update.calculate(
        ~settings=Calc.get_value(settings),
        ~dynamics=Dynamics.Map.empty,
        ~is_edited=true, // This editor technically edits Exps, but we want a Pat, so we put it in a function to emulate that.
        ~stitch=
          x =>
            x
            |> ProofHacks.exp_to_pat
            |> ProofHacks.add_wrapping_function(
                 ~typ=scrut_ty |> Calc.get_value,
               ),
        pattern,
      );
    let elab_pattern =
      Calc.set(
        ~eq=Pat.fast_equal,
        CodeEditable.Model.get_statics(pattern).elaborated
        |> ProofHacks.remove_wrapping_function,
        elab_pattern,
      );
    let inner_exp =
      inner_exp
      |> {
        open Calc.Syntax;
        let.calc elab_pattern = elab_pattern
        and.calc elab_scrut = elab_scrut
        and.calc exp = exp;
        DHExp.replace_exp(
          elab_scrut,
          elab_pattern |> ProofHacks.pat_to_exp,
          exp,
        );
      };
    let hypo_points =
      hypo_points
      |> {
        open Calc.Syntax;
        let.calc elab_pattern = elab_pattern
        and.calc scrut_ty = scrut_ty;
        ProofHacks.get_inductive_hypotheses(
          CodeEditable.Model.get_statics(pattern).info_map,
          scrut_ty,
          elab_pattern,
        )
        |> List.map(v => Exp.fresh(Var(v)));
      };
    let (stepper, last_exp) =
      calculate_step(
        ~settings, // TODO: this is a little ugly
        ctx,
        inner_exp,
        state,
        stepper,
      );
    Model.{
      pattern,
      elab_pattern: elab_pattern |> Calc.save,
      inner_exp: inner_exp |> Calc.save,
      hypo_points: hypo_points |> Calc.save,
      step: stepper,
      last_exp: last_exp |> Calc.save,
    };
  };
};

module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('step) =
    | Pattern(CodeSelectable.Selection.t)
    | Stepper('step);

  let get_cursor_info =
      (
        type step,
        type step_model,
        ~get_cursor_info_step,
        ~selection: t(step),
        ~model: Model.t(step_model),
      ) =>
    switch (selection) {
    | Pattern(a) =>
      let+ ci =
        CodeEditable.Selection.get_cursor_info(~selection=a, model.pattern);
      Update.PatternUpdate(ci);
    | Stepper(a) =>
      let+ ci = get_cursor_info_step(~selection=a, model.step);
      Update.StepUpdate(ci);
    };

  let handle_key_event =
      (
        type step,
        type step_model,
        ~handle_key_event_step,
        ~selection: t(step),
        ~event,
        model: Model.t(step_model),
      ) =>
    switch (selection) {
    | Pattern(a) =>
      CodeEditable.Selection.handle_key_event(
        ~selection=a,
        model.pattern,
        event,
      )
      |> Option.map(x => Update.PatternUpdate(x))
    | Stepper(a) =>
      handle_key_event_step(~selection=a, ~event, model.step)
      |> Option.map(x => Update.StepUpdate(x))
    };
};

module View = {
  open Web;

  type event('step_focus) =
    | MakeActive(Selection.t('step_focus))
    | HideStepper;

  let view =
      (
        type step_model,
        type step_update,
        type step_focus,
        ~view_stepper',
        ~globals: Globals.t,
        ~signal: event(step_focus) => Ui_effect.t(unit),
        ~inject: Update.t(step_update) => Ui_effect.t(unit),
        ~selected: option(Selection.t(step_focus)),
        model: Model.t(step_model),
      ) => {
    let pattern_editor =
      CodeEditable.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => signal(MakeActive(Pattern())),
        ~inject=x => inject(PatternUpdate(x)),
        ~selected=
          switch (selected) {
          | Some(Pattern(_)) => true
          | Some(_)
          | None => false
          },
        model.pattern,
      );

    let pattern_editor = div_c("inline-editor-wrapper", [pattern_editor]);

    let stepper_view =
      view_stepper'(
        ~globals,
        ~signal_make_active=x => signal(MakeActive(Stepper(x))),
        ~signal_hide_stepper=signal(HideStepper),
        ~inject=x => inject(StepUpdate(x)),
        ~selected=
          switch (selected) {
          | Some(Stepper(s)) => Some(s)
          | Some(_)
          | None => None
          },
        model.step,
      );
    div_c(
      "induction-case",
      [
        div_c(
          "induction-case-header",
          [Node.text("Pattern "), pattern_editor],
        ),
        div_c(
          "induction-case-hypotheses",
          List.flatten(
            List.map(
              x =>
                [
                  CodeViewable.view_segment(
                    ~globals,
                    ~sort=Exp,
                    ~shape_map=ProjectorCore.Shape.Map.empty,
                    ExpToSegment.exp_to_segment(
                      ~settings=
                        ExpToSegment.Settings.of_core(
                          ~inline=true,
                          globals.settings.core,
                        ),
                      x,
                    ),
                  ),
                  Node.text(", "),
                ],
              model.hypo_points |> Calc.get_saved_exc,
            ),
          ),
        ),
      ]
      @ stepper_view,
    );
  };
};
