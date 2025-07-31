open Util;
open Language;
open Haz3lcore;
open StepInterface;

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  // Updated
  pattern: CodeEditable.Model.t,
  // Calculated
  elab_pattern: Calc.saved(Pat.t),
  inner_exp: Calc.saved(Exp.t),
  step: 'stepper,
  last_exp: Calc.saved(Exp.t),
  hypo_points: Calc.saved(list(Exp.t)),
  inner_ctx: Calc.saved(Ctx.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('stepper) =
  | PatternUpdate(CodeEditable.Update.t)
  | StepUpdate('stepper);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('stepper) =
  | Pattern(CodeSelectable.Selection.t)
  | Stepper('stepper);

module F = (Stepper: STEPPER) => {
  type model = model'(Stepper.model);
  type action = action'(Stepper.action);
  type focus = focus'(Stepper.focus);

  let init = {
    pattern: CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
    elab_pattern: Calc.Pending,
    inner_exp: Calc.Pending,
    step: Stepper.init,
    last_exp: Calc.Pending,
    hypo_points: Calc.Pending,
    inner_ctx: Calc.Pending,
  };

  let update = (~settings: Settings.t, action: action, model: model) => {
    Updated.(
      switch (action) {
      | PatternUpdate(a) =>
        let* new_pattern =
          CodeEditable.Update.update(~settings, a, model.pattern);
        {
          ...model,
          pattern: new_pattern,
        };
      | StepUpdate(a) =>
        let* new_step = Stepper.update(~settings, a, model.step);
        {
          ...model,
          step: new_step,
        };
      }
    );
  };

  let can_undo = a =>
    switch (a) {
    | PatternUpdate(action) => CodeEditable.Update.can_undo(action)
    | StepUpdate(action) => Stepper.can_undo(action)
    };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~elab_scrut: Calc.t(Exp.t),
        ~scrut_ty: Calc.t(Typ.t),
        ~ctx: Calc.t(Ctx.t),
        ~exp: Calc.t(Exp.t),
        ~state: Calc.t(EvaluatorState.t),
        model: model,
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
        ~is_dynamic_term=true,
        model.pattern,
      );
    let elab_pattern =
      Calc.set(
        ~eq=Pat.fast_equal,
        CodeEditable.Model.get_statics(pattern).elaborated
        |> ProofHacks.remove_wrapping_function,
        model.elab_pattern,
      );
    let inner_exp =
      model.inner_exp
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
      model.hypo_points
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
    let inner_ctx =
      model.inner_ctx
      |> {
        open Calc.Syntax;
        let.calc elab_pattern = elab_pattern
        and.calc scrut_ty = scrut_ty
        and.calc ctx = ctx;
        ProofHacks.dhpat_extend_ctx(elab_pattern, scrut_ty, ctx)
        |> Option.value(~default=ctx);
      };
    let (stepper, last_exp) =
      Stepper.calculate(
        ~settings, // TODO: this is a little ugly
        ~ctx=inner_ctx,
        ~exp=inner_exp,
        ~state,
        model.step,
      );
    {
      pattern,
      elab_pattern: elab_pattern |> Calc.save,
      inner_exp: inner_exp |> Calc.save,
      hypo_points: hypo_points |> Calc.save,
      step: stepper,
      last_exp: last_exp |> Calc.save,
      inner_ctx: inner_ctx |> Calc.save,
    };
  };

  let get_cursor_info = (~focus: focus, model: model) => {
    Cursor.(
      switch (focus) {
      | Pattern(a) =>
        let+ ci =
          CodeEditable.Selection.get_cursor_info(~selection=a, model.pattern);
        PatternUpdate(ci);
      | Stepper(a) =>
        let+ ci = Stepper.get_cursor_info(~focus=a, model.step);
        StepUpdate(ci);
      }
    );
  };

  let handle_key_event = (~focus: focus, ~event: Key.t, model: model) => {
    switch (focus, model) {
    | (Pattern(a), _) =>
      CodeEditable.Selection.handle_key_event(
        ~selection=a,
        model.pattern,
        event,
      )
      |> Option.map(x => PatternUpdate(x))
    | (Stepper(a), _) =>
      Stepper.handle_key_event(~focus=a, ~event, model.step)
      |> Option.map(x => StepUpdate(x))
    };
  };

  let view =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~remove_case: Ui_effect.t(unit),
        model: model,
      ) => {
    let remove_case_button =
      Widgets.button(
        Icons.trash,
        _ => remove_case,
        ~tooltip="Remove case",
        ~clss=["subtle-button"],
      );
    let pattern_editor =
      CodeEditable.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => take_focus(Pattern()),
        ~inject=x => inject(PatternUpdate(x)),
        ~selected=
          switch (focus) {
          | Some(Pattern ()) => true
          | _ => false
          },
        model.pattern,
      );
    let pattern_editor =
      WebUtil.div_c("inline-editor-wrapper", [pattern_editor]);
    let stepper_view =
      Stepper.view(
        ~globals,
        ~take_focus=s => take_focus(Stepper(s)),
        ~hide_stepper,
        ~inject=x => inject(StepUpdate(x)),
        ~focus=
          switch (focus) {
          | Some(Stepper(f)) => Some(f)
          | _ => None
          },
        ~is_toplevel=false,
        model.step,
      );
    WebUtil.div_c(
      "induction-case",
      [
        WebUtil.div_c(
          "induction-case-header",
          [
            remove_case_button,
            WebUtil.Node.text("Case "),
            pattern_editor,
            WebUtil.Node.text(" : "),
          ],
        ),
      ]
      @ stepper_view,
    );
  };
};
