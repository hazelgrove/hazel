open Util;
open Language;
open Haz3lcore;
open StepInterface;

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  // Updated
  pattern: EditorManager.Model.t,
  // Calculated
  elab_pattern: Calc.saved(Pat.t),
  inner_exp: Calc.saved(Exp.t),
  step: 'stepper,
  last_exp: Calc.saved(Exp.t),
  hypo_points: Calc.saved(list(Exp.t)),
  inner_ctx: Calc.saved(Ctx.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent'('stepper) = {
  pattern: EditorManager.Model.persistent,
  stepper: 'stepper,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('stepper) =
  | PatternUpdate(EditorManager.Update.t)
  | StepUpdate('stepper);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('stepper) =
  | Pattern(Editor.Focus.t)
  | Stepper('stepper);

module F = (Stepper: STEPPER) => {
  type model = model'(Stepper.model);
  type persistent = persistent'(Stepper.persistent);
  type action = action'(Stepper.action);
  type focus = focus'(Stepper.focus);

  let init = {
    pattern: EditorManager.Model.of_editor(Editor.of_zipper(Zipper.init())),
    elab_pattern: Calc.Pending,
    inner_exp: Calc.Pending,
    step: Stepper.init,
    last_exp: Calc.Pending,
    hypo_points: Calc.Pending,
    inner_ctx: Calc.Pending,
  };

  let persist = (model: model): persistent => {
    {
      pattern: model.pattern.editor |> Editor.get_z |> PersistentZipper.persist,
      stepper: Stepper.persist(model.step),
    };
  };

  let unpersist = (p: persistent): model => {
    {
      pattern:
        p.pattern
        |> PersistentZipper.unpersist
        |> Editor.of_zipper
        |> EditorManager.Model.of_editor,
      elab_pattern: Calc.Pending,
      inner_exp: Calc.Pending,
      step: Stepper.unpersist(p.stepper),
      last_exp: Calc.Pending,
      hypo_points: Calc.Pending,
      inner_ctx: Calc.Pending,
    };
  };

  let update = (~globals: Globals.t, action: action, model: model) => {
    let common: Common.global = Globals.to_common_global(globals);
    Updated.(
      switch (action) {
      | PatternUpdate(a) =>
        let* new_pattern =
          EditorManager.Update.update(
            ~common,
            ~dynamics=Dynamics.Map.empty,
            a,
            model.pattern,
          );
        {
          ...model,
          pattern: new_pattern,
        };
      | StepUpdate(a) =>
        let* new_step = Stepper.update(~globals, a, model.step);
        {
          ...model,
          step: new_step,
        };
      }
    );
  };

  let can_undo = (a: action): bool =>
    switch (a) {
    | PatternUpdate(action) => EditorManager.Update.can_undo(action)
    | StepUpdate(action) => Stepper.can_undo(action)
    };

  let calculate =
      (
        ~globals: Globals.t,
        ~settings: Calc.t(CoreSettings.t),
        ~elab_scrut: Calc.t(Exp.t),
        ~scrut_ty: Calc.t(Typ.t),
        ~scrut_co_ctx: Calc.t(CoCtx.t),
        ~ctx: Calc.t(Ctx.t),
        ~exp: Calc.t(Exp.t),
        ~state: Calc.t(EvaluatorState.t),
        model: model,
      ) => {
    let pattern =
      EditorManager.Update.calculate(
        ~common=Globals.to_common_global(globals),
        ~dynamics=Dynamics.Map.empty,
        // This editor technically edits Exps, but we want a Pat, so we put it in a function to emulate that.
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
    let statics = EditorManager.Model.get_statics(pattern);
    let elab_pattern =
      Calc.set(
        ~eq=Pat.fast_equal,
        statics.elaborated |> ProofHacks.remove_wrapping_function,
        model.elab_pattern,
      );
    let inner_exp =
      model.inner_exp
      |> {
        open Calc.Syntax;
        let.calc elab_pattern = elab_pattern
        and.calc elab_scrut = elab_scrut
        and.calc scrut_co_ctx = scrut_co_ctx
        and.calc exp = exp;
        ProofHacks.replace_exp(
          elab_scrut,
          scrut_co_ctx,
          elab_pattern |> ProofHacks.pat_to_exp,
          elab_pattern |> Pat.bindings |> CoCtx.of_bindings,
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
          statics.info_map,
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
        ~globals,
        ~settings,
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

  let get_cursor_info =
      (
        ~globals: Globals.t,
        ~inject: action => Ui_effect.t(unit),
        ~focus: focus,
        model: model,
      )
      : Haz3lcore.Cursor.t =>
    switch (focus) {
    | Pattern(ed_focus) =>
      EditorManager.Focus.get_cursor_info(
        ~common=Globals.to_common_global(globals),
        ~dynamics=Language.Dynamics.Map.empty,
        ~inject=x => inject(PatternUpdate(x)),
        ~read_only=false,
        model.pattern,
        ed_focus,
      )
    | Stepper(a) =>
      Stepper.get_cursor_info(
        ~globals,
        ~inject=x => inject(StepUpdate(x)),
        ~focus=a,
        model.step,
      )
    };

  let handle_key_event =
      (~focus: focus, ~event: Key.t, model: model): option(action) =>
    switch (focus, model) {
    | (Pattern(_), _) =>
      // Use standard keyboard handler for editor actions
      Keyboard.handle_key_event(event) |> Option.map(x => PatternUpdate(x))
    | (Stepper(a), _) =>
      Stepper.handle_key_event(~focus=a, ~event, model.step)
      |> Option.map(x => StepUpdate(x))
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
    let pattern_editor = {
      let statics = EditorManager.Model.get_statics(model.pattern);
      let common: Common.t = {
        settings: globals.settings.core,
        font_metrics: globals.font_metrics,
        secondary_icons: globals.settings.secondary_icons,
        color_highlights: globals.color_highlights,
        statics,
        dynamics: Dynamics.Map.empty,
      };
      Editor.View.view(
        ~common,
        ~mode=
          Editable({
            inject: x => inject(PatternUpdate(x)),
            take_focus: _ => take_focus(Pattern(Editor.Focus.here())),
            escape: _ => Ui_effect.Ignore,
            focus:
              switch (focus) {
              | Some(Pattern(f)) => Some(f)
              | _ => None
              },
          }),
        ~sort=Sort.Pat,
        model.pattern.editor,
      );
    };
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
