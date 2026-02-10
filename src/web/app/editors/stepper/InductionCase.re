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
  inner_ctx: Calc.saved(SemanticCtx.t),
  hypotheses: Calc.saved(list((Binding.t, Exp.t))),
  constraint_: Calc.saved(option(Coverage.Constraint.t)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent'('stepper) = {
  pattern: CodeEditable.Model.persistent,
  stepper: 'stepper,
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
  type persistent = persistent'(Stepper.persistent);
  type action = action'(Stepper.action);
  type focus = focus'(Stepper.focus);

  let init = {
    pattern: CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
    elab_pattern: Calc.Pending,
    inner_exp: Calc.Pending,
    step: Stepper.init,
    last_exp: Calc.Pending,
    inner_ctx: Calc.Pending,
    hypotheses: Calc.Pending,
    constraint_: Calc.Pending,
  };

  let persist = (model: model) => {
    {
      pattern: CodeEditable.Model.persist(model.pattern),
      stepper: Stepper.persist(model.step),
    };
  };

  let unpersist = (p: persistent) => {
    {
      pattern: CodeEditable.Model.unpersist(p.pattern),
      elab_pattern: Calc.Pending,
      inner_exp: Calc.Pending,
      step: Stepper.unpersist(p.stepper),
      last_exp: Calc.Pending,
      inner_ctx: Calc.Pending,
      hypotheses: Calc.Pending,
      constraint_: Calc.Pending,
    };
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
        ~scrut_co_ctx: Calc.t(CoCtx.t),
        ~scrut_ty: Calc.t(Typ.t),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~info_map: Calc.t(Statics.Map.t),
        ~ana: Calc.t(Typ.t),
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

    let (inner_ctx, inner_exp, hypotheses) =
      (model.inner_ctx, model.inner_exp, model.hypotheses)
      |> Calc.saved_3
      |> {
        open Calc.Syntax;
        let.calc elab_pattern = elab_pattern
        and.calc elab_scrut = elab_scrut
        and.calc scrut_co_ctx = scrut_co_ctx
        and.calc scrut_ty = scrut_ty
        and.calc sem_ctx = ctx
        and.calc info_map = info_map
        and.calc exp = exp;

        // 1. Find what variables the pattern adds to the scope, and
        // add them to the env and ctx.
        let added_variables =
          elab_pattern |> Pat.bindings |> Binding.variable_names;
        let sem_ctx =
          SemanticCtx.add_from_pattern(sem_ctx, elab_pattern, scrut_ty);

        // 2. Work out what the inner exp would be
        // Note: this is an option in case some capture nonsense happens.
        let inner_exp =
          ProofHacks.replace_exp(
            info_map,
            elab_scrut,
            scrut_co_ctx,
            elab_pattern |> ProofHacks.pat_to_exp,
            elab_pattern |> Pat.bindings |> CoCtx.of_bindings,
            added_variables,
            exp,
          );

        // 3. Create the case_equality assertion, and add to env and ctx if appropriate
        // Note: if the LHS of case_eq is in any way captured by the added variables, then we cannot use it.
        let is_case_eq_captured =
          CoCtx.has_any(scrut_co_ctx, added_variables);
        let case_eq =
          is_case_eq_captured
            ? None
            : Some(
                BinOp(
                  Poly(Equals),
                  elab_scrut,
                  elab_pattern |> ProofHacks.pat_to_exp,
                )
                |> Exp.fresh
                |> Substitution.in_exp(SemanticCtx.get_env(sem_ctx)),
              );
        let (sem_ctx, case_eq_name) =
          switch (case_eq) {
          | Some(case_eq) =>
            SemanticCtx.add_hypothesis(sem_ctx, "case_eq", case_eq)
            |> PairUtil.map_snd(Option.some)
          | None => (sem_ctx, None)
          };

        // 4. Find the inductive hypotheses, and add to env and ctx
        // Note: we do not add any IHs that are captured by the added variables. (This should happen iff the inner exp is captured)
        let inductive_hypotheses =
          ProofHacks.get_inductive_hypotheses(
            CodeEditable.Model.get_statics(pattern).info_map,
            scrut_ty,
            elab_pattern,
          )
          |> List.filter_map(h =>
               ProofHacks.replace_exp(
                 info_map,
                 elab_scrut,
                 scrut_co_ctx,
                 h |> ProofHacks.pat_to_exp,
                 h |> Pat.bindings |> CoCtx.of_bindings,
                 added_variables,
                 exp,
               )
             );
        let (sem_ctx, ihs) =
          List.fold_left(
            ((acc, ihs), h) =>
              SemanticCtx.add_hypothesis(acc, "ih", h)
              |> PairUtil.map_snd(x => [(x, h), ...ihs]),
            (sem_ctx, []),
            inductive_hypotheses,
          );

        let inner_exp =
          inner_exp |> Option.value(~default=Exp.fresh(EmptyHole));

        let case_eq_h =
          switch (case_eq, case_eq_name) {
          | (Some(e), Some(n)) => [(n, e)]
          | _ => []
          };
        let hypotheses = case_eq_h @ ihs;

        (sem_ctx, inner_exp, hypotheses);
      }
      |> Calc.to_3;

    let (stepper, last_exp, validity) =
      Stepper.calculate(
        ~settings, // TODO: this is a little ugly
        ~ctx=inner_ctx,
        ~exp=inner_exp,
        ~ana,
        model.step,
      );

    let constraint_ =
      {
        open OptUtil.Syntax;
        let statics = CodeWithStatics.Model.get_statics(pattern);
        let* info =
          Statics.Map.lookup(
            elab_pattern |> Calc.get_value |> Pat.rep_id,
            statics.info_map,
          );
        let* info_pat =
          switch (info) {
          | InfoPat(info_pat) => Some(info_pat)
          | _ => None
          };
        Some(Info.pat_constraint(info_pat));
      }
      |> Calc.set(_, model.constraint_);

    (
      {
        pattern,
        elab_pattern: elab_pattern |> Calc.save,
        inner_exp: inner_exp |> Calc.save,
        step: stepper,
        last_exp: last_exp |> Calc.save,
        inner_ctx: inner_ctx |> Calc.save,
        hypotheses: hypotheses |> Calc.save,
        constraint_: constraint_ |> Calc.save,
      },
      constraint_,
      validity,
    );
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
          | Some(Pattern ()) => Yes
          | _ => No
          },
        ~dynamics=Dynamics.Map.empty,
        model.pattern,
      );
    let pattern_editor =
      WebUtil.div_c("inline-editor-wrapper", [pattern_editor]);
    module StepperTargetBox = StepperTargetBox.F(Stepper);
    let stepper_view =
      StepperTargetBox.target_box(
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
        Exp.fresh(Atom(Bool(true))),
        model.last_exp |> Calc.get_saved_exc(~print="last_exp not calculated"),
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
        WebUtil.div_c(
          "induction-case-hypotheses",
          List.filter_map(
            fun
            | (Binding.{name: _, id: _}, exp) => {
                let rule = ProofRule.exp_to_rule(exp);
                let conclusion = ProofRule.conclusion_exp(rule);
                let code =
                  CodeViewable.view_any(
                    ~globals,
                    ~settings=
                      Haz3lcore.ExpToSegment.Settings.of_core(
                        ~inline=true,
                        ~fold_fn_bodies=`Text,
                        globals.settings.core,
                      ),
                    Exp(conclusion),
                  );
                Some(
                  WebUtil.div_c(
                    "induction-case-hypothesis",
                    [WebUtil.Node.text("assume "), code],
                  ),
                );
              },
            model.hypotheses
            |> Calc.get_saved_exc(~print="hypotheses not calculated"),
          ),
        ),
      ]
      @ stepper_view,
    );
  };
};
