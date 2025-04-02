open Util;
open Haz3lcore;
open Web;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type stepper = {
    cached_settings: Calc.saved(CoreSettings.t),
    cached_elab: Calc.saved(Exp.t),
    cached_elab_subst: Calc.saved(Exp.t),
    ctx: Calc.saved(Ctx.t),
    root: step,
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and step = {
    // Calculated
    expr: Calc.saved(Exp.t),
    state: Calc.saved(EvaluatorState.t),
    editor: Calc.saved(CodeSelectable.Model.t), // Also Updated.
    // Updated
    step_kind,
    next_step: option(step),
    // Calculated
    hidden: Calc.saved(bool),
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and step_kind =
    | SingleStep(single_step)
    | InductionStep(InductionStep.Model.t(step))
    | ForallStep(forall_step)
    | MissingStep(MissingStep.Model.t)
    | AxiomStep(axiom_step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and single_step = {
    evalobj: EvaluatorStep.EvalObj.t,
    next_exp: Calc.saved(Exp.t),
    next_state: Calc.saved(EvaluatorState.t),
  }

  and axiom_step = {
    // Constant
    at_id: Id.t,
    at_exp: Exp.t,
    with_exp: Exp.t,
    // Calculated
    next_exp: Calc.saved(Exp.t),
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and forall_step = {
    // Calculated
    inner_exp: Calc.saved(Exp.t),
    bindings: Calc.saved(Ctx.t),
    inner_stepper: step,
  };

  let init_missing_step = MissingStep(MissingStep.Model.init);

  let init_step = {
    expr: Calc.Pending,
    state: Calc.Pending,
    editor: Calc.Pending,
    step_kind: init_missing_step,
    next_step: None,
    hidden: Calc.Pending,
  };

  let init_forall_step = {
    inner_exp: Calc.Pending,
    bindings: Calc.Pending,
    inner_stepper: init_step,
  };

  let init_stepper = {
    cached_settings: Calc.Pending,
    cached_elab: Calc.Pending,
    cached_elab_subst: Calc.Pending,
    ctx: Calc.Pending,
    root: init_step,
  };

  let get_state_stepper = (model: stepper): EvaluatorState.t => {
    let rec get_state_step = (step: step): EvaluatorState.t => {
      switch (step.next_step) {
      | None => step.state |> Calc.get_saved_exc(~print="get_state_step")
      | Some(next) => get_state_step(next)
      };
    };
    get_state_step(model.root);
  };

  let get_elaboration_stepper = (model: stepper): option(Exp.t) => {
    model.cached_elab |> Calc.saved_to_option;
  };
};

module Update = {
  open Updated;
  open Calc.Syntax;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type stepper =
    | RootAction(step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and step =
    | EditorAction(StepperEditor.Update.t)
    | NextStep(step)
    // | MissingStep(missing_step)
    | SingleStep(single_step)
    | InductionStep(InductionStep.Update.t(step))
    | ForallStep(forall_step)
    | MissingStep(MissingStep.Update.t)
    | RemoveStep
    | StepForward(int)
    | AddInduction
    | AddForall
    | AddAxiomStep(Exp.t, Exp.t)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and single_step = unit

  and forall_step =
    | InnerExp(step);

  let rec update_stepper =
          (~settings, action: stepper, model: Model.stepper)
          : Updated.t(Model.stepper) => {
    switch (action) {
    | RootAction(action) =>
      let* new_root = update_step(~settings, action, model.root);
      {
        ...model,
        root: new_root,
      };
    };
  }

  and update_step =
      (~settings, action: step, model: Model.step): Updated.t(Model.step) => {
    switch (action, model.step_kind, model.next_step) {
    | (EditorAction(ea), _, _) =>
      switch (model.editor) {
      | Calc.Pending => model |> return_quiet
      | Calc.Calculated(editor) =>
        let* new_editor = CodeSelectable.Update.update(~settings, ea, editor);
        {
          ...model,
          editor: Calc.Calculated(new_editor),
        };
      }
    | (NextStep(a), _, Some(ns)) =>
      let* new_next_step = update_step(~settings, a, ns);
      {
        ...model,
        next_step: Some(new_next_step),
      };
    | (NextStep(_), _, None) => model |> return_quiet
    | (SingleStep(a), SingleStep(m), _) =>
      let* step_kind = update_single_step(~settings, a, m);
      {
        ...model,
        step_kind,
      };
    | (SingleStep(_), _, _) => model |> return_quiet
    | (InductionStep(a), InductionStep(m), _) =>
      let* ind_st =
        InductionStep.Update.update(
          ~settings,
          ~init_step=Model.init_step,
          ~update_step,
          a,
          m,
        );
      {
        ...model,
        step_kind: InductionStep(ind_st),
      };
    | (InductionStep(_), _, _) => model |> return_quiet
    | (ForallStep(a), ForallStep(m), _) =>
      let* step_kind = update_forall_step(~settings, a, m);
      {
        ...model,
        step_kind,
      };
    | (ForallStep(_), _, _) => model |> return_quiet
    | (MissingStep(a), MissingStep(ms), _) =>
      let* ms = MissingStep.Update.update(~settings, a, ms);
      {
        ...model,
        step_kind: MissingStep(ms),
      };
    | (MissingStep(_), _, _) => model |> return_quiet
    | (RemoveStep, _, _) =>
      {
        ...model,
        step_kind: Model.init_missing_step,
      }
      |> return
    | (StepForward(idx), MissingStep(ms), _) =>
      let msns = MissingStep.Model.get_next_steps(ms);
      switch (List.nth_opt(msns, idx)) {
      | Some((_, evalobj)) =>
        {
          ...model,
          step_kind:
            Model.SingleStep({
              evalobj,
              next_exp: Calc.Pending,
              next_state: Calc.Pending,
            }),
        }
        |> return
      | None => model |> return_quiet
      };
    | (StepForward(_), _, _) => model |> return_quiet
    | (AddInduction, MissingStep(_), _) =>
      {
        ...model,
        step_kind: Model.InductionStep(InductionStep.Model.init),
      }
      |> return
    | (AddInduction, _, _) => model |> return_quiet
    | (AddForall, MissingStep(_), _) =>
      {
        ...model,
        step_kind: Model.ForallStep(Model.init_forall_step),
      }
      |> return
    | (AddForall, _, _) => model |> return_quiet
    | (AddAxiomStep(at_exp, with_exp), MissingStep(_), _) =>
      let at_id = Exp.rep_id(at_exp);
      {
        ...model,
        step_kind:
          Model.AxiomStep({
            at_id,
            at_exp,
            with_exp,
            next_exp: Calc.Pending,
          }),
      }
      |> return;
    | (AddAxiomStep(_, _), _, _) => model |> return_quiet
    };
  }

  and update_single_step =
      (~settings as _, action: single_step, _model: Model.single_step)
      : Updated.t(Model.step_kind) => {
    switch (action) {
    | () => assert(false)
    };
  }

  and update_forall_step =
      (~settings, action: forall_step, _model: Model.forall_step)
      : Updated.t(Model.step_kind) => {
    switch (action) {
    | InnerExp(a) =>
      let* new_inner_step = update_step(~settings, a, _model.inner_stepper);
      Model.ForallStep({
        ..._model,
        inner_stepper: new_inner_step,
      });
    };
  };

  let rec calculate_stepper =
          (
            ~settings,
            elab: Exp.t,
            {ctx, cached_settings, cached_elab, cached_elab_subst, root}: Model.stepper,
          )
          : Model.stepper => {
    let settings =
      cached_settings
      |> Calc.set(settings, ~eq=(a, b) => {
           CoreSettings.{
             ...a,
             evaluation: {
               ...a.evaluation,
               show_settings: true,
               stepper_history: true,
             },
           }
           == CoreSettings.{
                ...b,
                evaluation: {
                  ...b.evaluation,
                  show_settings: true,
                  stepper_history: true,
                },
              }
         });
    let elab = cached_elab |> Calc.set(~eq=Exp.fast_equal, elab);
    let ctx = ctx |> Calc.set(Builtins.ctx_init);
    let elab_subst =
      cached_elab_subst
      |> {
        open Calc.Syntax;
        let.calc elab = elab;
        Substitution.subst(Builtins.env_init, elab);
      };
    let state = Calc.OldValue(EvaluatorState.init);
    let root = calculate_step(~settings, ctx, elab_subst, state, root) |> fst;
    {
      cached_settings: settings |> Calc.save,
      cached_elab: elab |> Calc.save,
      cached_elab_subst: elab_subst |> Calc.save,
      ctx: ctx |> Calc.save,
      root,
    };
  }
  and calculate_step =
      (
        ~settings: Calc.t(CoreSettings.t),
        ctx: Calc.t(Ctx.t),
        expr: Calc.t(Exp.t),
        state: Calc.t(EvaluatorState.t),
        {expr: _, state: _, editor, step_kind, next_step, hidden}: Model.step,
      )
      : (Model.step, Calc.t(Exp.t)) => {
    let editor =
      editor
      |> {
        let.calc settings = settings
        and.calc expr = expr
        and.calc ctx = ctx;
        expr
        |> CodeWithStatics.Model.mk_from_exp(~settings)
        |> CodeSelectable.Update.calculate(
             ~settings,
             ~is_edited=true,
             ~ctx,
             ~dynamics=Dynamics.Map.empty,
             ~stitch=x =>
             x
           );
      };
    let (step_kind, hidden, next_expr_state) =
      calculate_step_kind(
        ~settings,
        ctx,
        expr,
        state,
        step_kind,
        hidden,
        editor,
      );
    let (next_step, last_expr) =
      switch (next_expr_state) {
      | Some((next_expr, next_state)) =>
        let next_step = Option.value(~default=Model.init_step, next_step);
        let (next_step, last_expr) =
          calculate_step(~settings, ctx, next_expr, next_state, next_step);
        (Some(next_step), last_expr);
      | None => (None, expr)
      };
    (
      Model.{
        expr: expr |> Calc.save,
        state: state |> Calc.save,
        editor: editor |> Calc.save,
        step_kind,
        next_step,
        hidden: hidden |> Calc.save,
      },
      last_expr,
    );
  }
  and calculate_step_kind =
      (
        ~settings: Calc.t(CoreSettings.t),
        ctx: Calc.t(Ctx.t),
        expr: Calc.t(Exp.t),
        state: Calc.t(EvaluatorState.t),
        step_kind: Model.step_kind,
        hidden: Calc.saved(bool),
        editor: Calc.t(CodeSelectable.Model.t),
      ) => {
    switch (step_kind) {
    | SingleStep(m) =>
      calculate_single_step(~settings, expr, ctx, state, m, hidden, editor)
    | InductionStep(m) =>
      InductionStep.Update.calculate(
        ~settings,
        ~calculate_step,
        ctx,
        expr,
        state,
        m,
        hidden,
      )
      |> (((a, b, c)) => (Model.InductionStep(a), b, c))
    | ForallStep(m) =>
      calculate_forall_step(~settings, ctx, expr, state, m, hidden, editor)
    | MissingStep(m) =>
      calculate_missing_step(~settings, expr, ctx, state, m, hidden, editor)
    | AxiomStep(m) =>
      calculate_axiom_step(~settings, expr, ctx, state, m, hidden, editor)
    };
  }

  and calculate_missing_step =
      (
        ~settings,
        exp,
        ctx,
        state,
        missing_step: MissingStep.Model.t,
        hidden,
        editor: Calc.t(CodeSelectable.Model.t),
      )
      : (
          Model.step_kind,
          Calc.t(bool),
          option((Calc.t(Exp.t), Calc.t(EvaluatorState.t))),
        ) => {
    let next_steps =
      missing_step.next_steps
      |> Calc.update(
           Calc.combine(settings, Calc.combine(exp, state)),
           ((settings, (exp, state))) => {
           EvaluatorStep.decompose(exp, state)
           |> List.map(
                EvaluatorStep.should_hide_eval_obj(
                  ~settings=settings.evaluation,
                ),
              )
         });
    let next_step_to_take =
      switch (next_steps) {
      | Calc.NewValue(next_steps) =>
        List.find_opt(
          fun
          | (FilterAction.Step, _) => false
          | (FilterAction.Eval, _) => true,
          next_steps,
        )
      | Calc.OldValue(_) => None
      };
    switch (next_step_to_take) {
    | Some((_, evalobj)) =>
      calculate_single_step(
        ~settings,
        exp |> Calc.make_new,
        ctx |> Calc.make_new,
        state |> Calc.make_new,
        Model.{
          evalobj,
          next_exp: Calc.Pending,
          next_state: Calc.Pending,
        },
        hidden,
        editor,
      )
    | None => (
        Model.MissingStep(
          MissingStep.Update.calculate(
            ~settings=settings |> Calc.get_value,
            exp,
            ctx,
            state,
            next_steps,
            missing_step,
            editor,
          ),
        ),
        Calc.set(false, hidden),
        None,
      )
    };
  }

  and calculate_single_step =
      (
        ~settings,
        exp,
        ctx,
        state,
        {evalobj, next_exp, next_state},
        hidden,
        editor,
      )
      : (
          Model.step_kind,
          Calc.t(bool),
          option((Calc.t(Exp.t), Calc.t(EvaluatorState.t))),
        ) =>
    {
      open OptUtil.Syntax;
      let* hidden_and_eo =
        Calc.pair_saved(hidden, Calculated(evalobj))
        |> Calc.map_saved(Option.some)
        |> {
          let.calc settings = settings
          and.calc exp = exp
          and.calc state = state;
          let next_steps =
            EvaluatorStep.decompose(exp, state)
            |> List.map(
                 EvaluatorStep.should_hide_eval_obj(
                   ~settings=settings.evaluation,
                 ),
               );
          let+ (filer_action, eo) =
            List.find_opt(
              ((_, eo: EvaluatorStep.EvalObj.t)) =>
                eo.d_loc.annotation == evalobj.d_loc.annotation,
              next_steps,
            );
          let hidden =
            switch (filer_action) {
            | FilterAction.Step => false
            | FilterAction.Eval => true
            };
          (hidden, eo);
        }
        |> Calc.to_option;
      let (hidden, evalobj) = Calc.to_pair(hidden_and_eo);
      let+ next_exp_and_state =
        Calc.pair_saved(next_exp, next_state)
        |> Calc.map_saved(Option.some)
        |> {
          let.calc evalobj = evalobj
          and.calc state = state;
          let state = ref(state);
          let+ next_expr =
            EvaluatorStep.take_step(state, evalobj.env, evalobj.d_loc);
          let next_state = state^;
          let next_expr =
            EvalCtx.compose(evalobj.ctx, next_expr) |> Typ.replace_temp_exp;
          (next_expr, next_state);
        }
        |> Calc.to_option;
      let (next_exp, next_state) = Calc.to_pair(next_exp_and_state);
      (
        Model.SingleStep({
          evalobj: evalobj |> Calc.get_value,
          next_exp: next_exp |> Calc.save,
          next_state: next_state |> Calc.save,
        }),
        hidden,
        Some((next_exp, next_state)),
      );
    }
    |> OptUtil.get(() =>
         calculate_missing_step(
           ~settings,
           exp |> Calc.make_new,
           ctx |> Calc.make_new,
           state |> Calc.make_new,
           MissingStep.Model.init,
           hidden,
           editor,
         )
       )

  and calculate_forall_step =
      (~settings, ctx, exp, state, m: Model.forall_step, hidden, editor) =>
    {
      open OptUtil.Syntax;
      let {inner_exp, bindings, inner_stepper}: Model.forall_step = m;
      let+ (bindings, inner_exp) =
        (bindings, inner_exp)
        |> Calc.saved_pair
        |> Calc.map_saved(Option.some)
        |> {
          let.calc exp = exp
          and.calc ctx = ctx;
          switch (exp |> Exp.term_of) {
          | Fun(p, d1, t, _) =>
            let t = OptUtil.get(() => Typ.fresh(Unknown(Internal)), t);
            let* bindings = TypeAssignment.dhpat_extend_ctx(p, t, ctx);
            Some((bindings, d1));
          | _ => None
          };
        }
        |> Calc.to_option
        |> Option.map(Calc.to_pair);
      let (inner_stepper, _) =
        calculate_step(~settings, bindings, inner_exp, state, inner_stepper);
      (
        Model.ForallStep({
          inner_exp: inner_exp |> Calc.save,
          bindings: bindings |> Calc.save,
          inner_stepper,
        }),
        hidden |> Calc.set(false),
        None,
      );
    }
    |> OptUtil.get(() => {
         calculate_missing_step(
           ~settings,
           exp |> Calc.make_new,
           ctx |> Calc.make_new,
           state |> Calc.make_new,
           MissingStep.Model.init,
           hidden,
           editor,
         )
       })

  and calculate_axiom_step = (~settings, exp, ctx, state, m, hidden, editor) =>
    {
      let {at_id, at_exp, with_exp, next_exp}: Model.axiom_step = m;
      open OptUtil.Syntax;
      let+ next_exp =
        next_exp
        |> Calc.map_saved(Option.some)
        |> {
          let.calc exp = exp;
          let* _ = ProofHacks.find_exp_id(at_id, exp);
          Some(ProofHacks.replace_exp_id(at_id, exp, with_exp));
        }
        |> Calc.to_option;
      (
        Model.AxiomStep({
          at_id,
          at_exp,
          with_exp,
          next_exp: next_exp |> Calc.save,
        }),
        hidden |> Calc.set(false),
        Some((next_exp, state)),
      );
    }
    |> OptUtil.get(() => {
         calculate_missing_step(
           ~settings,
           exp |> Calc.make_new,
           ctx |> Calc.make_new,
           state |> Calc.make_new,
           MissingStep.Model.init,
           hidden,
           editor,
         )
       });
};

module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type stepper = step

  [@deriving (show({with_path: false}), sexp, yojson)]
  and step =
    | Here(CodeSelectable.Selection.t)
    | Next(step)
    | InductionStep(InductionStep.Selection.t(step))
    | ForallStep(forall_step)
    | MissingStep(MissingStep.Selection.t)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and forall_step =
    | InnerExp(step);

  let rec get_cursor_info_stepper =
          (~selection: stepper, model: Model.stepper): cursor(Update.stepper) => {
    let+ ci = get_cursor_info_step(~selection, model.root);
    Update.RootAction(ci);
  }
  and get_cursor_info_step =
      (~selection: step, model: Model.step): cursor(Update.step) =>
    switch (selection, model.step_kind, model.next_step) {
    | (Here(a), _, _) =>
      let+ ci =
        StepperEditor.Selection.get_cursor_info(
          ~selection=a,
          model.editor |> Calc.get_saved_exc(~print="Step editor selection"),
        );
      Update.EditorAction(ci);
    | (Next(a), _, Some(next_step)) =>
      let+ ci = get_cursor_info_step(~selection=a, next_step);
      Update.NextStep(ci);
    | (InductionStep(a), Model.InductionStep(m), _) =>
      let+ ci =
        InductionStep.Selection.get_cursor_info(
          ~get_cursor_info_step,
          ~selection=a,
          ~model=m,
        );
      Update.InductionStep(ci);
    | (Next(_), _, None)
    | (InductionStep(_), _, _) => empty
    | (ForallStep(a), Model.ForallStep(m), _) =>
      get_cursor_info_forall_step(~selection=a, ~model=m)
    | (ForallStep(_), _, _) => empty
    | (MissingStep(a), Model.MissingStep(m), _) =>
      let+ ci = MissingStep.Selection.get_cursor_info(~selection=a, m);
      Update.MissingStep(ci);
    | (MissingStep(_), _, _) => empty
    }

  and get_cursor_info_forall_step =
      (~selection: forall_step, ~model: Model.forall_step) =>
    switch (selection) {
    | InnerExp(a) => get_cursor_info_step(~selection=a, model.inner_stepper)
    };

  let rec handle_key_event_stepper =
          (~selection: stepper, ~event, model: Model.stepper) =>
    handle_key_event_step(~selection, ~event, model.root)
    |> Option.map(x => Update.RootAction(x))
  and handle_key_event_step = (~selection: step, ~event, model: Model.step) =>
    switch (selection, model) {
    | (Here(a), {editor: Calc.Calculated(editor), _}) =>
      CodeSelectable.Selection.handle_key_event(~selection=a, editor, event)
      |> Option.map(x => Update.EditorAction(x))
    | (Here(_), {editor: Calc.Pending, _}) => None
    | (Next(a), {next_step: Some(next_step), _}) =>
      handle_key_event_step(~selection=a, ~event, next_step)
      |> Option.map(x => Update.NextStep(x))
    | (Next(_), {next_step: None, _}) => None
    | (InductionStep(a), {step_kind: Model.InductionStep(m), _}) =>
      InductionStep.Selection.handle_key_event(
        ~handle_key_event_step,
        ~selection=a,
        ~event,
        m,
      )
      |> Option.map(x => Update.InductionStep(x))
    | (InductionStep(_), _) => None
    | (ForallStep(a), {step_kind: Model.ForallStep(m), _}) =>
      handle_key_event_forall_step(~selection=a, ~event, ~model=m)
      |> Option.map(x => Update.ForallStep(x))
    | (ForallStep(_), _) => None
    | (MissingStep(a), {step_kind: Model.MissingStep(m), _}) =>
      MissingStep.Selection.handle_key_event(~selection=a, ~event, ~model=m)
      |> Option.map(x => Update.MissingStep(x))
    | (MissingStep(_), _) => None
    }

  and handle_key_event_forall_step =
      (~selection: forall_step, ~event, ~model: Model.forall_step) =>
    switch (selection) {
    | InnerExp(a) =>
      handle_key_event_step(~selection=a, ~event, model.inner_stepper)
      |> Option.map(x => Update.InnerExp(x))
    };
};

module View = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type event_stepper =
    | MakeActive(Selection.stepper)
    | HideStepper;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type event_step =
    | MakeActive(Selection.step)
    | HideStepper;

  let rec view_stepper =
          (
            ~globals: Globals.t,
            ~signal: event_stepper => Ui_effect.t(unit),
            ~inject: Update.stepper => Ui_effect.t(unit),
            ~selected: option(Selection.stepper),
            model: Model.stepper,
          ) => {
    let settings_modal =
      globals.settings.core.evaluation.show_settings
        ? SettingsModal.view(
            ~inject=u => globals.inject_global(Set(u)),
            globals.settings.core.evaluation,
          )
        : [];
    view_stepper'(
      ~globals,
      ~signal_hide_stepper=signal(HideStepper),
      ~signal_make_active=a => signal(MakeActive(a)),
      ~inject=u => inject(RootAction(u)),
      ~selected,
      model.root,
    )
    @ settings_modal;
  }

  and view_stepper' =
      (
        ~globals: Globals.t,
        ~signal_make_active: Selection.step => Ui_effect.t(unit),
        ~signal_hide_stepper: Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~selected: option(Selection.step),
        root_step,
      ) => {
    [
      Web.Node.div(
        ~attrs=[Attr.classes(["stepper", "cell-result"])],
        view_step(
          ~globals,
          ~signal_make_active,
          ~signal_hide_stepper,
          ~inject,
          ~selected,
          ~undo=None,
          root_step,
        ),
      ),
    ];
  }

  and view_step =
      (
        ~globals: Globals.t,
        ~signal_make_active: Selection.step => Ui_effect.t(unit),
        ~signal_hide_stepper: Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~selected: option(Selection.step),
        ~undo: option(Ui_effect.t(unit)),
        model: Model.step,
      ) => {
    let current_step =
      if (model.hidden == Calc.Calculated(true)
          && !globals.settings.core.evaluation.show_hidden_steps) {
        [];
      } else {
        let taken_steps =
          switch (model.step_kind) {
          | Model.SingleStep(m) => [m.evalobj.d_loc |> Exp.rep_id]
          | _ => []
          };
        let next_steps =
          switch (model.step_kind) {
          | Model.MissingStep(m) =>
            m.next_steps
            |> Calc.get_saved_exc(~print="Next Steps")
            |> List.map(((_, eo: EvaluatorStep.EvalObj.t)) =>
                 eo.d_loc |> Exp.rep_id
               )
          | _ => []
          };
        let editor =
          StepperEditor.View.view(
            ~globals,
            ~signal=
              fun
              | MakeActive => signal_make_active(Here())
              | TakeStep(int) => inject(StepForward(int)),
            ~inject=x => inject(EditorAction(x)),
            ~selected=
              switch (selected) {
              | Some(Here(_)) => true
              | _ => false
              },
            ~overlays=
              switch (model.step_kind) {
              | Model.MissingStep(m) =>
                MissingStep.View.view_overlay(
                  ~globals,
                  ~inject=x => inject(MissingStep(x)),
                  ~selected=
                    switch (selected) {
                    | Some(MissingStep(s)) => Some(s)
                    | _ => None
                    },
                  ~signal=
                    fun
                    | HideStepper => Ui_effect.Ignore
                    | MakeActive(s) => signal_make_active(MissingStep(s))
                    | AddForall => inject(AddForall)
                    | AddInduction => inject(AddInduction)
                    | AddAxiomStep(e1, e2) => inject(AddAxiomStep(e1, e2)),
                  ~editor=model.editor |> Calc.get_saved_exc(~print="Editor"),
                  m,
                )
              | _ => []
              },
            StepperEditor.Model.{
              editor: model.editor |> Calc.get_saved_exc(~print="Editor"),
              taken_steps,
              next_steps,
            },
          );
        let signal: event_step => Ui_effect.t(unit) =
          fun
          | MakeActive(s) => signal_make_active(s)
          | HideStepper => signal_hide_stepper;
        let justification =
          view_justification(
            ~globals: Globals.t,
            ~signal: event_step => Ui_effect.t(unit),
            ~inject: Update.step => Ui_effect.t(unit),
            ~undo,
            model.step_kind,
          );
        let step_content =
          view_step_content(
            ~globals,
            ~signal,
            ~inject,
            ~selected,
            model.step_kind,
          );
        [
          Web.div_c(
            "step-border",
            [
              Web.div_c(
                "step-display",
                [
                  div_c("equiv", [Node.text("≡")]),
                  div_c("step-output", [editor]),
                  justification,
                ],
              ),
            ]
            @ step_content,
          ),
        ];
      };
    let next_step =
      Option.map(
        view_step(
          ~globals,
          ~signal_make_active=a => signal_make_active(Next(a)),
          ~signal_hide_stepper,
          ~inject=x => inject(NextStep(x)),
          ~selected=
            switch (selected) {
            | Some(Next(s)) => Some(s)
            | _ => None
            },
          ~undo=
            if (model.hidden |> Calc.get_saved_exc(~print="hidden")) {
              undo;
            } else {
              Some(inject(RemoveStep));
            },
        ),
        model.next_step,
      )
      |> Option.value(~default=[]);
    current_step @ next_step;
  }

  and view_justification =
      (
        ~globals: Globals.t,
        ~signal: event_step => Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~undo: option(Ui_effect.t(unit)),
        step_kind: Model.step_kind,
      ) => {
    let justification =
      switch (step_kind) {
      | SingleStep(m) =>
        Node.text(m.evalobj.knd |> Transition.stepper_justification)
      | InductionStep(_) => Node.text("Induction Step")
      | ForallStep(_) => Node.text("Enter Function")
      | AxiomStep(_) => Node.text("Axiom Step")
      | MissingStep(ms) =>
        MissingStep.View.view_justification(
          ~globals,
          ~signal=
            fun
            | HideStepper => Ui_effect.Ignore
            | MakeActive(s) => signal(MakeActive(MissingStep(s)))
            | AddForall => inject(AddForall)
            | AddInduction => inject(AddInduction)
            | AddAxiomStep(e1, e2) => inject(AddAxiomStep(e1, e2)),
          ~undo,
          ms,
        )
      };
    div_c("step-justification", [justification]);
  }

  and view_step_content =
      (
        ~globals: Globals.t,
        ~signal: event_step => Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~selected: option(Selection.step),
        step_kind: Model.step_kind,
      ) => {
    switch (step_kind) {
    | SingleStep(_) => []
    | AxiomStep(_) => []
    | InductionStep(m) =>
      InductionStep.View.view(
        ~view_stepper',
        ~globals,
        ~signal=
          fun
          | MakeActive(s) => signal(MakeActive(InductionStep(s)))
          | HideStepper => signal(HideStepper),
        ~inject=x => inject(InductionStep(x)),
        ~selected=
          switch (selected) {
          | Some(InductionStep(s)) => Some(s)
          | _ => None
          },
        m,
      )
    | ForallStep(fs) =>
      view_step(
        ~globals,
        ~signal_make_active=
          a => signal(MakeActive(ForallStep(InnerExp(a)))),
        ~signal_hide_stepper=signal(HideStepper),
        ~inject=x => inject(ForallStep(InnerExp(x))),
        ~selected=
          switch (selected) {
          | Some(ForallStep(InnerExp(s))) => Some(s)
          | _ => None
          },
        ~undo=Some(inject(RemoveStep)),
        fs.inner_stepper,
      )
    | MissingStep(_) => []
    };
  };
};
