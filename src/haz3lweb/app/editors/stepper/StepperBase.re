open Util;
open Calc.Syntax;
open Haz3lcore;
open Web;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type stepper = {
    cached_settings: Calc.saved(CoreSettings.t),
    cached_elab: Calc.saved(Exp.t),
    root: step,
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and step =
    | MissingStep(missing_step)
    | SingleStep(single_step)
    | InductionStep(induction_step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and missing_step = {
    common: step_common,
    next_steps: Calc.saved(EvaluatorStep.status),
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and single_step = {
    common: step_common,
    evalobj: EvaluatorStep.step,
    hidden: Calc.saved(bool),
    next_exp: Calc.saved(Exp.t),
    next_state: Calc.saved(EvaluatorState.t),
    next_step: step,
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_valid =
    | Valid

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_step = {
    common: step_common,
    // Updated
    scrut: CodeEditable.Model.t,
    cases: list((CodeEditable.Model.t, stepper)),
    next_step: step,
    // Calculated
    result: Calc.saved(Exp.t),
    result_state: Calc.saved(EvaluatorState.t),
    induction_valid: Calc.saved(induction_valid),
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and step_common = {
    // Calculated
    expr: Calc.saved(Exp.t),
    state: Calc.saved(EvaluatorState.t),
    editor: Calc.saved(CodeSelectable.Model.t) // Also Updated.
  };

  let init_step_common = {
    expr: Calc.Pending,
    state: Calc.Pending,
    editor: Calc.Pending,
  };

  let init_step =
    MissingStep({
      common: init_step_common,
      next_steps: Calc.Pending,
    });

  let init_induction_step = {
    common: init_step_common,
    scrut: CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
    cases: [],
    next_step: init_step,
    result: Calc.Pending,
    result_state: Calc.Pending,
    induction_valid: Calc.Pending,
  };

  let init_stepper = {
    cached_settings: Calc.Pending,
    cached_elab: Calc.Pending,
    root: init_step,
  };

  let get_state_stepper = (model: stepper): EvaluatorState.t => {
    let rec get_state_step = (step: step): EvaluatorState.t => {
      switch (step) {
      | MissingStep(m) =>
        m.common.state |> Calc.get_saved_exc(~print="Evaluator State")
      | SingleStep(m) => get_state_step(m.next_step)
      | InductionStep(m) => get_state_step(m.next_step)
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

  [@deriving (show({with_path: false}), sexp, yojson)]
  type stepper =
    | RootAction(step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and step =
    | MissingStep(missing_step)
    | SingleStep(single_step)
    | InductionStep(induction_step)
    | StepForward(int)
    | AddInduction
    | RemoveStep

  [@deriving (show({with_path: false}), sexp, yojson)]
  and missing_step =
    | EditorAction(StepperEditor.Update.t)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and single_step =
    | EditorAction(StepperEditor.Update.t)
    | NextStepUpdate(step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_step =
    | EditorAction(StepperEditor.Update.t)
    | ScrutUpdate(CodeEditable.Update.t)
    | CasePatternUpdate(int, CodeEditable.Update.t)
    | CaseStepperUpdate(int, stepper)
    | AddCase
    | RemoveCase(int)
    | NextStepUpdate(step);

  let stepper_undo = (model: Model.stepper): option(stepper) => {
    let rec step_undo = (step: Model.step): option(step) => {
      switch (step) {
      | MissingStep(m) => None
      | SingleStep(m) when m.hidden == Calc.Calculated(true) => None
      | SingleStep(m) =>
        switch (step_undo(m.next_step)) {
        | Some(nsu) => Some(SingleStep(NextStepUpdate(nsu)))
        | None => Some(RemoveStep)
        }
      | InductionStep(m) =>
        switch (step_undo(m.next_step)) {
        | Some(nsu) => Some(InductionStep(NextStepUpdate(nsu)))
        | None => Some(RemoveStep)
        }
      };
    };
    step_undo(model.root) |> Option.map(x => RootAction(x));
  };

  let can_undo = (model: Model.stepper): bool => stepper_undo(model) != None;

  let stepper_add_induction = (model: Model.stepper): stepper => {
    let rec step_add_induction = (step: Model.step): step => {
      switch (step) {
      | MissingStep(m) => AddInduction
      | SingleStep(m) =>
        SingleStep(NextStepUpdate(step_add_induction(m.next_step)))
      | InductionStep(m) =>
        InductionStep(NextStepUpdate(step_add_induction(m.next_step)))
      };
    };
    RootAction(step_add_induction(model.root));
  };

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
    switch (action, model) {
    | (MissingStep(a), Model.MissingStep(m)) =>
      let* new_a = update_missing_step(~settings, a, m);
      Model.MissingStep(new_a);
    | (SingleStep(a), Model.SingleStep(m)) =>
      let* new_a = update_single_step(~settings, a, m);
      Model.SingleStep(new_a);
    | (InductionStep(a), Model.InductionStep(m)) =>
      let* new_a = update_induction_step(~settings, a, m);
      Model.InductionStep(new_a);
    | (StepForward(i), Model.MissingStep(m)) =>
      switch (
        m.next_steps |> Calc.get_saved(EvaluatorStep.AvailableSteps([]))
      ) {
      | AutoStep(next) =>
        Model.SingleStep(
          Model.{
            common: init_step_common,
            evalobj: next,
            hidden: Calc.Pending,
            next_exp: Calc.Pending,
            next_state: Calc.Pending,
            next_step: init_step,
          },
        )
        |> return
      | AvailableSteps(_) => model |> return_quiet
      }
    | (AddInduction, Model.MissingStep(_)) =>
      Model.InductionStep(Model.init_induction_step) |> return
    | (RemoveStep, _) => Model.init_step |> return
    | (MissingStep(_), _)
    | (SingleStep(_), _)
    | (InductionStep(_), _)
    | (StepForward(_), _)
    | (AddInduction, _) => model |> return_quiet
    };
  }

  and update_missing_step =
      (~settings, action: missing_step, model: Model.missing_step)
      : Updated.t(Model.missing_step) => {
    switch (action) {
    | EditorAction(a) =>
      switch (model.common.editor) {
      | Calc.Pending => model |> return_quiet
      | Calc.Calculated(editor) =>
        let* new_editor = CodeSelectable.Update.update(~settings, a, editor);
        (
          {
            common: {
              ...model.common,
              editor: Calc.Calculated(new_editor),
            },
            next_steps: model.next_steps,
          }: Model.missing_step
        );
      }
    };
  }

  and update_single_step =
      (~settings, action: single_step, model: Model.single_step)
      : Updated.t(Model.single_step) => {
    switch (action) {
    | EditorAction(a) =>
      switch (model.common.editor) {
      | Calc.Pending => model |> return_quiet
      | Calc.Calculated(editor) =>
        let* new_editor = CodeSelectable.Update.update(~settings, a, editor);
        (
          {
            ...model,
            common: {
              ...model.common,
              editor: Calc.Calculated(new_editor),
            },
          }: Model.single_step
        );
      }
    | NextStepUpdate(a) =>
      let* new_next_step = update_step(~settings, a, model.next_step);
      (
        {
          ...model,
          next_step: new_next_step,
        }: Model.single_step
      );
    };
  }

  and update_induction_step =
      (~settings, action: induction_step, model: Model.induction_step)
      : Updated.t(Model.induction_step) => {
    switch (action) {
    | EditorAction(a) =>
      switch (model.common.editor) {
      | Calc.Pending => model |> return_quiet
      | Calc.Calculated(editor) =>
        let* new_editor = CodeSelectable.Update.update(~settings, a, editor);
        (
          {
            ...model,
            common: {
              ...model.common,
              editor: Calc.Calculated(new_editor),
            },
          }: Model.induction_step
        );
      }
    | ScrutUpdate(a) =>
      let* new_scrut = CodeEditable.Update.update(~settings, a, model.scrut);
      (
        {
          ...model,
          scrut: new_scrut,
        }: Model.induction_step
      );
    | CasePatternUpdate(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some((pattern, stepper)) =>
        let* new_pattern = CodeEditable.Update.update(~settings, a, pattern);
        (
          {
            ...model,
            cases: ListUtil.put_nth(i, (new_pattern, stepper), model.cases),
          }: Model.induction_step
        );
      | None => model |> return_quiet
      }
    | CaseStepperUpdate(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some((pattern, stepper)) =>
        let* new_stepper = update_stepper(~settings, a, stepper);
        (
          {
            ...model,
            cases: ListUtil.put_nth(i, (pattern, new_stepper), model.cases),
          }: Model.induction_step
        );
      | None => model |> return_quiet
      }
    | AddCase =>
      let new_case = (
        CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
        Model.init_stepper,
      );
      (
        {
          ...model,
          cases: model.cases @ [new_case],
        }: Model.induction_step
      )
      |> return;
    | RemoveCase(i) =>
      switch (ListUtil.remove_nth(i, model.cases)) {
      | Some(new_cases) =>
        (
          {
            ...model,
            cases: new_cases,
          }: Model.induction_step
        )
        |> return
      | None => model |> return_quiet
      }
    | NextStepUpdate(a) =>
      let* new_next_step = update_step(~settings, a, model.next_step);
      (
        {
          ...model,
          next_step: new_next_step,
        }: Model.induction_step
      );
    };
  };

  let rec calculate_stepper =
          (
            ~settings,
            elab: Exp.t,
            {cached_settings, cached_elab, root}: Model.stepper,
          )
          : Model.stepper => {
    let _ = print_endline("Calculating Stepper");
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
    calculate_stepper'(
      ~settings,
      elab,
      {
        cached_settings,
        cached_elab,
        root,
      },
    );
  }
  and calculate_stepper' =
      (
        ~settings: Calc.t(CoreSettings.t),
        elab: Calc.t(Exp.t),
        {cached_settings: _, cached_elab: _, root}: Model.stepper,
      ) => {
    let root =
      calculate_step(
        ~settings,
        elab,
        Calc.OldValue(EvaluatorState.init),
        root,
      );
    Model.{
      cached_settings: settings |> Calc.save,
      cached_elab: elab |> Calc.save,
      root,
    };
  }
  and calculate_step =
      (
        ~settings: Calc.t(CoreSettings.t),
        exp: Calc.t(Exp.t),
        state: Calc.t(EvaluatorState.t),
        step: Model.step,
      )
      : Model.step => {
    switch (step) {
    | Model.MissingStep(m) => calculate_missing_step(~settings, exp, state, m)
    | Model.SingleStep(m) => calculate_single_step(~settings, exp, state, m)
    | Model.InductionStep(m) =>
      Model.InductionStep(calculate_induction_step(~settings, exp, state, m))
    };
  }
  and calculate_step_common =
      (
        ~settings,
        expr: Calc.t(Exp.t),
        state: Calc.t(EvaluatorState.t),
        {expr: _, state: _, editor}: Model.step_common,
      ) => {
    let editor =
      editor
      |> Calc.update(Calc.combine(settings, expr), ((settings, exp)) =>
           CodeWithStatics.Model.mk_from_exp(~settings, exp)
         )
      |> Calc.save;
    let expr = expr |> Calc.save;
    let state = state |> Calc.save;
    Model.{
      expr,
      state,
      editor,
    };
  }
  and calculate_missing_step =
      (
        ~settings,
        exp,
        state: Calc.t(EvaluatorState.t),
        {common, next_steps}: Model.missing_step,
      )
      : Model.step => {
    let next_steps =
      next_steps
      |> {
        let.calc settings = settings
        and.calc exp = exp
        and.calc state = state;
        EvaluatorStep.get_status(~settings, exp, state);
      };
    let next_step_to_take =
      Calc.Calculated(None)
      |> {
        let.calc next_steps: EvaluatorStep.status = next_steps;
        switch (next_steps) {
        | AutoStep(next) => Some(next)
        | AvailableSteps(_) => None
        };
      }
      |> Calc.get_value;
    switch (next_step_to_take) {
    | Some(evalobj) =>
      Model.SingleStep({
        common: Model.init_step_common,
        evalobj,
        hidden: Calc.Calculated(true),
        next_exp: Calc.Pending,
        next_state: Calc.Pending,
        next_step: Model.init_step,
      })
      |> calculate_step(
           ~settings,
           exp |> Calc.make_new,
           state |> Calc.make_new,
         )
    | None =>
      let common = calculate_step_common(~settings, exp, state, common);
      Model.MissingStep({
        common,
        next_steps: next_steps |> Calc.save,
      });
    };
  }
  and calculate_single_step =
      (
        ~settings,
        exp,
        state,
        {common, evalobj, hidden, next_exp, next_state, next_step},
      )
      : Model.step =>
    {
      open OptUtil.Syntax;
      let* hidden_and_eo =
        Calc.pair_saved(hidden, Calculated(evalobj))
        |> Calc.map_saved(Option.some)
        |> {
          let.calc settings = settings
          and.calc exp = exp
          and.calc state = state;
          let+ (filter_action, eo) =
            EvaluatorStep.refresh_step(~settings, exp, state, evalobj);
          let hidden =
            switch (filter_action) {
            | FilterAction.Step => false
            | FilterAction.Eval => true
            };
          (hidden, eo);
        }
        |> Calc.to_option;
      let (hidden, evalobj) = Calc.to_pair(hidden_and_eo);
      let common = calculate_step_common(~settings, exp, state, common);
      let+ next_exp_and_state =
        Calc.pair_saved(next_exp, next_state)
        |> Calc.map_saved(Option.some)
        |> {
          let.calc evalobj = evalobj;
          EvaluatorStep.take_step(evalobj);
        }
        |> Calc.to_option;
      let (next_exp, next_state) = Calc.to_pair(next_exp_and_state);
      let next_step =
        next_step |> calculate_step(~settings, next_exp, next_state);
      Model.SingleStep({
        common,
        evalobj: evalobj |> Calc.get_value,
        hidden: hidden |> Calc.save,
        next_exp: next_exp |> Calc.save,
        next_state: next_state |> Calc.save,
        next_step,
      });
    }
    |> OptUtil.get(() =>
         Model.init_step |> calculate_step(~settings, exp, state)
       )

  and calculate_induction_step = (~settings, exp, state, m) => {
    let {
      common,
      scrut,
      cases,
      next_step,
      result: _,
      result_state: _,
      induction_valid: _,
    }: Model.induction_step = m;
    let common = calculate_step_common(~settings, exp, state, common);
    let scrut =
      CodeEditable.Update.calculate(
        ~is_dynamic_term=true,
        ~settings=Calc.get_value(settings),
        ~dynamics=Dynamics.Map.empty,
        ~is_edited=true,
        ~stitch=x => x,
        scrut,
      );
    // TODO: complete
    let cases =
      List.map(
        ((pattern, stepper)) =>
          (
            CodeEditable.Update.calculate(
              ~is_dynamic_term=true,
              ~settings=Calc.get_value(settings),
              ~dynamics=Dynamics.Map.empty,
              ~is_edited=true,
              ~stitch=x => x,
              pattern,
            ),
            calculate_stepper'(
              ~settings, // TODO: this is a little ugly
              exp,
              stepper,
            ),
          ),
        cases,
      );

    let result = exp |> Calc.save;
    let result_state = state |> Calc.save;
    let induction_valid = Calc.Pending;

    let next_step = calculate_step(~settings, exp, state, next_step);
    Model.{
      common,
      scrut,
      cases,
      next_step,
      result,
      result_state,
      induction_valid,
    };
  };
};

module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type stepper = step

  [@deriving (show({with_path: false}), sexp, yojson)]
  and step =
    | MissingStep(missing_step)
    | SingleStep(single_step)
    | InductionStep(induction_step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and missing_step = StepperEditor.Selection.t

  [@deriving (show({with_path: false}), sexp, yojson)]
  and single_step =
    | Here(CodeSelectable.Selection.t)
    | Next(step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_step =
    | Scrut(CodeSelectable.Selection.t)
    | CasePattern(int, CodeSelectable.Selection.t)
    | CaseStepper(int, stepper);

  let rec get_cursor_info_stepper =
          (~selection: stepper, model: Model.stepper): cursor(Update.stepper) => {
    let+ ci = get_cursor_info_step(~selection, model.root);
    Update.RootAction(ci);
  }
  and get_cursor_info_step =
      (~selection: step, model: Model.step): cursor(Update.step) =>
    switch (selection, model) {
    | (MissingStep(a), Model.MissingStep(m)) =>
      get_cursor_info_missing_step(~selection=a, ~model=m)
    | (SingleStep(a), Model.SingleStep(m)) =>
      get_cursor_info_single_step(~selection=a, ~model=m)
    | (InductionStep(a), Model.InductionStep(m)) =>
      get_cursor_info_induction_step(~selection=a, ~model=m)
    | (MissingStep(_), _)
    | (SingleStep(_), _)
    | (InductionStep(_), _) => empty
    }
  and get_cursor_info_missing_step =
      (~selection: missing_step, ~model: Model.missing_step) =>
    switch (model.common.editor) {
    | Calc.Pending => empty
    | Calc.Calculated(editor) =>
      let+ ci = StepperEditor.Selection.get_cursor_info(~selection, editor);
      Update.MissingStep(EditorAction(ci));
    }
  and get_cursor_info_single_step =
      (~selection: single_step, ~model: Model.single_step) =>
    switch (selection, model.common.editor) {
    | (Here(a), Calc.Calculated(editor)) =>
      let+ ci =
        CodeSelectable.Selection.get_cursor_info(~selection=a, editor);
      Update.SingleStep(Update.EditorAction(ci));
    | (Next(a), _) => get_cursor_info_step(~selection=a, model.next_step)
    | (Here(_), Calc.Pending) => empty
    }
  and get_cursor_info_induction_step =
      (~selection: induction_step, ~model: Model.induction_step) =>
    switch (selection) {
    | Scrut(a) =>
      let+ ci =
        CodeEditable.Selection.get_cursor_info(~selection=a, model.scrut);
      Update.InductionStep(ScrutUpdate(ci));
    | CasePattern(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some((pattern, _)) =>
        let+ ci =
          CodeEditable.Selection.get_cursor_info(~selection=a, pattern);
        Update.InductionStep(CasePatternUpdate(i, ci));
      | None => empty
      }
    | CaseStepper(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some((_, stepper)) =>
        let+ ci = get_cursor_info_stepper(~selection=a, stepper);
        Update.InductionStep(CaseStepperUpdate(i, ci));
      | None => empty
      }
    };

  open OptUtil.Syntax;

  let rec handle_key_event_stepper =
          (~selection: stepper, ~event, model: Model.stepper) =>
    handle_key_event_step(~selection, ~event, model.root)
    |> Option.map(x => Update.RootAction(x))
  and handle_key_event_step = (~selection: step, ~event, model: Model.step) =>
    switch (selection, model) {
    | (MissingStep(a), Model.MissingStep(m)) =>
      handle_key_event_missing_step(~selection=a, ~event, ~model=m)
      |> Option.map(x => Update.MissingStep(x))
    | (SingleStep(a), Model.SingleStep(m)) =>
      handle_key_event_single_step(~selection=a, ~event, ~model=m)
      |> Option.map(x => Update.SingleStep(x))
    | (InductionStep(a), Model.InductionStep(m)) =>
      handle_key_event_induction_step(~selection=a, ~event, ~model=m)
      |> Option.map(x => Update.InductionStep(x))
    | (MissingStep(_), _)
    | (SingleStep(_), _)
    | (InductionStep(_), _) => None
    }
  and handle_key_event_missing_step =
      (~selection: missing_step, ~event, ~model: Model.missing_step) => {
    let* editor = model.common.editor |> Calc.saved_to_option;
    CodeSelectable.Selection.handle_key_event(~selection, editor, event)
    |> Option.map((x): Update.missing_step => Update.EditorAction(x));
  }
  and handle_key_event_single_step =
      (~selection: single_step, ~event, ~model: Model.single_step) =>
    switch (selection) {
    | Here(a) =>
      let* editor = model.common.editor |> Calc.saved_to_option;
      CodeSelectable.Selection.handle_key_event(~selection=a, editor, event)
      |> Option.map((x): Update.single_step => Update.EditorAction(x));
    | Next(a) =>
      handle_key_event_step(~selection=a, ~event, model.next_step)
      |> Option.map((x): Update.single_step => Update.NextStepUpdate(x))
    }
  and handle_key_event_induction_step =
      (~selection: induction_step, ~event, ~model: Model.induction_step) =>
    switch (selection) {
    | Scrut(a) =>
      let* editor = model.common.editor |> Calc.saved_to_option;
      CodeEditable.Selection.handle_key_event(~selection=a, editor, event)
      |> Option.map((x): Update.induction_step => Update.ScrutUpdate(x));
    | CasePattern(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some((pattern, _)) =>
        CodeEditable.Selection.handle_key_event(~selection=a, pattern, event)
        |> Option.map((x): Update.induction_step =>
             Update.CasePatternUpdate(i, x)
           )
      | None => None
      }
    | CaseStepper(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some((_, stepper)) =>
        handle_key_event_stepper(~selection=a, ~event, stepper)
        |> Option.map((x): Update.induction_step =>
             Update.CaseStepperUpdate(i, x)
           )
      | None => None
      }
    };
};

module View = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type event_stepper =
    | MakeActive(Selection.stepper)
    | HideStepper;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type event_step =
    | MakeActive(Selection.step);

  let rec view_stepper =
          (
            ~globals: Globals.t,
            ~signal: event_stepper => Ui_effect.t(unit),
            ~inject: Update.stepper => Ui_effect.t(unit),
            ~selected: option(Selection.stepper),
            model: Model.stepper,
          ) => {
    let button_back =
      Widgets.button_d(
        Icons.undo,
        switch (Update.stepper_undo(model)) {
        | Some(u) => inject(u)
        | None => Ui_effect.Ignore
        },
        ~disabled=!Update.can_undo(model),
        ~tooltip="Step Backwards",
      );
    let button_induction =
      Widgets.button_d(
        Icons.star,
        inject(Update.stepper_add_induction(model)),
        ~disabled=false,
        ~tooltip="Begin a proof by induction",
      );
    let button_hide_stepper =
      Widgets.toggle(~tooltip="Show Stepper", "s", true, _ =>
        signal(HideStepper)
      );
    let toggle_show_history =
      Widgets.toggle(
        ~tooltip="Show History",
        "h",
        globals.settings.core.evaluation.stepper_history,
        _ =>
        globals.inject_global(Set(Evaluation(ShowRecord)))
      );
    let eval_settings =
      Widgets.button(Icons.gear, _ =>
        globals.inject_global(Set(Evaluation(ShowSettings)))
      );
    let top_bar = [
      button_back,
      button_induction,
      eval_settings,
      toggle_show_history,
      button_hide_stepper,
    ];

    let root_step =
      view_step(
        ~globals,
        ~signal=
          fun
          | MakeActive(s) => signal(MakeActive(s)),
        ~inject=x => inject(RootAction(x)),
        ~selected,
        ~top_bar,
        model.root,
      );

    let settings_modal =
      globals.settings.core.evaluation.show_settings
        ? SettingsModal.view(
            ~inject=u => globals.inject_global(Set(u)),
            globals.settings.core.evaluation,
          )
        : [];

    root_step @ settings_modal;
  }

  and view_step =
      (
        ~globals: Globals.t,
        ~signal: event_step => Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~selected: option(Selection.step),
        ~top_bar: list(Node.t)=[],
        model: Model.step,
      ) =>
    switch (model) {
    | Model.MissingStep(m) =>
      view_missing_step(
        ~globals,
        ~signal,
        ~inject,
        ~selected=
          switch (selected) {
          | Some(MissingStep(s)) => Some(s)
          | _ => None
          },
        ~top_bar,
        m,
      )
    | Model.SingleStep(m) =>
      view_single_step(
        ~globals,
        ~signal,
        ~inject,
        ~selected=
          switch (selected) {
          | Some(SingleStep(s)) => Some(s)
          | _ => None
          },
        ~top_bar,
        m,
      )
    | Model.InductionStep(m) =>
      view_induction_step(
        ~globals,
        ~signal,
        ~inject,
        ~selected=
          switch (selected) {
          | Some(InductionStep(s)) => Some(s)
          | _ => None
          },
        ~top_bar,
        m,
      )
    }

  and view_missing_step =
      (
        ~globals: Globals.t,
        ~signal: event_step => Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~selected: option(Selection.missing_step),
        ~top_bar: list(Node.t),
        model: Model.missing_step,
      ) => {
    let editor =
      StepperEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => signal(MakeActive(MissingStep()))
          | TakeStep(int) => inject(Update.StepForward(int)),
        ~inject=x => inject(MissingStep(EditorAction(x))),
        ~selected=
          switch (selected) {
          | Some () => true
          | None => false
          },
        StepperEditor.Model.{
          editor:
            model.common.editor
            |> Calc.get_saved_exc(~print="Editor - Missing"),
          taken_steps: [],
          next_steps:
            switch (
              model.next_steps
              |> Calc.get_saved(EvaluatorStep.AvailableSteps([]))
            ) {
            | AutoStep(_) => []
            | AvailableSteps(steps) =>
              steps |> List.map(EvaluatorStep.get_step_id)
            },
        },
      );
    [
      Node.div(
        ~attrs=[Attr.classes(["cell-item", "cell-result"])],
        [div_c("equiv", [Node.text("≡")]), editor] @ top_bar,
      ),
    ];
  }

  and view_single_step =
      (
        ~globals: Globals.t,
        ~signal: event_step => Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~selected: option(Selection.single_step),
        ~top_bar: list(Node.t),
        model: Model.single_step,
      ) => {
    let editor =
      StepperEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => signal(MakeActive(SingleStep(Here())))
          | TakeStep(int) => inject(Update.StepForward(int)),
        ~inject=x => inject(SingleStep(EditorAction(x))),
        ~selected=
          switch (selected) {
          | Some(Here(_)) => true
          | Some(Next(_))
          | None => false
          },
        StepperEditor.Model.{
          editor: model.common.editor |> Calc.get_saved_exc(~print="Editor"),
          taken_steps: [model.evalobj |> EvaluatorStep.get_step_id],
          next_steps: [],
        },
      );
    let next_step =
      view_step(
        ~globals,
        ~signal=
          fun
          | MakeActive(s) => signal(MakeActive(SingleStep(Next(s)))),
        ~inject=x => inject(SingleStep(NextStepUpdate(x))),
        ~selected=
          switch (selected) {
          | Some(Next(s)) => Some(s)
          | Some(Here(_))
          | None => None
          },
        ~top_bar,
        model.next_step,
      );
    [
      Node.div(
        ~attrs=[Attr.classes(["cell-item", "cell-result"])],
        [div_c("equiv", [Node.text("≡")]), editor],
      ),
    ]
    @ next_step;
  }

  and view_induction_step =
      (
        ~globals as _: Globals.t,
        ~signal as _: event_step => Ui_effect.t(unit),
        ~inject as _: Update.step => Ui_effect.t(unit),
        ~selected as _: option(Selection.induction_step),
        ~top_bar as _: list(Node.t),
        _model: Model.induction_step,
      ) => {
    [Node.text("Induction Step")];
  };
};
