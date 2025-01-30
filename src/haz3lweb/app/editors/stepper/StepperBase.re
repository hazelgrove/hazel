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
    | ForallStep(forall_step)

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

  and case = {
    // Updated
    pattern: CodeEditable.Model.t,
    // Calculated
    elab_pattern: Calc.saved(Exp.t),
    inner_exp: Calc.saved(Exp.t),
    stepper,
    last_exp: Calc.saved(Exp.t),
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_step = {
    common: step_common,
    // Updated
    scrut: CodeEditable.Model.t,
    cases: list(case),
    next_step: step,
    // Calculated
    elab_scrut: Calc.saved(Exp.t),
    result: Calc.saved(Exp.t),
    result_state: Calc.saved(EvaluatorState.t),
    induction_valid: Calc.saved(induction_valid),
    join_exp: Calc.saved(Exp.t),
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and forall_step = {
    common: step_common,
    // Calculated
    bindings: Calc.saved(list((string, Typ.t))),
    next_step: step,
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
    elab_scrut: Calc.Pending,
    result: Calc.Pending,
    result_state: Calc.Pending,
    induction_valid: Calc.Pending,
    join_exp: Calc.Pending,
  };

  let init_forall_step = {
    common: init_step_common,
    bindings: Calc.Pending,
    next_step: init_step,
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
      | ForallStep(m) => get_state_step(m.next_step)
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
    | ForallStep(forall_step)
    | StepForward(int)
    | AddInduction
    | AddForall
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
    | NextStepUpdate(step)

  and forall_step =
    | EditorAction(StepperEditor.Update.t)
    | NextStepUpdate(step);

  let stepper_undo = (model: Model.stepper): option(stepper) => {
    let rec step_undo = (step: Model.step): option(step) => {
      switch (step) {
      | MissingStep(_) => None
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
      | ForallStep(m) =>
        switch (step_undo(m.next_step)) {
        | Some(nsu) => Some(ForallStep(NextStepUpdate(nsu)))
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
      | MissingStep(_) => AddInduction
      | SingleStep(m) =>
        SingleStep(NextStepUpdate(step_add_induction(m.next_step)))
      | InductionStep(m) =>
        InductionStep(NextStepUpdate(step_add_induction(m.next_step)))
      | ForallStep(m) =>
        ForallStep(NextStepUpdate(step_add_induction(m.next_step)))
      };
    };
    RootAction(step_add_induction(model.root));
  };

  let stepper_add_forall = (model: Model.stepper): stepper => {
    let rec step_add_forall = (step: Model.step): step => {
      switch (step) {
      | MissingStep(_) => AddForall
      | SingleStep(m) =>
        SingleStep(NextStepUpdate(step_add_forall(m.next_step)))
      | InductionStep(m) =>
        InductionStep(NextStepUpdate(step_add_forall(m.next_step)))
      | ForallStep(m) =>
        ForallStep(NextStepUpdate(step_add_forall(m.next_step)))
      };
    };
    RootAction(step_add_forall(model.root));
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
    | (ForallStep(a), Model.ForallStep(m)) =>
      let* new_a = update_forall_step(~settings, a, m);
      Model.ForallStep(new_a);
    | (AddForall, Model.MissingStep(_)) =>
      Model.ForallStep(Model.init_forall_step) |> return
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
    | (ForallStep(_), _)
    | (StepForward(_), _)
    | (AddInduction, _)
    | (AddForall, _) => model |> return_quiet
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
      | Some(case) =>
        let* new_pattern =
          CodeEditable.Update.update(~settings, a, case.pattern);
        (
          {
            ...model,
            cases:
              ListUtil.put_nth(
                i,
                {
                  ...case,
                  pattern: new_pattern,
                },
                model.cases,
              ),
          }: Model.induction_step
        );
      | None => model |> return_quiet
      }
    | CaseStepperUpdate(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        let* new_stepper = update_stepper(~settings, a, case.stepper);
        (
          {
            ...model,
            cases:
              ListUtil.put_nth(
                i,
                {
                  ...case,
                  stepper: new_stepper,
                },
                model.cases,
              ),
          }: Model.induction_step
        );
      | None => model |> return_quiet
      }
    | AddCase =>
      let new_case =
        Model.{
          pattern: CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
          elab_pattern: Calc.Pending,
          inner_exp: Calc.Pending,
          stepper: Model.init_stepper,
          last_exp: Calc.Pending,
        };
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
  }

  and update_forall_step =
      (~settings, action: forall_step, model: Model.forall_step)
      : Updated.t(Model.forall_step) => {
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
          }: Model.forall_step
        );
      }
    | NextStepUpdate(a) =>
      let* new_next_step = update_step(~settings, a, model.next_step);
      ({...model, next_step: new_next_step}: Model.forall_step);
    };
  };

  let rec calculate_stepper =
          (
            ~settings,
            elab: Exp.t,
            {cached_settings, cached_elab, root}: Model.stepper,
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
    calculate_stepper'(
      ~settings,
      elab,
      {
        cached_settings,
        cached_elab,
        root,
      },
    )
    |> fst;
  }
  and calculate_stepper' =
      (
        ~settings: Calc.t(CoreSettings.t),
        elab: Calc.t(Exp.t),
        {cached_settings: _, cached_elab: _, root}: Model.stepper,
      ) => {
    let (root, last_expr) =
      calculate_step(
        ~settings,
        elab,
        Calc.OldValue(EvaluatorState.init),
        root,
      );
    (
      Model.{
        cached_settings: settings |> Calc.save,
        cached_elab: elab |> Calc.save,
        root,
      },
      last_expr,
    );
  }
  and calculate_step =
      (
        ~settings: Calc.t(CoreSettings.t),
        exp: Calc.t(Exp.t),
        state: Calc.t(EvaluatorState.t),
        step: Model.step,
      )
      : (Model.step, Calc.t(Exp.t)) => {
    switch (step) {
    | Model.MissingStep(m) => calculate_missing_step(~settings, exp, state, m)
    | Model.SingleStep(m) => calculate_single_step(~settings, exp, state, m)
    | Model.ForallStep(m) => calculate_forall_step(~settings, exp, state, m)
    | Model.InductionStep(m) =>
      let (new_step, last_expr) =
        calculate_induction_step(~settings, exp, state, m);
      (Model.InductionStep(new_step), last_expr);
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
      ) => {
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
      (
        Model.MissingStep({
          common,
          next_steps: next_steps |> Calc.save,
        }),
        exp,
      );
    };
  }
  and calculate_single_step =
      (
        ~settings,
        exp,
        state,
        {common, evalobj, hidden, next_exp, next_state, next_step},
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
      let (next_step, last_expr) =
        next_step |> calculate_step(~settings, next_exp, next_state);
      (
        Model.SingleStep({
          common,
          evalobj: evalobj |> Calc.get_value,
          hidden: hidden |> Calc.save,
          next_exp: next_exp |> Calc.save,
          next_state: next_state |> Calc.save,
          next_step,
        }),
        last_expr,
      );
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
      elab_scrut,
      result: _,
      result_state: _,
      induction_valid: _,
      join_exp,
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
    let elab_scrut =
      Calc.set(
        ~eq=Exp.fast_equal,
        CodeEditable.Model.get_statics(scrut).elaborated,
        elab_scrut,
      );
    let cases =
      List.map(
        (Model.{pattern, elab_pattern, inner_exp, stepper, last_exp: _}) => {
          let pattern =
            CodeEditable.Update.calculate(
              ~is_dynamic_term=true,
              ~settings=Calc.get_value(settings),
              ~dynamics=Dynamics.Map.empty,
              ~is_edited=true,
              ~stitch=x => x,
              pattern,
            );
          let elab_pattern =
            Calc.set(
              ~eq=Exp.fast_equal,
              CodeEditable.Model.get_statics(pattern).elaborated,
              elab_pattern,
            );
          let inner_exp =
            inner_exp
            |> {
              open Calc.Syntax;
              let.calc elab_pattern = elab_pattern
              and.calc elab_scrut = elab_scrut
              and.calc exp = exp;
              DHExp.replace_exp(elab_scrut, elab_pattern, exp);
            };
          let (stepper, last_exp) =
            calculate_stepper'(
              ~settings, // TODO: this is a little ugly
              inner_exp,
              stepper,
            );
          Model.{
            pattern,
            elab_pattern: elab_pattern |> Calc.save,
            inner_exp: inner_exp |> Calc.save,
            stepper,
            last_exp: last_exp |> Calc.save,
          };
        },
        cases,
      );

    let new_join_exp =
      List.fold_left(
        (acc, case: Model.case) =>
          switch (acc, case.last_exp) {
          | (None, Calc.Pending) => None
          | (None, Calc.Calculated(last_exp)) => Some(last_exp)
          | (Some(acc), Calc.Pending) => Some(acc)
          | (Some(acc), Calc.Calculated(last_exp))
              when Exp.fast_equal(acc, last_exp) =>
            Some(acc)
          | (Some(_), Calc.Calculated(_)) => Some(Exp.fresh(EmptyHole))
          },
        None,
        cases,
      );
    let join_exp =
      Calc.set(
        ~eq=Exp.fast_equal,
        new_join_exp |> Option.value(~default=Exp.fresh(EmptyHole)),
        join_exp,
      );

    let result = exp |> Calc.save;
    let result_state = state |> Calc.save;
    let induction_valid = Calc.Pending;

    let (next_step, last_exp) =
      calculate_step(~settings, join_exp, state, next_step);
    (
      Model.{
        common,
        scrut,
        cases,
        next_step,
        elab_scrut: elab_scrut |> Calc.save,
        result,
        result_state,
        induction_valid,
        join_exp: join_exp |> Calc.save,
      },
      last_exp,
    );
  }

  and calculate_forall_step =
      (~settings, exp, state, m: Model.forall_step)
      : (Model.step, Calc.t(Exp.t)) => {
    switch (exp |> Calc.get_value |> Exp.term_of) {
    | Fun(_, d1, _, _) =>
      let {common, bindings, next_step}: Model.forall_step = m;
      let common = calculate_step_common(~settings, exp, state, common);
      let (next_step, last_exp) =
        calculate_step(~settings, Calc.NewValue(d1), state, next_step);
      (ForallStep({common, bindings, next_step}), last_exp);
    | _ => Model.init_step |> calculate_step(~settings, exp, state)
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
    | ForallStep(forall_step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and missing_step = StepperEditor.Selection.t

  [@deriving (show({with_path: false}), sexp, yojson)]
  and single_step =
    | Here(CodeSelectable.Selection.t)
    | Next(step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and forall_step =
    | Here(CodeSelectable.Selection.t)
    | Next(step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_step =
    | Here(CodeSelectable.Selection.t)
    | Scrut(CodeEditable.Selection.t)
    | CasePattern(int, CodeSelectable.Selection.t)
    | CaseStepper(int, stepper)
    | Next(step);

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
    | (ForallStep(a), Model.ForallStep(m)) =>
      get_cursor_info_forall_step(~selection=a, ~model=m)
    | (MissingStep(_), _)
    | (SingleStep(_), _)
    | (InductionStep(_), _)
    | (ForallStep(_), _) => empty
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
  and get_cursor_info_forall_step =
      (~selection: forall_step, ~model: Model.forall_step) =>
    switch (selection, model.common.editor) {
    | (Here(a), Calc.Calculated(editor)) =>
      let+ ci =
        CodeSelectable.Selection.get_cursor_info(~selection=a, editor);
      Update.ForallStep(EditorAction(ci));
    | (Next(a), _) => get_cursor_info_step(~selection=a, model.next_step)
    | (Here(_), Calc.Pending) => empty
    }
  and get_cursor_info_induction_step =
      (~selection: induction_step, ~model: Model.induction_step) =>
    switch (selection) {
    | Here(a) =>
      switch (model.common.editor) {
      | Calc.Pending => empty
      | Calc.Calculated(editor) =>
        let+ ci =
          CodeSelectable.Selection.get_cursor_info(~selection=a, editor);
        Update.InductionStep(EditorAction(ci));
      }
    | Scrut(a) =>
      let+ ci =
        CodeEditable.Selection.get_cursor_info(~selection=a, model.scrut);
      Update.InductionStep(ScrutUpdate(ci));
    | CasePattern(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        let+ ci =
          CodeEditable.Selection.get_cursor_info(~selection=a, case.pattern);
        Update.InductionStep(CasePatternUpdate(i, ci));
      | None => empty
      }
    | CaseStepper(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        let+ ci = get_cursor_info_stepper(~selection=a, case.stepper);
        Update.InductionStep(CaseStepperUpdate(i, ci));
      | None => empty
      }
    | Next(a) =>
      let+ ci = get_cursor_info_step(~selection=a, model.next_step);
      Update.InductionStep(NextStepUpdate(ci));
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
    | (ForallStep(a), Model.ForallStep(m)) =>
      handle_key_event_forall_step(~selection=a, ~event, ~model=m)
      |> Option.map(x => Update.ForallStep(x))
    | (InductionStep(a), Model.InductionStep(m)) =>
      handle_key_event_induction_step(~selection=a, ~event, ~model=m)
      |> Option.map(x => Update.InductionStep(x))
    | (MissingStep(_), _)
    | (SingleStep(_), _)
    | (ForallStep(_), _)
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
  and handle_key_event_forall_step =
      (~selection: forall_step, ~event, ~model: Model.forall_step) =>
    switch (selection) {
    | Here(a) =>
      let* editor = model.common.editor |> Calc.saved_to_option;
      CodeSelectable.Selection.handle_key_event(~selection=a, editor, event)
      |> Option.map((x) => (Update.EditorAction(x): Update.forall_step));
    | Next(a) =>
      handle_key_event_step(~selection=a, ~event, model.next_step)
      |> Option.map((x) => (Update.NextStepUpdate(x): Update.forall_step))
    }
  and handle_key_event_induction_step =
      (~selection: induction_step, ~event, ~model: Model.induction_step) =>
    switch (selection) {
    | Here(a) =>
      let* editor = model.common.editor |> Calc.saved_to_option;
      CodeSelectable.Selection.handle_key_event(~selection=a, editor, event)
      |> Option.map((x): Update.induction_step => Update.EditorAction(x));
    | Scrut(a) =>
      let editor = model.scrut;
      CodeEditable.Selection.handle_key_event(~selection=a, editor, event)
      |> Option.map((x): Update.induction_step => Update.ScrutUpdate(x));
    | CasePattern(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        CodeEditable.Selection.handle_key_event(
          ~selection=a,
          case.pattern,
          event,
        )
        |> Option.map((x): Update.induction_step =>
             Update.CasePatternUpdate(i, x)
           )
      | None => None
      }
    | CaseStepper(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        handle_key_event_stepper(~selection=a, ~event, case.stepper)
        |> Option.map((x): Update.induction_step =>
             Update.CaseStepperUpdate(i, x)
           )
      | None => None
      }
    | Next(a) =>
      handle_key_event_step(~selection=a, ~event, model.next_step)
      |> Option.map((x): Update.induction_step => Update.NextStepUpdate(x))
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
    let button_forall =
      Widgets.button_d(
        Icons.star,
        inject(Update.stepper_add_forall(model)),
        ~disabled=false,
        ~tooltip="Prove a forall",
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
    let top_bar =
      Web.Node.div(
        ~attrs=[Attr.classes(["stepper-controls"])],
        [
          button_back,
          button_induction,
          button_forall,
          eval_settings,
          toggle_show_history,
          button_hide_stepper,
        ],
      );

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
    [
      Web.Node.div(
        ~attrs=[Attr.classes(["stepper", "cell-result"])],
        root_step,
      ),
    ]
    @ settings_modal;
  }

  and view_step =
      (
        ~globals: Globals.t,
        ~signal: event_step => Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~selected: option(Selection.step),
        ~top_bar: Node.t,
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
    | Model.ForallStep(m) =>
      view_forall_step(
        ~globals,
        ~signal,
        ~inject,
        ~selected=
          switch (selected) {
          | Some(ForallStep(s)) => Some(s)
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
        ~top_bar: Node.t,
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
      Web.div_c(
        "step-border",
        [
          Web.div_c(
            "step-display",
            [
              div_c("equiv", [Node.text("≡")]),
              div_c("step-output", [editor]),
              top_bar,
            ],
          ),
        ],
      ),
    ];
  }

  and view_single_step =
      (
        ~globals: Globals.t,
        ~signal: event_step => Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~selected: option(Selection.single_step),
        ~top_bar: Node.t,
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
      Web.div_c(
        "step-border",
        [
          Web.div_c(
            "step-display",
            [
              div_c("equiv", [Node.text("≡")]),
              div_c("step-output", [editor]),
              div_c(
                "step-justification",
                [
                  Node.text(
                    model.evalobj.knd |> Transition.stepper_justification,
                  ),
                ],
              ),
            ],
          ),
        ],
      ),
    ]
    @ next_step;
  }

  and view_forall_step =
      (
        ~globals: Globals.t,
        ~signal: event_step => Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~selected: option(Selection.forall_step),
        ~top_bar: Node.t,
        model: Model.forall_step,
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
          taken_steps: [
            model.common.expr
            |> Calc.get_saved_exc(~print="Exp")
            |> Exp.rep_id,
          ],
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
      Web.div_c(
        "step-border",
        [
          Web.div_c(
            "step-display",
            [
              div_c("equiv", [Node.text("≡")]),
              div_c("step-output", [editor]),
              div_c("step-justification", [Node.text("Forall Step")]),
            ],
          ),
        ],
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
        ~top_bar as _: Node.t,
        model: Model.induction_step,
      ) => {
    let output_editor =
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
          | _ => false
          },
        StepperEditor.Model.{
          editor: model.common.editor |> Calc.get_saved_exc(~print="Editor"),
          taken_steps: [],
          next_steps: [],
        },
      );

    let scrut_editor =
      CodeEditable.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => signal(MakeActive(InductionStep(Scrut()))),
        ~inject=x => inject(InductionStep(ScrutUpdate(x))),
        ~selected=
          switch (selected) {
          | Some(Scrut(_)) => true
          | Some(_)
          | None => false
          },
        model.scrut,
      );

    let add_case_button =
      Widgets.button(Icons.star, _ => inject(InductionStep(AddCase)));

    let cases =
      List.mapi(
        (i, Model.{pattern, stepper, _}) => {
          let remove_case_button =
            Widgets.button(Icons.star, _ =>
              inject(InductionStep(RemoveCase(i)))
            );
          let pattern_editor =
            CodeEditable.View.view(
              ~globals,
              ~signal=
                fun
                | MakeActive =>
                  signal(MakeActive(InductionStep(CasePattern(i, ())))),
              ~inject=x => inject(InductionStep(CasePatternUpdate(i, x))),
              ~selected=
                switch (selected) {
                | Some(CasePattern(j, _)) when i == j => true
                | Some(_)
                | None => false
                },
              pattern,
            );
          let pattern_editor =
            div_c("inline-editor-wrapper", [pattern_editor]);
          let stepper_view =
            view_stepper(
              ~globals,
              ~signal=
                fun
                | MakeActive(s) =>
                  signal(MakeActive(InductionStep(CaseStepper(i, s))))
                | HideStepper => Ui_effect.Ignore, // TODO: prevent hiding inner steppers
              ~inject=x => inject(InductionStep(CaseStepperUpdate(i, x))),
              ~selected=
                switch (selected) {
                | Some(CaseStepper(j, s)) when i == j => Some(s)
                | Some(_)
                | None => None
                },
              stepper,
            );
          div_c(
            "induction-case",
            [
              div_c(
                "induction-case-header",
                [remove_case_button, Node.text("Case "), pattern_editor],
              ),
            ]
            @ stepper_view,
          );
        },
        model.cases,
      );

    let next_step =
      view_step(
        ~globals,
        ~signal=
          fun
          | MakeActive(s) => signal(MakeActive(InductionStep(Next(s)))),
        ~inject=x => inject(InductionStep(NextStepUpdate(x))),
        ~selected=
          switch (selected) {
          | Some(Next(s)) => Some(s)
          | Some(_)
          | None => None
          },
        ~top_bar,
        model.next_step,
      );
    [
      Web.div_c(
        "step-border",
        [
          Web.div_c(
            "step-display",
            [
              div_c("equiv", [Node.text("≡")]),
              div_c("step-output", [output_editor]),
              div_c("step-justification", [Node.text("by cases")]),
            ],
          ),
        ]
        @ [
          Web.div_c(
            "induction-scrut",
            [
              Node.text("Cases on: "),
              Web.div_c("inline-editor-wrapper", [scrut_editor]),
            ],
          ),
        ]
        @ cases
        @ [add_case_button],
      ),
    ]
    @ next_step;
  };
};
