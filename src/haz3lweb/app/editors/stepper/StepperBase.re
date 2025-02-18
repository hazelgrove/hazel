open Util;
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
    | InductionStep(induction_step)
    | ForallStep(forall_step)
    | MissingStep(MissingStep.Model.t)
    | AxiomStep(axiom_step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and single_step = {
    evalobj: EvaluatorStep.step,
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
  and induction_valid =
    | Valid

  and case = {
    // Updated
    pattern: CodeEditable.Model.t,
    // Calculated
    elab_pattern: Calc.saved(Exp.t),
    inner_exp: Calc.saved(Exp.t),
    step,
    last_exp: Calc.saved(Exp.t),
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_step = {
    // Updated
    scrut: CodeEditable.Model.t,
    cases: list(case),
    // Calculated
    elab_scrut: Calc.saved(Exp.t),
    result: Calc.saved(Exp.t),
    result_state: Calc.saved(EvaluatorState.t),
    induction_valid: Calc.saved(induction_valid),
    join_exp: Calc.saved(Exp.t),
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and forall_step = {
    // Calculated
    inner_exp: Calc.saved(Exp.t),
    bindings: Calc.saved(list((string, Typ.t))),
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

  let init_induction_step = {
    scrut: CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
    cases: [],
    elab_scrut: Calc.Pending,
    result: Calc.Pending,
    result_state: Calc.Pending,
    induction_valid: Calc.Pending,
    join_exp: Calc.Pending,
  };

  let init_forall_step = {
    inner_exp: Calc.Pending,
    bindings: Calc.Pending,
  };

  let init_stepper = {
    cached_settings: Calc.Pending,
    cached_elab: Calc.Pending,
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
    | InductionStep(induction_step)
    | ForallStep(forall_step)
    | RemoveStep
    | StepForward(int)
    | AddInduction
    | AddForall
    | AddAxiom(Exp.t, Exp.t)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and single_step = unit

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_step =
    | ScrutUpdate(CodeEditable.Update.t)
    | CasePatternUpdate(int, CodeEditable.Update.t)
    | CaseStepperUpdate(int, step)
    | AddCase
    | RemoveCase(int)

  and forall_step = unit;

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
      let* step_kind = update_induction_step(~settings, a, m);
      {
        ...model,
        step_kind,
      };
    | (InductionStep(_), _, _) => model |> return_quiet
    | (ForallStep(a), ForallStep(m), _) =>
      let* step_kind = update_forall_step(~settings, a, m);
      {
        ...model,
        step_kind,
      };
    | (ForallStep(_), _, _) => model |> return_quiet
    | (RemoveStep, _, _) =>
      {
        ...model,
        step_kind: Model.init_missing_step,
      }
      |> return
    | (StepForward(idx), MissingStep(ms), _) =>
      let msns =
        ms.next_steps
        |> Calc.get_saved_exc(~print="StepForward")
        |> (
          fun
          | AutoStep(_) => []
          | AvailableSteps(msns) => msns
        );
      switch (List.nth_opt(msns, idx)) {
      | Some(evalobj) =>
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
        step_kind: Model.InductionStep(Model.init_induction_step),
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
    | (AddAxiom(at_exp, with_exp), MissingStep(_), _) =>
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
    | (AddAxiom(_, _), _, _) => model |> return_quiet
    };
  }

  and update_single_step =
      (~settings as _, action: single_step, _model: Model.single_step)
      : Updated.t(Model.step_kind) => {
    switch (action) {
    | () => assert(false)
    };
  }
  and update_induction_step =
      (~settings, action: induction_step, model: Model.induction_step)
      : Updated.t(Model.step_kind) => {
    switch (action) {
    | ScrutUpdate(a) =>
      let* new_scrut = CodeEditable.Update.update(~settings, a, model.scrut);
      Model.InductionStep({
        ...model,
        scrut: new_scrut,
      });
    | CasePatternUpdate(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        let* new_pattern =
          CodeEditable.Update.update(~settings, a, case.pattern);
        Model.InductionStep({
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
        });
      | None => Model.InductionStep(model) |> return_quiet
      }
    | CaseStepperUpdate(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        let* new_step = update_step(~settings, a, case.step);
        Model.InductionStep({
          ...model,
          cases:
            ListUtil.put_nth(
              i,
              {
                ...case,
                step: new_step,
              },
              model.cases,
            ),
        });
      | None => Model.InductionStep(model) |> return_quiet
      }
    | AddCase =>
      let new_case =
        Model.{
          pattern: CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
          elab_pattern: Calc.Pending,
          inner_exp: Calc.Pending,
          step: Model.init_step,
          last_exp: Calc.Pending,
        };
      Model.InductionStep({
        ...model,
        cases: model.cases @ [new_case],
      })
      |> return;
    | RemoveCase(i) =>
      switch (ListUtil.remove_nth(i, model.cases)) {
      | Some(new_cases) =>
        Model.InductionStep({
          ...model,
          cases: new_cases,
        })
        |> return
      | None => Model.InductionStep(model) |> return_quiet
      }
    };
  }

  and update_forall_step =
      (~settings as _, action: forall_step, _model: Model.forall_step)
      : Updated.t(Model.step_kind) => {
    switch (action) {
    | () => assert(false)
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
    let state = Calc.OldValue(EvaluatorState.init);
    let root = calculate_step(~settings, elab, state, root) |> fst;
    {
      cached_settings: settings |> Calc.save,
      cached_elab: elab |> Calc.save,
      root,
    };
  }
  and calculate_step =
      (
        ~settings: Calc.t(CoreSettings.t),
        expr: Calc.t(Exp.t),
        state: Calc.t(EvaluatorState.t),
        {expr: _, state: _, editor, step_kind, next_step, hidden}: Model.step,
      )
      : (Model.step, Calc.t(Exp.t)) => {
    let editor =
      editor
      |> {
        let.calc settings = settings
        and.calc expr = expr;
        expr
        |> CodeWithStatics.Model.mk_from_exp(~settings)
        |> CodeSelectable.Update.calculate(
             ~is_dynamic_term=true,
             ~settings,
             ~is_edited=true,
             ~dynamics=Dynamics.Map.empty,
             ~stitch=x =>
             x
           );
      };
    let (step_kind, hidden, next_expr_state) =
      calculate_step_kind(~settings, expr, state, step_kind, hidden, editor);
    let (next_step, last_expr) =
      switch (next_expr_state) {
      | Some((next_expr, next_state)) =>
        let next_step = Option.value(~default=Model.init_step, next_step);
        let (next_step, last_expr) =
          calculate_step(~settings, next_expr, next_state, next_step);
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
        expr: Calc.t(Exp.t),
        state: Calc.t(EvaluatorState.t),
        step_kind: Model.step_kind,
        hidden: Calc.saved(bool),
        editor: Calc.t(CodeSelectable.Model.t),
      ) => {
    switch (step_kind) {
    | SingleStep(m) =>
      calculate_single_step(~settings, expr, state, m, hidden, editor)
    | InductionStep(m) =>
      calculate_induction_step(~settings, expr, state, m, hidden)
    | ForallStep(m) =>
      calculate_forall_step(~settings, expr, state, m, hidden, editor)
    | MissingStep(m) =>
      calculate_missing_step(~settings, expr, state, m, hidden, editor)
    | AxiomStep(m) =>
      calculate_axiom_step(~settings, expr, state, m, hidden, editor)
    };
  }

  and calculate_missing_step =
      (
        ~settings,
        exp,
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
      calculate_single_step(
        ~settings,
        exp |> Calc.make_new,
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
            ~settings,
            exp,
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
      let+ next_exp_and_state =
        Calc.pair_saved(next_exp, next_state)
        |> Calc.map_saved(Option.some)
        |> {
          let.calc evalobj = evalobj;
          EvaluatorStep.take_step(evalobj);
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
           state |> Calc.make_new,
           MissingStep.Model.init,
           hidden,
           editor,
         )
       )

  and calculate_induction_step = (~settings, exp, state, m, hidden) => {
    let {
      scrut,
      cases,
      elab_scrut,
      result: _,
      result_state: _,
      induction_valid: _,
      join_exp,
    }: Model.induction_step = m;
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
        (
          Model.{pattern, elab_pattern, inner_exp, step: stepper, last_exp: _},
        ) => {
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
            calculate_step(
              ~settings, // TODO: this is a little ugly
              inner_exp,
              state,
              stepper,
            );
          Model.{
            pattern,
            elab_pattern: elab_pattern |> Calc.save,
            inner_exp: inner_exp |> Calc.save,
            step: stepper,
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
    let induction_valid = Calc.Pending; // TODO

    (
      Model.InductionStep({
        scrut,
        cases,
        elab_scrut: elab_scrut |> Calc.save,
        result,
        result_state,
        induction_valid,
        join_exp: join_exp |> Calc.save,
      }),
      hidden |> Calc.set(false),
      Some((join_exp, state)),
    );
  }

  and calculate_forall_step =
      (~settings, exp, state, m: Model.forall_step, hidden, editor) =>
    {
      open OptUtil.Syntax;
      let {inner_exp, bindings}: Model.forall_step = m;
      let+ inner_exp =
        inner_exp
        |> Calc.map_saved(Option.some)
        |> {
          let.calc exp = exp;
          switch (exp |> Exp.term_of) {
          | Fun(_, d1, _, _) => Some(d1)
          | _ => None
          };
        }
        |> Calc.to_option;
      (
        Model.ForallStep({
          inner_exp: inner_exp |> Calc.save,
          bindings,
        }),
        hidden |> Calc.set(false),
        Some((inner_exp, state)),
      );
    }
    |> OptUtil.get(() => {
         calculate_missing_step(
           ~settings,
           exp |> Calc.make_new,
           state |> Calc.make_new,
           MissingStep.Model.init,
           hidden,
           editor,
         )
       })

  and calculate_axiom_step = (~settings, exp, state, m, hidden, editor) =>
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
    | InductionStep(induction_step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_step =
    | Scrut(CodeEditable.Selection.t)
    | CasePattern(int, CodeSelectable.Selection.t)
    | CaseStepper(int, stepper);

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
      get_cursor_info_induction_step(~selection=a, ~model=m)
    | (Next(_), _, None)
    | (InductionStep(_), _, _) => empty
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
      | Some(case) =>
        let+ ci =
          CodeEditable.Selection.get_cursor_info(~selection=a, case.pattern);
        Update.InductionStep(CasePatternUpdate(i, ci));
      | None => empty
      }
    | CaseStepper(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        let+ ci = get_cursor_info_step(~selection=a, case.step);
        Update.InductionStep(CaseStepperUpdate(i, ci));
      | None => empty
      }
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
      handle_key_event_induction_step(~selection=a, ~event, ~model=m)
      |> Option.map(x => Update.InductionStep(x))
    | (InductionStep(_), _) => None
    }
  and handle_key_event_induction_step =
      (~selection: induction_step, ~event, ~model: Model.induction_step) =>
    switch (selection) {
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
        handle_key_event_step(~selection=a, ~event, case.step)
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
      ~signal=
        fun
        | MakeActive(s) => signal(MakeActive(s))
        | HideStepper => signal(HideStepper),
      ~inject=u => inject(RootAction(u)),
      ~selected,
      model.root,
    )
    @ settings_modal;
  }

  and view_stepper' =
      (
        ~globals: Globals.t,
        ~signal: event_step => Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~selected: option(Selection.step),
        root_step,
      ) => {
    [
      Web.Node.div(
        ~attrs=[Attr.classes(["stepper", "cell-result"])],
        view_step(
          ~globals,
          ~signal,
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
        ~signal: event_step => Ui_effect.t(unit),
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
          | Model.SingleStep(m) => [m.evalobj |> EvaluatorStep.get_step_id]
          | _ => []
          };
        let next_steps =
          switch (model.step_kind) {
          | Model.MissingStep(m) =>
            m.next_steps
            |> Calc.get_saved_exc(~print="next_steps")
            |> (
              fun
              | AutoStep(_) => []
              | AvailableSteps(steps) => steps
            )
            |> List.map(step => step |> EvaluatorStep.get_step_id)
          | _ => []
          };
        let editor =
          StepperEditor.View.view(
            ~globals,
            ~signal=
              fun
              | MakeActive => signal(MakeActive(Here()))
              | TakeStep(int) => inject(StepForward(int)),
            ~inject=x => inject(EditorAction(x)),
            ~selected=
              switch (selected) {
              | Some(Here(_)) => true
              | _ => false
              },
            StepperEditor.Model.{
              editor: model.editor |> Calc.get_saved_exc(~print="Editor"),
              taken_steps,
              next_steps,
            },
          );
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
          ~signal=
            fun
            | MakeActive(s) => signal(MakeActive(Next(s)))
            | HideStepper => signal(HideStepper),
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
        ~signal as _: event_step => Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~undo: option(Ui_effect.t(unit)),
        step_kind: Model.step_kind,
      ) => {
    let justification =
      switch (step_kind) {
      | SingleStep(m) =>
        Node.text(
          m.evalobj
          |> EvaluatorStep.get_step_kind
          |> Transition.stepper_justification,
        )
      | InductionStep(_) => Node.text("Induction Step")
      | ForallStep(_) => Node.text("Forall Step")
      | AxiomStep(_) => Node.text("Axiom Step")
      | MissingStep(ms) =>
        MissingStep.View.view_justification(
          ~globals,
          ~signal=
            fun
            | HideStepper => Ui_effect.Ignore
            | AddForall => inject(AddForall)
            | AddInduction => inject(AddInduction)
            | AddAxiom(e1, e2) => inject(AddAxiom(e1, e2)),
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
      view_induction_step(
        ~globals,
        ~signal,
        ~inject,
        ~selected=
          switch (selected) {
          | Some(InductionStep(s)) => Some(s)
          | _ => None
          },
        m,
      )
    | ForallStep(_) => []
    | MissingStep(ms) =>
      MissingStep.View.view_step_content(
        ~globals,
        ~signal=
          fun
          | HideStepper => Ui_effect.Ignore
          | AddForall => inject(AddForall)
          | AddInduction => inject(AddInduction)
          | AddAxiom(e1, e2) => inject(AddAxiom(e1, e2)),
        ms,
      )
    };
  }

  and view_induction_step =
      (
        ~globals: Globals.t,
        ~signal: event_step => Ui_effect.t(unit),
        ~inject: Update.step => Ui_effect.t(unit),
        ~selected: option(Selection.induction_step),
        model: Model.induction_step,
      ) => {
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
        (i, Model.{pattern, step: stepper, _}) => {
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
            view_stepper'(
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

    [
      Web.div_c(
        "induction-scrut",
        [
          Node.text("Cases on: "),
          Web.div_c("inline-editor-wrapper", [scrut_editor]),
        ],
      ),
    ]
    @ cases
    @ [add_case_button];
  };
};
