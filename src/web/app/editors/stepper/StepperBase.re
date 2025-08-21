open Util;
open Language;
open StepInterface;
open Calc.Syntax;
open OptUtil.Syntax;

/* Note[Matt]: I've defined the types outside the modules here,
   this is in case we ever want to parameterize the types in
   the future, it'll be easier to please the OCaml compiler. */

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_model =
  | SingleStep(SingleStep.model'(step_model))
  | InductionStep(InductionStep.model'(step_model))
  | ForallStep(ForallStep.model'(step_model))
  | MissingStep(MissingStep.Model.t)
  | AxiomStep(AxiomStep.model'(step_model))

and step_model = {
  // Calculated
  expr: Calc.saved(Exp.t),
  state: Calc.saved(EvaluatorState.t),
  editor: Calc.saved(CodeSelectable.Model.t), // Also Updated.
  // Updated
  step_kind: step_kind_model,
  next_step: option(step_model),
  // Calculated
  hidden: Calc.saved(bool),
};

let init_step = {
  expr: Calc.Pending,
  state: Calc.Pending,
  editor: Calc.Pending,
  step_kind: MissingStep(MissingStep.Model.init),
  next_step: None,
  hidden: Calc.Pending,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_step_kind =
  | SingleStep(SingleStep.persistent'(persistent_step))
  | InductionStep(InductionStep.persistent'(persistent_step))
  | ForallStep(ForallStep.persistent'(persistent_step))
  | MissingStep(MissingStep.Model.persistent)
  | AxiomStep(AxiomStep.persistent'(persistent_step))

and persistent_step = {
  step_kind: persistent_step_kind,
  next_step: option(persistent_step),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_action =
  | SingleStep(SingleStep.action'(step_action))
  | InductionStep(InductionStep.action'(step_action))
  | ForallStep(ForallStep.action'(step_action))
  | MissingStep(MissingStep.Update.t)
  | AxiomStep(AxiomStep.action'(step_action))

and step_action =
  | StepKindAction(step_kind_action)
  | EditorAction(CodeSelectable.Update.t)
  | NextStep(step_action)
  | RemoveStep
  | StepForward(int)
  | AddInduction(option(Exp.t))
  | AddForall
  | AddAxiomStep(string, int, Exp.t, Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_focus =
  | SingleStep(SingleStep.focus'(step_focus))
  | InductionStep(InductionStep.focus'(step_focus))
  | ForallStep(ForallStep.focus'(step_focus))
  | MissingStep(MissingStep.Selection.t)
  | AxiomStep(AxiomStep.focus'(step_focus))

and step_focus =
  | StepKindFocus(step_kind_focus)
  | Here(CodeSelectable.Selection.t)
  | Next(step_focus);

module rec StepKind: {
  include
    STEP with
      type model = step_kind_model and
      type persistent = persistent_step_kind and
      type action = step_kind_action and
      type focus = step_kind_focus;

  let is_missing_step: step_kind_model => bool;
} = {
  /* The StepKind code here is almost all dispatch to the
     individual step modules. */

  module SingleStep = SingleStep.F(Stepper);
  module InductionStep = InductionStep.F(Stepper);
  module ForallStep = ForallStep.F(Stepper);
  module MissingStep = MissingStep; // This could be functorized too.
  module AxiomStep = AxiomStep.F(Stepper);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = step_kind_model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = persistent_step_kind;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = step_kind_action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = step_kind_focus;

<<<<<<< HEAD
  and case = {
    // Updated
    pattern: EditorManager.Model.t,
    // Calculated
    elab_pattern: Calc.saved(Pat.t),
    inner_exp: Calc.saved(Exp.t),
    step,
    last_exp: Calc.saved(Exp.t),
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_step = {
    // Updated
    scrut: EditorManager.Model.t,
    cases: list(case),
    // Calculated
    elab_scrut: Calc.saved(Exp.t),
    scrut_ty: Calc.saved(Typ.t),
    result: Calc.saved(Exp.t),
    result_state: Calc.saved(EvaluatorState.t),
    induction_valid: Calc.saved(induction_valid),
    join_exp: Calc.saved(Exp.t),
  }

  [@deriving (show({with_path: false}), sexp, yojson)]
  and forall_step = {
    // Calculated
    inner_exp: Calc.saved(Exp.t),
    bindings: Calc.saved(Ctx.t),
    inner_stepper: step,
    result_function: Calc.saved(Exp.t),
=======
  let persist = (model: model): persistent => {
    switch (model) {
    | SingleStep(m) => SingleStep(SingleStep.persist(m))
    | InductionStep(m) => InductionStep(InductionStep.persist(m))
    | ForallStep(m) => ForallStep(ForallStep.persist(m))
    | MissingStep(m) => MissingStep(MissingStep.Model.persist(m))
    | AxiomStep(m) => AxiomStep(AxiomStep.persist(m))
    };
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
  };

  let unpersist = (p: persistent): model => {
    switch (p) {
    | SingleStep(m) => SingleStep(SingleStep.unpersist(m))
    | InductionStep(m) => InductionStep(InductionStep.unpersist(m))
    | ForallStep(m) => ForallStep(ForallStep.unpersist(m))
    | MissingStep(m) => MissingStep(MissingStep.Model.unpersist(m))
    | AxiomStep(m) => AxiomStep(AxiomStep.unpersist(m))
    };
  };

  let is_missing_step = (sk: step_kind_model): bool => {
    switch (sk) {
    | MissingStep(_) => true
    | _ => false
    };
  };

  let update = (~settings, action: action, model: model) => {
    Updated.(
      switch (action, model) {
      | (SingleStep(a), SingleStep(m)) =>
        let* s = SingleStep.update(~settings, a, m);
        (SingleStep(s): model);
      | (InductionStep(a), InductionStep(m)) =>
        let* s = InductionStep.update(~settings, a, m);
        (InductionStep(s): model);
      | (ForallStep(a), ForallStep(m)) =>
        let* s = ForallStep.update(~settings, a, m);
        (ForallStep(s): model);
      | (MissingStep(a), MissingStep(m)) =>
        let* s = MissingStep.Update.update(~settings, a, m);
        (MissingStep(s): model);
      | (AxiomStep(a), AxiomStep(m)) =>
        let* s = AxiomStep.update(~settings, a, m);
        (AxiomStep(s): model);
      | (
          SingleStep(_) | InductionStep(_) | ForallStep(_) | MissingStep(_) |
          AxiomStep(_),
          _,
        ) =>
        model |> Updated.return_quiet
      }
    );
  };

  let can_undo = (a: action): bool => {
    switch (a) {
    | SingleStep(action) => SingleStep.can_undo(action)
    | InductionStep(action) => InductionStep.can_undo(action)
    | ForallStep(action) => ForallStep.can_undo(action)
    | MissingStep(action) => MissingStep.Update.can_undo(action)
    | AxiomStep(action) => AxiomStep.can_undo(action)
    };
  };

  let rec calculate =
          (
            ~settings: Calc.t(CoreSettings.t),
            ~hidden: Calc.saved(bool),
            ~exp: Calc.t(Exp.t),
            ~ctx: Calc.t(Ctx.t),
            ~state: Calc.t(EvaluatorState.t),
            ~editor: Calc.t(CodeSelectable.Model.t),
            model: model,
          ) =>
    switch (model) {
    | SingleStep(m) =>
      let+ (m, h, e) =
        SingleStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~state,
          ~editor,
          m,
        );
      (SingleStep(m): model, h, e);
    | InductionStep(m) =>
      let+ (m, h, e) =
        InductionStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~state,
          ~editor,
          m,
        );
      (InductionStep(m): model, h, e);
    | ForallStep(m) =>
      let+ (m, h, e) =
        ForallStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~state,
          ~editor,
          m,
        );
      (ForallStep(m): model, h, e);
    | MissingStep(missing_step) =>
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
        calculate(
          ~settings,
          ~exp=exp |> Calc.make_new,
          ~ctx=ctx |> Calc.make_new,
          ~state=state |> Calc.make_new,
          SingleStep({
            persistent_evalobj: evalobj |> EvaluatorStep.persist,
            evalobj: Calc.Pending,
            next_exp: Calc.Pending,
            next_state: Calc.Pending,
          }): model,
          ~hidden,
          ~editor,
        )
      | None =>
        Some((
          MissingStep(
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
        ))
      };
    | AxiomStep(m) =>
      let+ (m, h, e) =
        AxiomStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~state,
          ~editor,
          m,
        );
      (AxiomStep(m): model, h, e);
    };

  let get_cursor_info = (~focus: focus, model: model) =>
    Cursor.(
      switch (focus, model) {
      | (SingleStep(focus), SingleStep(model)) =>
        let+ focus_info = SingleStep.get_cursor_info(~focus, model);
        (SingleStep(focus_info): action);
      | (InductionStep(focus), InductionStep(model)) =>
        let+ focus_info = InductionStep.get_cursor_info(~focus, model);
        (InductionStep(focus_info): action);
      | (ForallStep(focus), ForallStep(model)) =>
        let+ focus_info = ForallStep.get_cursor_info(~focus, model);
        (ForallStep(focus_info): action);
      | (MissingStep(selection), MissingStep(model)) =>
        let+ focus_info =
          MissingStep.Selection.get_cursor_info(~selection, model);
        (MissingStep(focus_info): action);
      | (AxiomStep(focus), AxiomStep(model)) =>
        let+ focus_info = AxiomStep.get_cursor_info(~focus, model);
        (AxiomStep(focus_info): action);
      | (
          SingleStep(_) | InductionStep(_) | ForallStep(_) | MissingStep(_) |
          AxiomStep(_),
          _,
        ) => Cursor.empty
      }
    );

  let handle_key_event =
      (~focus: focus, ~event: Key.t, model: model): option(action) =>
    switch (focus, model) {
    | (SingleStep(focus), SingleStep(model)) =>
      SingleStep.handle_key_event(~focus, ~event, model)
      |> Option.map((x): action => SingleStep(x))
    | (InductionStep(focus), InductionStep(model)) =>
      InductionStep.handle_key_event(~focus, ~event, model)
      |> Option.map((x): action => InductionStep(x))
    | (ForallStep(focus), ForallStep(model)) =>
      ForallStep.handle_key_event(~focus, ~event, model)
      |> Option.map((x): action => ForallStep(x))
    | (MissingStep(selection), MissingStep(model)) =>
      MissingStep.Selection.handle_key_event(~selection, ~event, ~model)
      |> Option.map((x): action => MissingStep(x))
    | (AxiomStep(focus), AxiomStep(model)) =>
      AxiomStep.handle_key_event(~focus, ~event, model)
      |> Option.map((x): action => AxiomStep(x))
    | (
        SingleStep(_) | InductionStep(_) | ForallStep(_) | MissingStep(_) |
        AxiomStep(_),
        _,
      ) =>
      None
    };

  let view_content =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~undo: option(Ui_effect.t(unit)),
        ~is_toplevel: bool,
        model: model,
      ) => {
    let f =
      switch (model) {
      | SingleStep(m) =>
        SingleStep.view_content(
          ~focus=
            switch (focus) {
            | Some(SingleStep(f)) => Some(f)
            | _ => None
            },
          ~inject=x => inject(SingleStep(x)),
          ~take_focus=x => take_focus(SingleStep(x)),
          m,
        )
      | InductionStep(m) =>
        InductionStep.view_content(
          ~focus=
            switch (focus) {
            | Some(InductionStep(f)) => Some(f)
            | _ => None
            },
          ~inject=x => inject(InductionStep(x)),
          ~take_focus=x => take_focus(InductionStep(x)),
          m,
        )
      | ForallStep(m) =>
        ForallStep.view_content(
          ~focus=
            switch (focus) {
            | Some(ForallStep(f)) => Some(f)
            | _ => None
            },
          ~inject=x => inject(ForallStep(x)),
          ~take_focus=x => take_focus(ForallStep(x)),
          m,
        )
      | MissingStep(_) => (
          (~globals as _, ~hide_stepper as _, ~undo as _, ~is_toplevel as _) =>
            []
        )
      | AxiomStep(m) =>
        AxiomStep.view_content(
          ~focus=
            switch (focus) {
            | Some(AxiomStep(f)) => Some(f)
            | _ => None
            },
          ~inject=x => inject(AxiomStep(x)),
          ~take_focus=x => take_focus(AxiomStep(x)),
          m,
        )
      };
    f(~globals, ~hide_stepper, ~undo, ~is_toplevel);
  };

  let view_justification =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~undo: option(Ui_effect.t(unit)),
        ~is_toplevel: bool,
        model: model,
      ) =>
    switch (model) {
    | SingleStep(m) =>
      SingleStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(SingleStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(SingleStep(x)),
        ~take_focus=x => take_focus(SingleStep(x)),
        ~hide_stepper,
        ~undo,
        ~is_toplevel,
        m,
      )
    | InductionStep(m) =>
      InductionStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(InductionStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(InductionStep(x)),
        ~take_focus=x => take_focus(InductionStep(x)),
        ~hide_stepper,
        ~undo,
        ~is_toplevel,
        m,
      )
    | ForallStep(m) =>
      ForallStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(ForallStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(ForallStep(x)),
        ~take_focus=x => take_focus(ForallStep(x)),
        ~hide_stepper,
        ~undo,
        ~is_toplevel,
        m,
      )
    | MissingStep(m) =>
      MissingStep.View.view_justification(
        ~globals,
        ~is_toplevel,
        ~hide_stepper,
        ~undo,
        m,
      )
    | AxiomStep(m) =>
      AxiomStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(AxiomStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(AxiomStep(x)),
        ~take_focus=x => take_focus(AxiomStep(x)),
        ~hide_stepper,
        ~undo,
        ~is_toplevel,
        m,
      )
    };
}

and Stepper: {
  include
    STEPPER with
      type model = step_model and
      type persistent = persistent_step and
      type action = step_action and
      type focus = step_focus;

  let get_state: step_model => EvaluatorState.t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = step_model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = persistent_step;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = step_action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = step_focus;

  let init = {
    expr: Calc.Pending,
    state: Calc.Pending,
    editor: Calc.Pending,
    step_kind: MissingStep(MissingStep.Model.init),
    next_step: None,
    hidden: Calc.Pending,
  };

<<<<<<< HEAD
  let init_induction_step = (~exp: option(Exp.t)=?, ()) => {
    let scrut =
      switch (exp) {
      | Some(e) => EditorManager.Model.mk_uncalculated(Exp(e))
      | None =>
        EditorManager.Model.mk_uncalculated(
          Exp(Language.Exp.fresh(EmptyHole)),
        )
      };
=======
  let rec persist = (model: model): persistent => {
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
    {
      step_kind: StepKind.persist(model.step_kind),
      next_step: model.next_step |> Option.map(persist),
    };
  };

  let rec unpersist = (p: persistent): model => {
    {
      expr: Calc.Pending,
      state: Calc.Pending,
      editor: Calc.Pending,
      step_kind: StepKind.unpersist(p.step_kind),
      next_step: p.next_step |> Option.map(unpersist),
      hidden: Calc.Pending,
    };
  };

<<<<<<< HEAD
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
    | MissingStep(MissingStep.Update.t)
    | RemoveStep
    | StepForward(int)
    | AddInduction(option(Exp.t))
    | AddForall
    | AddAxiomStep(Exp.t, Exp.t)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and single_step = unit

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_step =
    | ScrutUpdate(EditorManager.Update.t)
    | CasePatternUpdate(int, EditorManager.Update.t)
    | CaseStepperUpdate(int, step)
    | AddCase
    | RemoveCase(int)

  and forall_step =
    | InnerExp(step);

  let rec update_stepper =
          (~globals, action: stepper, model: Model.stepper)
          : Updated.t(Model.stepper) => {
    switch (action) {
    | RootAction(action) =>
      let* new_root = update_step(~globals, action, model.root);
      {
        ...model,
        root: new_root,
      };
=======
  let rec get_state = (model: model) =>
    switch (model.next_step) {
    | None => model.state |> Calc.get_saved_exc(~print="get_state_step")
    | Some(next) => get_state(next)
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
    };

<<<<<<< HEAD
  and update_step =
      (~globals, action: step, model: Model.step): Updated.t(Model.step) => {
    switch (action, model.step_kind, model.next_step) {
    | (EditorAction(ea), _, _) =>
      switch (model.editor) {
      | Calc.Pending => model |> return_quiet
      | Calc.Calculated(editor) =>
        let* new_editor =
          CodeSelectable.Update.update(
            ~globals,
            ~dynamics=Dynamics.Map.empty,
            ea,
            editor,
          );
=======
  let rec update =
          (~settings, action: step_action, model: step_model)
          : Updated.t(step_model) => {
    Updated.(
      switch (action, model.step_kind, model.next_step) {
      | (EditorAction(ea), _, _) =>
        switch (model.editor) {
        | Calc.Pending => model |> return_quiet
        | Calc.Calculated(editor) =>
          let* new_editor =
            CodeSelectable.Update.update(~settings, ea, editor);
          {
            ...model,
            editor: Calc.Calculated(new_editor),
          };
        }
      | (NextStep(a), _, Some(ns)) =>
        let* new_next_step = update(~settings, a, ns);
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
        {
          ...model,
          next_step: Some(new_next_step),
        };
<<<<<<< HEAD
      }
    | (NextStep(a), _, Some(ns)) =>
      let* new_next_step = update_step(~globals, a, ns);
      {
        ...model,
        next_step: Some(new_next_step),
      };
    | (NextStep(_), _, None) => model |> return_quiet
    | (SingleStep(a), SingleStep(m), _) =>
      let* step_kind = update_single_step(~globals, a, m);
      {
        ...model,
        step_kind,
      };
    | (SingleStep(_), _, _) => model |> return_quiet
    | (InductionStep(a), InductionStep(m), _) =>
      let* step_kind = update_induction_step(~globals, a, m);
      {
        ...model,
        step_kind,
      };
    | (InductionStep(_), _, _) => model |> return_quiet
    | (ForallStep(a), ForallStep(m), _) =>
      let* step_kind = update_forall_step(~globals, a, m);
      {
        ...model,
        step_kind,
      };
    | (ForallStep(_), _, _) => model |> return_quiet
    | (MissingStep(a), MissingStep(ms), _) =>
      let* ms = MissingStep.Update.update(~globals, a, ms);
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
=======
      | (NextStep(_), _, None) => model |> return_quiet
      | (RemoveStep, _, _) =>
        {
          ...model,
          step_kind: MissingStep(MissingStep.Model.init),
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
              SingleStep({
                persistent_evalobj: evalobj |> EvaluatorStep.persist,
                evalobj: Calc.Calculated(evalobj),
                next_exp: Calc.Pending,
                next_state: Calc.Pending,
              }),
          }
          |> return
        | None => model |> return_quiet
        };
      | (StepForward(_), _, _) => model |> return_quiet
      | (AddInduction(exp), MissingStep(_), _) =>
        {
          ...model,
          step_kind: InductionStep(InductionStep.init(~exp?, ())),
        }
        |> return
      | (AddInduction(_), _, _) => model |> return_quiet
      | (AddForall, MissingStep(_), _) =>
        {
          ...model,
          step_kind: ForallStep(ForallStep.init(init)),
        }
        |> return
      | (AddForall, _, _) => model |> return_quiet
      | (AddAxiomStep(name, at_idx, at_exp, with_exp), MissingStep(_), _) =>
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
        {
          ...model,
          step_kind:
            AxiomStep({
              name,
              at_idx,
              at_exp,
              with_exp,
              next_exp: Calc.Pending,
            }),
        }
        |> return
<<<<<<< HEAD
      | None => model |> return_quiet
      };
    | (StepForward(_), _, _) => model |> return_quiet
    | (AddInduction(exp), MissingStep(_), _) =>
      {
        ...model,
        step_kind: Model.InductionStep(Model.init_induction_step(~exp?, ())),
      }
      |> return
    | (AddInduction(_), _, _) => model |> return_quiet
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
      (~globals as _, action: single_step, _model: Model.single_step)
      : Updated.t(Model.step_kind) => {
    switch (action) {
    | () => assert(false)
    };
  }
  and update_induction_step =
      (~globals, action: induction_step, model: Model.induction_step)
      : Updated.t(Model.step_kind) => {
    switch (action) {
    | ScrutUpdate(a) =>
      let* new_scrut =
        EditorManager.Update.update(
          ~common=Globals.to_common_global(globals),
          ~dynamics=Language.Dynamics.Map.empty,
          a,
          model.scrut,
        );
      Model.InductionStep({
        ...model,
        scrut: new_scrut,
      });
    | CasePatternUpdate(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        let* new_pattern =
          EditorManager.Update.update(
            ~common=Globals.to_common_global(globals),
            ~dynamics=Language.Dynamics.Map.empty,
            a,
            case.pattern,
          );
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
        let* new_step = update_step(~globals, a, case.step);
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
          pattern:
            EditorManager.Model.mk_uncalculated(
              Exp(Language.Exp.fresh(EmptyHole)),
            ),
          elab_pattern: Calc.Pending,
          inner_exp: Calc.Pending,
          step: Model.init_step,
          last_exp: Calc.Pending,
=======
      | (AddAxiomStep(_, _, _, _), _, _) => model |> return_quiet
      | (StepKindAction(sk_action), _, _) =>
        let* new_step_kind =
          StepKind.update(~settings, sk_action, model.step_kind);
        {
          ...model,
          step_kind: new_step_kind,
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
        };
      }
<<<<<<< HEAD
    };
  }

  and update_forall_step =
      (~globals, action: forall_step, _model: Model.forall_step)
      : Updated.t(Model.step_kind) => {
    switch (action) {
    | InnerExp(a) =>
      let* new_inner_step = update_step(~globals, a, _model.inner_stepper);
      Model.ForallStep({
        ..._model,
        inner_stepper: new_inner_step,
      });
    };
=======
    );
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
  };

  let rec can_undo = (a: step_action): bool => {
    switch (a) {
    | EditorAction(action) => CodeSelectable.Update.can_undo(action)
    | NextStep(next) => can_undo(next)
    | RemoveStep => true
    | StepForward(_) => true
    | AddInduction(_) => true
    | AddForall => true
<<<<<<< HEAD
    | AddAxiomStep(_, _) => true
    };
  }

  and can_undo_induction_step = (a: induction_step): bool => {
    switch (a) {
    | ScrutUpdate(action) => EditorManager.Update.can_undo(action)
    | CasePatternUpdate(_, action) => EditorManager.Update.can_undo(action)
    | CaseStepperUpdate(_, step) => can_undo_step(step)
    | AddCase => true
    | RemoveCase(_) => true
    };
  }

  and can_undo_forall_step = (a: forall_step): bool => {
    switch (a) {
    | InnerExp(step) => can_undo_step(step)
=======
    | AddAxiomStep(_) => true
    | StepKindAction(action) => StepKind.can_undo(action)
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
    };
  };

  let rec calculate =
          (
<<<<<<< HEAD
            ~globals: Globals.t,
            elab: Exp.t,
            {ctx, cached_settings, cached_elab, cached_elab_subst, root}: Model.stepper,
          )
          : Model.stepper => {
    let settings =
      cached_settings
      |> Calc.set(globals.settings.core, ~eq=(a, b) => {
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
    let ctx = ctx |> Calc.const(() => Builtins.ctx_init(None));
    let elab_subst =
      cached_elab_subst
      |> {
        open Calc.Syntax;
        let.calc elab = elab;
        Substitution.subst(Builtins.env_init, elab);
      };
    let state = Calc.OldValue(EvaluatorState.init);
    let root =
      calculate_step(~globals, ~settings, ctx, elab_subst, state, root) |> fst;
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
        ~globals: Globals.t,
        ~settings: Calc.t(CoreSettings.t),
        ctx: Calc.t(Ctx.t),
        expr: Calc.t(Exp.t),
        state: Calc.t(EvaluatorState.t),
        {expr: _, state: _, editor, step_kind, next_step, hidden}: Model.step,
      )
      : (Model.step, Calc.t(Exp.t)) => {
=======
            ~settings: Calc.t(CoreSettings.t),
            ~ctx: Calc.t(Ctx.t),
            ~exp as expr: Calc.t(Exp.t),
            ~state: Calc.t(EvaluatorState.t),
            {expr: _, state: _, editor, step_kind, next_step, hidden}: step_model,
          )
          : (step_model, Calc.t(Exp.t)) => {
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
    let editor =
      editor
      |> {
        let.calc _ = settings
        and.calc expr = expr
        and.calc ctx = ctx;
        EditorManager.Update.init(
          ~common=Globals.to_common_global(globals),
          ~stitch=_ => expr,
          ~is_dynamic_term=true,
          ~inline=false,
          ~ctx,
          Exp(expr),
        );
      };
    let (step_kind, hidden, next_expr_state) =
<<<<<<< HEAD
      calculate_step_kind(
        ~globals,
=======
      StepKind.calculate(
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
        ~settings,
        ~ctx,
        ~exp=expr,
        ~state,
        ~hidden,
        ~editor,
        step_kind,
      )
      |> OptUtil.get(() =>
           MissingStep(MissingStep.Model.init)
           |> StepKind.calculate(
                ~settings,
                ~ctx,
                ~exp=expr,
                ~state,
                ~hidden,
                ~editor,
              )
           |> Option.get
         );
    let (next_step, last_expr) =
      switch (next_expr_state) {
      | Some((next_expr, next_state)) =>
        let next_step = Option.value(~default=init, next_step);
        let (next_step, last_expr) =
<<<<<<< HEAD
          calculate_step(
            ~globals,
            ~settings,
            ctx,
            next_expr,
            next_state,
=======
          calculate(
            ~settings,
            ~ctx,
            ~exp=next_expr,
            ~state=next_state,
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
            next_step,
          );
        (Some(next_step), last_expr);
      | None => (None, expr)
      };
    (
      {
        expr: expr |> Calc.save,
        state: state |> Calc.save,
        editor: editor |> Calc.save,
        step_kind,
        next_step,
        hidden: hidden |> Calc.save,
      },
      last_expr,
    );
<<<<<<< HEAD
  }
  and calculate_step_kind =
      (
        ~globals: Globals.t,
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
      calculate_single_step(
        ~globals,
        ~settings,
        expr,
        ctx,
        state,
        m,
        hidden,
        editor,
      )
    | InductionStep(m) =>
      calculate_induction_step(
        ~globals,
        ~settings,
        ctx,
        expr,
        state,
        m,
        hidden,
      )
    | ForallStep(m) =>
      calculate_forall_step(
        ~globals,
        ~settings,
        ctx,
        expr,
        state,
        m,
        hidden,
        editor,
      )
    | MissingStep(m) =>
      calculate_missing_step(
        ~globals,
        ~settings,
        expr,
        ctx,
        state,
        m,
        hidden,
        editor,
      )
    | AxiomStep(m) =>
      calculate_axiom_step(
        ~globals,
        ~settings,
        expr,
        ctx,
        state,
        m,
        hidden,
        editor,
      )
    };
  }

  and calculate_missing_step =
      (
        ~globals: Globals.t,
        ~settings: Calc.t(CoreSettings.t),
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
        ~globals,
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
            ~globals,
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
        ~globals,
        ~settings: Calc.t(CoreSettings.t),
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
           ~globals,
           ~settings,
           exp |> Calc.make_new,
           ctx |> Calc.make_new,
           state |> Calc.make_new,
           MissingStep.Model.init,
           hidden,
           editor,
         )
       )

  and calculate_induction_step =
      (~globals, ~settings, ctx, exp, state, m, hidden) => {
    let {
      scrut,
      cases,
      elab_scrut,
      scrut_ty,
      result: _,
      result_state: _,
      induction_valid: _,
      join_exp,
    }: Model.induction_step = m;
    let scrut =
      EditorManager.Update.calculate(
        ~is_dynamic_term=true,
        ~common=Globals.to_common_global(globals),
        ~ctx=Calc.get_value(ctx),
        ~dynamics=Dynamics.Map.empty,
        ~stitch=x => x,
        scrut,
      );
    let elab_scrut =
      Calc.set(
        ~eq=Exp.fast_equal,
        EditorManager.Model.get_statics(scrut).elaborated,
        elab_scrut,
      );
    let scrut_ty = {
      let self_ty =
        switch (
          Id.Map.find_opt(
            Exp.rep_id(EditorManager.Model.get_statics(scrut).elaborated),
            EditorManager.Model.get_statics(scrut).info_map,
          )
        ) {
        | Some(Info.InfoExp({ty, _})) => ty
        | _ => raise(Elaborator.MissingTypeInfo)
        };
      Calc.set(~eq=Typ.fast_equal, self_ty, scrut_ty);
    };
    let cases =
      List.map(
        (
          Model.{pattern, elab_pattern, inner_exp, step: stepper, last_exp: _},
        ) => {
          let pattern =
            EditorManager.Update.calculate(
              ~is_dynamic_term=true,
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
              pattern,
            );
          let elab_pattern =
            Calc.set(
              ~eq=Pat.fast_equal,
              EditorManager.Model.get_statics(pattern).elaborated
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
          let (stepper, last_exp) =
            calculate_step(
              ~globals, // TODO: this is a little ugly
              ~settings,
              ctx,
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
        scrut_ty: scrut_ty |> Calc.save,
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
      (
        ~globals,
        ~settings,
        ctx,
        exp,
        state,
        m: Model.forall_step,
        hidden,
        editor,
      ) =>
    {
      open OptUtil.Syntax;
      let {inner_exp, bindings, inner_stepper, result_function}: Model.forall_step = m;
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
            let* bindings = ProofHacks.dhpat_extend_ctx(p, t, ctx);
            Some((bindings, d1));
          | _ => None
          };
        }
        |> Calc.to_option
        |> Option.map(Calc.to_pair);
      let (inner_stepper, last) =
        calculate_step(
          ~globals,
          ~settings,
          bindings,
          inner_exp,
          state,
          inner_stepper,
        );
      let result_function =
        result_function
        |> {
          let.calc last = last
          and.calc exp = exp;
          switch (exp |> Exp.term_of) {
          | Fun(p, _, t, n) => DHExp.fresh(Fun(p, last, t, n))
          | _ =>
            DHExp.fresh(
              Fun(
                Pat.fresh(EmptyHole),
                last,
                Some(Typ.fresh(Unknown(Internal))),
                None,
              ),
            )
          };
        };
      (
        Model.ForallStep({
          inner_exp: inner_exp |> Calc.save,
          bindings: bindings |> Calc.save,
          inner_stepper,
          result_function: result_function |> Calc.save,
        }),
        hidden |> Calc.set(false),
        Some((result_function, state)),
      );
    }
    |> OptUtil.get(() => {
         calculate_missing_step(
           ~globals,
           ~settings,
           exp |> Calc.make_new,
           ctx |> Calc.make_new,
           state |> Calc.make_new,
           MissingStep.Model.init,
           hidden,
           editor,
         )
       })

  and calculate_axiom_step =
      (~globals, ~settings, exp, ctx, state, m, hidden, editor) =>
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
           ~globals,
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
    | InductionStep(induction_step)
    | ForallStep(forall_step)
    | MissingStep(MissingStep.Selection.t)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and induction_step =
    | Scrut(EditorManager.Focus.t)
    | CasePattern(int, CodeSelectable.Selection.t)
    | CaseStepper(int, step)

  [@deriving (show({with_path: false}), sexp, yojson)]
  and forall_step =
    | InnerExp(step);

  let rec get_cursor_info_stepper =
          (~globals, ~inject, ~selection: stepper, model: Model.stepper)
          : Cursor.t => {
    get_cursor_info_step(
      ~globals,
      ~inject=x => inject(Update.RootAction(x)),
      ~selection,
      model.root,
    );
  }
  and get_cursor_info_step =
      (~globals, ~inject, ~selection: step, model: Model.step): Cursor.t =>
    switch (selection, model.step_kind, model.next_step) {
    | (Here(a), _, _) =>
      StepperEditor.Selection.get_cursor_info(
        ~common=Globals.to_common_global(globals),
        ~inject=x => inject(Update.EditorAction(x)),
        ~dynamics=Dynamics.Map.empty,
        model.editor |> Calc.get_saved_exc(~print="Step editor selection"),
        a,
      )
    | (Next(a), _, Some(next_step)) =>
      get_cursor_info_step(
        ~globals,
        ~inject=x => inject(Update.NextStep(x)),
        ~selection=a,
        next_step,
      )
    | (InductionStep(a), Model.InductionStep(m), _) =>
      get_cursor_info_induction_step(
        ~globals,
        ~inject,
        ~selection=a,
        ~model=m,
      )
    | (Next(_), _, None)
    | (InductionStep(_), _, _) => empty
    | (ForallStep(a), Model.ForallStep(m), _) =>
      get_cursor_info_forall_step(~globals, ~inject, ~selection=a, ~model=m)
    | (ForallStep(_), _, _) => empty
    | (MissingStep(a), Model.MissingStep(m), _) =>
      MissingStep.Selection.get_cursor_info(
        ~globals,
        ~inject=x => inject(Update.MissingStep(x)),
        a,
        m,
      )
    | (MissingStep(_), _, _) => empty
    }
  and get_cursor_info_induction_step =
      (
        ~globals,
        ~inject,
        ~selection: induction_step,
        ~model: Model.induction_step,
      ) =>
    switch (selection) {
    | Scrut(a) =>
      EditorManager.Focus.get_cursor_info(
        ~common=Globals.to_common_global(globals),
        ~dynamics=Dynamics.Map.empty,
        ~inject=x => inject(Update.InductionStep(Update.ScrutUpdate(x))),
        ~read_only=false,
        model.scrut,
        a,
      )
    | CasePattern(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        EditorManager.Focus.get_cursor_info(
          ~common=Globals.to_common_global(globals),
          ~dynamics=Dynamics.Map.empty,
          ~inject=
            x =>
              inject(Update.InductionStep(Update.CasePatternUpdate(i, x))),
          ~read_only=false,
          case.pattern,
          a,
        )
      | None => empty
      }
    | CaseStepper(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        get_cursor_info_step(
          ~globals,
          ~inject=
            x =>
              inject(Update.InductionStep(Update.CaseStepperUpdate(i, x))),
          ~selection=a,
          case.step,
        )
      | None => empty
      }
    }

  and get_cursor_info_forall_step =
      (~globals, ~inject, ~selection: forall_step, ~model: Model.forall_step) =>
    switch (selection) {
    | InnerExp(a) =>
      get_cursor_info_step(
        ~globals,
        ~inject,
        ~selection=a,
        model.inner_stepper,
      )
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
=======
  };

  let rec get_cursor_info =
          (~focus: step_focus, model: step_model): Cursor.cursor(step_action) => {
    Cursor.(
      switch (focus, model.step_kind, model.next_step) {
      | (StepKindFocus(sk), skm, _) =>
        let+ ci = StepKind.get_cursor_info(~focus=sk, skm);
        StepKindAction(ci);
      | (Here(a), _, _) =>
        let+ ci =
          StepperEditor.Selection.get_cursor_info(
            ~selection=a,
            model.editor |> Calc.get_saved_exc(~print="Step editor selection"),
          );
        EditorAction(ci);
      | (Next(a), _, Some(next_step)) =>
        let+ ci = get_cursor_info(~focus=a, next_step);
        NextStep(ci);
      | (Next(_), _, None) => Cursor.empty
      }
    );
  };

  let rec handle_key_event =
          (~focus: step_focus, ~event: Key.t, model: step_model)
          : option(step_action) => {
    switch (focus, model.step_kind, model.next_step) {
    | (StepKindFocus(sk), skm, _) =>
      StepKind.handle_key_event(~focus=sk, ~event, skm)
      |> Option.map((x): step_action => StepKindAction(x))
    | (Here(a), _, _) =>
      StepperEditor.Selection.handle_key_event(
        ~selection=a,
        model.editor |> Calc.get_saved_exc(~print="Step editor selection"),
        event,
      )
      |> Option.map((x): step_action => EditorAction(x))
    | (Next(a), _, Some(next_step)) =>
      next_step
      |> handle_key_event(~focus=a, ~event)
      |> Option.map((x): step_action => NextStep(x))
    | _ => None
    };
  };

  let rec view_step =
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
          (
            ~globals: Globals.t,
            ~take_focus: step_focus => Ui_effect.t(unit),
            ~inject: step_action => Ui_effect.t(unit),
            ~hide_stepper: Ui_effect.t(unit),
            ~focus: option(step_focus),
            ~is_toplevel: bool=false,
            ~undo: option(Ui_effect.t(unit)),
            model: step_model,
          ) => {
    let is_last_step = StepKind.is_missing_step(model.step_kind);
    let is_skipped_step = model.hidden == Calc.Calculated(true);
    let showing_skiped_steps =
      globals.settings.core.evaluation.show_hidden_steps;
    let showing_history = globals.settings.core.evaluation.stepper_history;
    let this_step_shown =
      is_last_step
      || showing_history
      && (!is_skipped_step || showing_skiped_steps);
    let current_step =
      if (!this_step_shown) {
        [];
      } else {
        let taken_steps =
          switch (model.step_kind) {
          | SingleStep(m) => [
              m.evalobj
              |> Calc.get_saved_exc(~print="SingleStep")
              |> EvaluatorStep.get_step_id,
            ]
          | AxiomStep(m) => [m.at_exp |> Exp.rep_id]
          | _ => []
          };
        let next_steps =
          switch (model.step_kind) {
          | MissingStep(m) =>
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
        let refls =
          switch (model.step_kind) {
          | MissingStep(m) when globals.settings.core.evaluation.enable_proof =>
            m.refls
            |> Calc.get_saved_exc(~print="refls")
            |> List.map(Exp.rep_id)
          | _ => []
          };
        let editor =
          StepperEditor.View.view(
            ~globals,
            ~signal=
              fun
<<<<<<< HEAD
              | MakeActive(f) => signal(MakeActive(Here(f)))
              | TakeStep(int) => inject(StepForward(int)),
            ~inject=x => inject(EditorAction(x)),
            ~selected=
              switch (selected) {
              | Some(Here(f)) => Some(f)
              | _ => None
=======
              | MakeActive => take_focus(Here())
              | TakeStep(int) => inject(StepForward(int))
              | Refl(int) => {
                  let refl_exps =
                    switch (model.step_kind) {
                    | MissingStep(m) =>
                      m.refls |> Calc.get_saved_exc(~print="refls")
                    | _ => []
                    };
                  let from_exp = List.nth(refl_exps, int);
                  inject(
                    AddAxiomStep(
                      "reflexivity",
                      ProofHacks.exp_idx(
                        from_exp,
                        model.expr |> Calc.get_saved_exc(~print="expr"),
                      ),
                      from_exp,
                      Exp.fresh(Atom(Bool(true))),
                    ),
                  );
                },
            ~inject=x => inject(EditorAction(x)),
            ~selected=
              switch (focus) {
              | Some(Here(_)) => true
              | _ => false
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
              },
            ~overlays=
              switch (model.step_kind) {
              | MissingStep(m)
                  when globals.settings.core.evaluation.enable_proof =>
                MissingStep.View.view_overlay(
                  ~globals,
                  ~inject=x => inject(StepKindAction(MissingStep(x))),
                  ~selected=
                    switch (focus) {
                    | Some(StepKindFocus(MissingStep(s))) => Some(s)
                    | _ => None
                    },
                  ~signal=
                    fun
                    | HideStepper => hide_stepper
                    | MakeActive(s) =>
                      take_focus(StepKindFocus(MissingStep(s)))
                    | AddForall => inject(AddForall)
                    | AddInduction(exp) => inject(AddInduction(exp))
                    | AddAxiomStep(name, idx, e1, e2) =>
                      inject(AddAxiomStep(name, idx, e1, e2)),
                  ~editor=model.editor |> Calc.get_saved_exc(~print="Editor"),
                  m,
                )
              | _ => []
              },
            ~escape=_ => Ui_effect.Ignore,
            ~sort=Exp,
            StepperEditor.Model.{
              editor: model.editor |> Calc.get_saved_exc(~print="Editor"),
              taken_steps,
              next_steps,
              refls,
            },
          );
        let justification =
          StepKind.view_justification(
            ~globals: Globals.t,
            ~take_focus=f => take_focus(StepKindFocus(f)),
            ~hide_stepper,
            ~inject=a => inject(StepKindAction(a)),
            ~is_toplevel,
            ~focus=
              switch (focus) {
              | Some(StepKindFocus(sk)) => Some(sk)
              | _ => None
              },
            ~undo,
            model.step_kind,
          );
        let step_content =
          StepKind.view_content(
            ~globals,
            ~take_focus=f => take_focus(StepKindFocus(f)),
            ~hide_stepper,
            ~inject=a => inject(StepKindAction(a)),
            ~focus=
              switch (focus) {
              | Some(StepKindFocus(sk)) => Some(sk)
              | _ => None
              },
            ~is_toplevel,
            ~undo,
            model.step_kind,
          );
        WebUtil.[
          Node.div(
            ~attrs=
              [Attr.class_("step-border")]
              @ (is_skipped_step ? [Attr.class_("hidden")] : []),
            [
              WebUtil.div_c(
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
          ~is_toplevel,
          ~take_focus=f => take_focus(Next(f)),
          ~hide_stepper,
          ~inject=x => inject(NextStep(x)),
          ~focus=
            switch (focus) {
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
  };

  let view =
      (
        ~globals: Globals.t,
        ~take_focus: step_focus => Ui_effect.t(unit),
        ~inject: step_action => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~focus: option(step_focus),
        ~is_toplevel: bool,
        root_step,
      ) => {
    WebUtil.[
      Node.div(
        ~attrs=[Attr.classes(["stepper", "cell-result"])],
        view_step(
          ~globals,
          ~take_focus,
          ~hide_stepper,
          ~inject,
          ~focus,
          ~is_toplevel,
<<<<<<< HEAD
          ~signal=
            fun
            | HideStepper => signal(HideStepper)
            | MakeActive(s) => signal(MakeActive(MissingStep(s)))
            | AddForall => inject(AddForall)
            | AddInduction(exp) => inject(AddInduction(exp))
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
    | ForallStep(fs) =>
      view_step(
        ~globals,
        ~signal=
          fun
          | MakeActive(s) => signal(MakeActive(ForallStep(InnerExp(s))))
          | HideStepper => signal(HideStepper),
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
      Editor.View.view(
        ~common=
          Common.{
            settings: globals.settings.core,
            font_metrics: globals.font_metrics,
            secondary_icons: globals.settings.secondary_icons,
            color_highlights: globals.color_highlights,
            statics: CachedStatics.empty,
            dynamics: Dynamics.Map.empty,
          },
        ~mode=
          Editable({
            inject: x =>
              inject(Update.InductionStep(Update.ScrutUpdate(x))),
            escape: _ => Ui_effect.Ignore,
            take_focus: f => signal(MakeActive(InductionStep(Scrut(f)))),
            focus:
              switch (selected) {
              | Some(Scrut(f)) => Some(f)
              | _ => None
              },
          }),
        ~sort=Exp,
        model.scrut.editor,
      );

    let add_case_button =
      Widgets.button(
        Node.text("Case ..."),
        ~tooltip="Add case",
        ~clss=["subtle-button", "add-case-button"],
        _ =>
        inject(InductionStep(AddCase))
      );

    let cases =
      List.mapi(
        (i, Model.{pattern, step: stepper, _}) => {
          let remove_case_button =
            Widgets.button(
              Icons.trash,
              _ => inject(InductionStep(RemoveCase(i))),
              ~tooltip="Remove case",
              ~clss=["subtle-button"],
            );
          let pattern_editor =
            Editor.View.view(
              ~common=
                Common.{
                  settings: globals.settings.core,
                  font_metrics: globals.font_metrics,
                  secondary_icons: globals.settings.secondary_icons,
                  color_highlights: globals.color_highlights,
                  statics: CachedStatics.empty,
                  dynamics: Dynamics.Map.empty,
                },
              ~mode=
                Editable({
                  inject: x =>
                    inject(InductionStep(Update.CasePatternUpdate(i, x))),
                  escape: _ => Ui_effect.Ignore,
                  take_focus: f =>
                    signal(MakeActive(InductionStep(CasePattern(i, f)))),
                  focus:
                    switch (selected) {
                    | Some(CasePattern(j, f)) when i == j => Some(f)
                    | _ => None
                    },
                }),
              ~sort=Exp,
              pattern.editor,
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
                | HideStepper => signal(HideStepper),
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
                [
                  remove_case_button,
                  Node.text("Case "),
                  pattern_editor,
                  Node.text(" : "),
                ],
              ),
            ]
            @ stepper_view,
          );
        },
        model.cases,
      );

    [
      div_c(
        "induction-scrut",
        [
          Node.text("Cases on: "),
          div_c("inline-editor-wrapper", [scrut_editor]),
        ],
=======
          ~undo=None,
          root_step,
        ),
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
      ),
    ];
  };
};
