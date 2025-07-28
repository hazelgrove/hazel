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
  | CasesStep(CasesStep.model'(step_model))
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
type step_kind_action =
  | SingleStep(SingleStep.action'(step_action))
  | CasesStep(CasesStep.action'(step_action))
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
  | AddCases(option(Exp.t))
  | AddInduction(option(Exp.t))
  | AddForall
  | AddAxiomStep(string, Exp.t, Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_focus =
  | SingleStep(SingleStep.focus'(step_focus))
  | CasesStep(CasesStep.focus'(step_focus))
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
      type action = step_kind_action and
      type focus = step_kind_focus;

  let is_missing_step: step_kind_model => bool;
} = {
  /* The StepKind code here is almost all dispatch to the
     individual step modules. */

  module SingleStep = SingleStep.F(Stepper);
  module CasesStep = CasesStep.F(Stepper);
  module InductionStep = InductionStep.F(Stepper);
  module ForallStep = ForallStep.F(Stepper);
  module MissingStep = MissingStep; // This could be functorized too.
  module AxiomStep = AxiomStep.F(Stepper);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = step_kind_model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = step_kind_action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = step_kind_focus;

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
      | (CasesStep(a), CasesStep(m)) =>
        let* s = CasesStep.update(~settings, a, m);
        (CasesStep(s): model);
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
          SingleStep(_) | CasesStep(_) | InductionStep(_) | ForallStep(_) |
          MissingStep(_) |
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
    | CasesStep(action) => CasesStep.can_undo(action)
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
            ~env: Calc.t(ClosureEnvironment.t),
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
          ~env,
          ~state,
          ~editor,
          m,
        );
      (SingleStep(m): model, h, e);
    | CasesStep(m) =>
      let+ (m, h, e) =
        CasesStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~env,
          ~state,
          ~editor,
          m,
        );
      (CasesStep(m): model, h, e);
    | InductionStep(m) =>
      let+ (m, h, e) =
        InductionStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~env,
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
          ~env,
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
          and.calc env = env
          and.calc state = state;
          EvaluatorStep.get_status(~settings, exp, env, state);
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
          ~env=env |> Calc.make_new,
          ~state=state |> Calc.make_new,
          SingleStep({
            evalobj,
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
          ~env,
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
      | (CasesStep(focus), CasesStep(model)) =>
        let+ focus_info = CasesStep.get_cursor_info(~focus, model);
        (CasesStep(focus_info): action);
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
          SingleStep(_) | CasesStep(_) | InductionStep(_) | ForallStep(_) |
          MissingStep(_) |
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
    | (CasesStep(focus), CasesStep(model)) =>
      CasesStep.handle_key_event(~focus, ~event, model)
      |> Option.map((x): action => CasesStep(x))
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
        SingleStep(_) | CasesStep(_) | InductionStep(_) | ForallStep(_) |
        MissingStep(_) |
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
      | CasesStep(m) =>
        CasesStep.view_content(
          ~focus=
            switch (focus) {
            | Some(CasesStep(f)) => Some(f)
            | _ => None
            },
          ~inject=x => inject(CasesStep(x)),
          ~take_focus=x => take_focus(CasesStep(x)),
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
    | CasesStep(m) =>
      CasesStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(CasesStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(CasesStep(x)),
        ~take_focus=x => take_focus(CasesStep(x)),
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
      type action = step_action and
      type focus = step_focus;

  let get_state: step_model => EvaluatorState.t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = step_model;
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

  let rec get_state = (model: model) =>
    switch (model.next_step) {
    | None => model.state |> Calc.get_saved_exc(~print="get_state_step")
    | Some(next) => get_state(next)
    };

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
        {
          ...model,
          next_step: Some(new_next_step),
        };
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
                evalobj,
                next_exp: Calc.Pending,
                next_state: Calc.Pending,
              }),
          }
          |> return
        | None => model |> return_quiet
        };
      | (StepForward(_), _, _) => model |> return_quiet
      | (AddCases(exp), MissingStep(_), _) =>
        {
          ...model,
          step_kind: CasesStep(CasesStep.init(~exp?, ())),
        }
        |> return
      | (AddCases(_), _, _) => model |> return_quiet
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
      | (AddAxiomStep(name, at_exp, with_exp), MissingStep(_), _) =>
        let at_id = Exp.rep_id(at_exp);
        {
          ...model,
          step_kind:
            AxiomStep({
              name,
              at_id,
              at_exp,
              with_exp,
              next_exp: Calc.Pending,
            }),
        }
        |> return;
      | (AddAxiomStep(_, _, _), _, _) => model |> return_quiet
      | (StepKindAction(sk_action), _, _) =>
        let* new_step_kind =
          StepKind.update(~settings, sk_action, model.step_kind);
        {
          ...model,
          step_kind: new_step_kind,
        };
      }
    );
  };

  let rec can_undo = (a: step_action): bool => {
    switch (a) {
    | EditorAction(action) => CodeSelectable.Update.can_undo(action)
    | NextStep(next) => can_undo(next)
    | RemoveStep => true
    | StepForward(_) => true
    | AddCases(_) => true
    | AddInduction(_) => true
    | AddForall => true
    | AddAxiomStep(_, _, _) => true
    | StepKindAction(action) => StepKind.can_undo(action)
    };
  };

  let rec calculate =
          (
            ~settings: Calc.t(CoreSettings.t),
            ~exp as expr: Calc.t(Exp.t),
            ~ctx: Calc.t(Ctx.t),
            ~env: Calc.t(ClosureEnvironment.t),
            ~state: Calc.t(EvaluatorState.t),
            {expr: _, state: _, editor, step_kind, next_step, hidden}: step_model,
          )
          : (step_model, Calc.t(Exp.t)) => {
    let editor =
      editor
      |> {
        let.calc settings = settings
        and.calc expr = expr
        and.calc ctx = ctx;
        expr
        |> CodeWithStatics.Model.mk_from_exp(~settings)
        |> CodeSelectable.Update.calculate(
             ~is_dynamic_term=true,
             ~settings,
             ~is_edited=true,
             ~ctx,
             ~dynamics=Dynamics.Map.empty,
             ~stitch=_ =>
             expr
           );
      };
    let (step_kind, hidden, next_expr_state) =
      StepKind.calculate(
        ~settings,
        ~ctx,
        ~exp=expr,
        ~env,
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
                ~env,
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
          calculate(
            ~settings,
            ~exp=next_expr,
            ~ctx,
            ~env,
            ~state=next_state,
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
          | SingleStep(m) => [m.evalobj |> EvaluatorStep.get_step_id]
          | AxiomStep(m) => [m.at_id]
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
              | MakeActive => take_focus(Here())
              | TakeStep(int) => inject(StepForward(int))
              | Refl(int) =>
                inject(
                  AddAxiomStep(
                    "reflexivity",
                    {
                      let _ = print_endline("XYZ");
                      let refl_exps =
                        switch (model.step_kind) {
                        | MissingStep(m) =>
                          m.refls |> Calc.get_saved_exc(~print="refls")
                        | _ => []
                        };
                      List.nth(refl_exps, int);
                    },
                    Exp.fresh(Atom(Bool(true))),
                  ),
                ),
            ~inject=x => inject(EditorAction(x)),
            ~selected=
              switch (focus) {
              | Some(Here(_)) => true
              | _ => false
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
                    | AddCases(exp) => inject(AddCases(exp))
                    | AddAxiomStep(name, e1, e2) =>
                      inject(AddAxiomStep(name, e1, e2)),
                  ~editor=model.editor |> Calc.get_saved_exc(~print="Editor"),
                  m,
                )
              | _ => []
              },
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
          ~undo=None,
          root_step,
        ),
      ),
    ];
  };
};
