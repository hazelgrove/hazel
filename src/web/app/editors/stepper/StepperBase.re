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
  | AlgebriteStep(AlgebriteStep.model'(step_model))

and step_model = {
  // Calculated
  expr: Calc.saved(Exp.t),
  editor: Calc.saved(CodeSelectable.Model.t), // Also Updated.
  // Updated
  step_kind: step_kind_model,
  next_step: option(step_model),
  // Calculated
  hidden: Calc.saved(bool),
  proof_validity: Calc.saved(option(bool)),
  editor_info_map: Calc.saved(Statics.Map.t),
};

let init_step = {
  expr: Calc.Pending,
  editor: Calc.Pending,
  step_kind: MissingStep(MissingStep.Model.init),
  next_step: None,
  hidden: Calc.Pending,
  proof_validity: Calc.Pending,
  editor_info_map: Calc.Pending,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_step_kind =
  | SingleStep(SingleStep.persistent'(persistent_step))
  | InductionStep(InductionStep.persistent'(persistent_step))
  | ForallStep(ForallStep.persistent'(persistent_step))
  | MissingStep(MissingStep.Model.persistent)
  | AxiomStep(AxiomStep.persistent'(persistent_step))
  | AlgebriteStep(AlgebriteStep.persistent'(persistent_step))

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
  | AlgebriteStep(AlgebriteStep.action'(step_action))

and step_action =
  | StepKindAction(step_kind_action)
  | EditorAction(CodeSelectable.Update.t)
  | NextStep(step_action)
  | RemoveStep
  | StepForward(int)
  | AddInduction(option(Exp.t))
  | AddForall
  | AddAxiomStep(string, int, Exp.t, Direction.t, string)
  | AddAlgebriteStep(int, Exp.t, Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_focus =
  | SingleStep(SingleStep.focus'(step_focus))
  | InductionStep(InductionStep.focus'(step_focus))
  | ForallStep(ForallStep.focus'(step_focus))
  | MissingStep(MissingStep.Selection.t)
  | AxiomStep(AxiomStep.focus'(step_focus))
  | AlgebriteStep(AlgebriteStep.focus'(step_focus))

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
  module AlgebriteStep = AlgebriteStep.F(Stepper);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = step_kind_model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = persistent_step_kind;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = step_kind_action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = step_kind_focus;

  let persist = (model: model): persistent => {
    switch (model) {
    | SingleStep(m) => SingleStep(SingleStep.persist(m))
    | InductionStep(m) => InductionStep(InductionStep.persist(m))
    | ForallStep(m) => ForallStep(ForallStep.persist(m))
    | MissingStep(m) => MissingStep(MissingStep.Model.persist(m))
    | AxiomStep(m) => AxiomStep(AxiomStep.persist(m))
    | AlgebriteStep(m) => AlgebriteStep(AlgebriteStep.persist(m))
    };
  };

  let unpersist = (p: persistent): model => {
    switch (p) {
    | SingleStep(m) => SingleStep(SingleStep.unpersist(m))
    | InductionStep(m) => InductionStep(InductionStep.unpersist(m))
    | ForallStep(m) => ForallStep(ForallStep.unpersist(m))
    | MissingStep(m) => MissingStep(MissingStep.Model.unpersist(m))
    | AxiomStep(m) => AxiomStep(AxiomStep.unpersist(m))
    | AlgebriteStep(m) => AlgebriteStep(AlgebriteStep.unpersist(m))
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
      | (AlgebriteStep(a), AlgebriteStep(m)) =>
        let* s = AlgebriteStep.update(~settings, a, m);
        (AlgebriteStep(s): model);
      | (
          SingleStep(_) | InductionStep(_) | ForallStep(_) | MissingStep(_) |
          AxiomStep(_) |
          AlgebriteStep(_),
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
    | AlgebriteStep(action) => AlgebriteStep.can_undo(action)
    };
  };

  let rec calculate =
          (
            ~settings: Calc.t(CoreSettings.t),
            ~hidden: Calc.saved(bool),
            ~exp: Calc.t(Exp.t),
            ~ctx: Calc.t(SemanticCtx.t),
            ~editor: Calc.t(CodeSelectable.Model.t),
            ~info_map: Calc.t(Statics.Map.t),
            ~ana,
            model: model,
          ) =>
    switch (model) {
    | SingleStep(m) =>
      let+ (m, h, e, v) =
        SingleStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~editor,
          ~info_map,
          ~ana,
          m,
        );
      (SingleStep(m): model, h, e, v);
    | InductionStep(m) =>
      let+ (m, h, e, v) =
        InductionStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~editor,
          ~info_map,
          ~ana,
          m,
        );
      (InductionStep(m): model, h, e, v);
    | ForallStep(m) =>
      let+ (m, h, e, v) =
        ForallStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~editor,
          ~info_map,
          ~ana,
          m,
        );
      (ForallStep(m): model, h, e, v);
    | MissingStep(missing_step) =>
      let next_steps =
        missing_step.next_steps
        |> {
          let.calc settings = settings
          and.calc exp = exp
          and.calc ctx = ctx;
          EvaluatorStep.get_status(~settings, exp, SemanticCtx.get_env(ctx));
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
          ~info_map,
          ~exp=exp |> Calc.make_new,
          ~ctx=ctx |> Calc.make_new,
          SingleStep({
            persistent_evalobj: evalobj |> EvaluatorStep.persist,
            evalobj: Calc.Pending,
            next_exp: Calc.Pending,
          }): model,
          ~hidden,
          ~editor,
          ~ana,
        )
      | None =>
        Some((
          MissingStep(
            MissingStep.Update.calculate(
              ~settings=settings |> Calc.get_value,
              exp,
              info_map,
              ctx,
              next_steps,
              missing_step,
              editor,
            ),
          ),
          Calc.set(false, hidden),
          None,
          Calc.NewValue(
            // TODO: Incremental validity check
            DHExp.fast_equal(
              exp |> Calc.get_value,
              Exp.temp(Atom(Bool(true))),
            )
              ? Some(true)
              : DHExp.fast_equal(
                  exp |> Calc.get_value,
                  Exp.temp(Atom(Bool(false))),
                )
                  ? Some(false) : None,
          ),
        ))
      };
    | AxiomStep(m) =>
      let+ (m, h, e, v) =
        AxiomStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~editor,
          ~info_map,
          ~ana,
          m,
        );
      (AxiomStep(m): model, h, e, v);
    | AlgebriteStep(m) =>
      let+ (m, h, e, v) =
        AlgebriteStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~editor,
          ~info_map,
          ~ana,
          m,
        );
      (AlgebriteStep(m): model, h, e, v);
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
      | (AlgebriteStep(focus), AlgebriteStep(model)) =>
        let+ focus_info = AlgebriteStep.get_cursor_info(~focus, model);
        (AlgebriteStep(focus_info): action);
      | (
          SingleStep(_) | InductionStep(_) | ForallStep(_) | MissingStep(_) |
          AxiomStep(_) |
          AlgebriteStep(_),
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
    | (AlgebriteStep(focus), AlgebriteStep(model)) =>
      AlgebriteStep.handle_key_event(~focus, ~event, model)
      |> Option.map((x): action => AlgebriteStep(x))
    | (
        SingleStep(_) | InductionStep(_) | ForallStep(_) | MissingStep(_) |
        AxiomStep(_) |
        AlgebriteStep(_),
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
      | AlgebriteStep(m) =>
        AlgebriteStep.view_content(
          ~focus=
            switch (focus) {
            | Some(AlgebriteStep(f)) => Some(f)
            | _ => None
            },
          ~inject=x => inject(AlgebriteStep(x)),
          ~take_focus=x => take_focus(AlgebriteStep(x)),
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
    | AlgebriteStep(m) =>
      AlgebriteStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(AlgebriteStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(AlgebriteStep(x)),
        ~take_focus=x => take_focus(AlgebriteStep(x)),
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
  let get_validity: step_model => option(bool);
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
    editor: Calc.Pending,
    step_kind: MissingStep(MissingStep.Model.init),
    next_step: None,
    hidden: Calc.Pending,
    proof_validity: Calc.Pending,
    editor_info_map: Calc.Pending,
  };

  let rec persist = (model: model): persistent => {
    {
      step_kind: StepKind.persist(model.step_kind),
      next_step: model.next_step |> Option.map(persist),
    };
  };

  let rec unpersist = (p: persistent): model => {
    {
      expr: Calc.Pending,
      editor: Calc.Pending,
      step_kind: StepKind.unpersist(p.step_kind),
      next_step: p.next_step |> Option.map(unpersist),
      hidden: Calc.Pending,
      proof_validity: Calc.Pending,
      editor_info_map: Calc.Pending,
    };
  };

  let get_validity = (model: model) =>
    model.proof_validity
    |> Calc.get_saved_exc(
         ~print="get_validity called before calculate on stepper",
       );

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
                persistent_evalobj: evalobj |> EvaluatorStep.persist,
                evalobj: Calc.Calculated(evalobj),
                next_exp: Calc.Pending,
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
      | (
          AddAxiomStep(name, at_idx, at_exp, direction, equality),
          MissingStep(_),
          _,
        ) =>
        {
          ...model,
          step_kind:
            AxiomStep({
              name,
              at_idx,
              at_exp,
              direction,
              equality,
              next_exp: Calc.Pending,
            }),
        }
        |> return
      | (AddAxiomStep(_, _, _, _, _), _, _) => model |> return_quiet
      | (AddAlgebriteStep(at_idx, at_exp, with_exp), MissingStep(_), _) =>
        {
          ...model,
          step_kind:
            AlgebriteStep({
              at_idx,
              at_exp,
              with_exp,
              next_exp: Calc.Pending,
            }),
        }
        |> return
      | (AddAlgebriteStep(_, _, _), _, _) => model |> return_quiet
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
    | AddInduction(_) => true
    | AddForall => true
    | AddAxiomStep(_) => true
    | AddAlgebriteStep(_) => true
    | StepKindAction(action) => StepKind.can_undo(action)
    };
  };

  let rec calculate =
          (
            ~settings: Calc.t(CoreSettings.t),
            ~exp as expr: Calc.t(Exp.t),
            ~ctx: Calc.t(SemanticCtx.t),
            ~ana: Calc.t(Typ.t),
            {
              expr: _,
              editor,
              step_kind,
              next_step,
              hidden,
              proof_validity,
              editor_info_map: info_map,
            }: step_model,
          )
          : (step_model, Calc.t(Exp.t), Calc.t(option(bool))) => {
    let editor =
      editor
      |> {
        let.calc settings = settings
        and.calc expr = expr
        and.calc _ctx = ctx
        and.calc _ana = ana;
        expr |> CodeWithStatics.Model.mk_from_exp(~settings);
      };
    let info_map =
      info_map
      |> {
        let.calc editor: CodeSelectable.Model.t = editor;
        editor.statics.info_map;
      };
    let (step_kind, hidden, next_expr, inner_validity) =
      StepKind.calculate(
        ~settings,
        ~ctx,
        ~exp=expr,
        ~hidden,
        ~editor,
        ~info_map,
        ~ana,
        step_kind,
      )
      |> OptUtil.get(() =>
           MissingStep(MissingStep.Model.init)
           |> StepKind.calculate(
                ~settings,
                ~ctx,
                ~exp=expr,
                ~hidden,
                ~editor,
                ~info_map,
                ~ana,
              )
           |> Option.get
         );
    let (next_step, last_expr, next_validity) =
      switch (next_expr) {
      | Some(next_expr) =>
        let next_step = Option.value(~default=init, next_step);
        let (next_step, last_expr, next_validity) =
          calculate(~settings, ~exp=next_expr, ~ctx, ~ana, next_step);
        (Some(next_step), last_expr, next_validity);
      | None => (None, expr, inner_validity)
      };

    let proof_validity =
      proof_validity
      |> {
        let.calc next_validity = next_validity
        and.calc inner_validity = inner_validity;
        switch (next_validity, inner_validity) {
        | (Some(true), Some(true)) => Some(true)
        | (Some(false), Some(false)) => Some(false)
        | _ => None
        };
      };

    // TODO: Make editor calculation more incremental
    let editor =
      CodeSelectable.Update.calculate(
        ~is_dynamic_term=true,
        ~settings=Calc.get_value(settings),
        ~is_edited=true,
        ~ctx=Calc.get_value(ctx) |> SemanticCtx.get_ctx,
        ~dynamics=Dynamics.Map.empty,
        ~ana=Calc.get_value(ana),
        ~stitch=_ => Calc.get_value(expr),
        Calc.get_value(editor),
      );

    (
      {
        expr: expr |> Calc.save,

        editor: Calc.Calculated(editor),
        step_kind,
        next_step,
        hidden: hidden |> Calc.save,
        proof_validity: proof_validity |> Calc.save,
        editor_info_map: info_map |> Calc.save,
      },
      last_expr,
      proof_validity,
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
        let selected_exp =
          switch (model.step_kind) {
          | MissingStep(m) =>
            m.selected_exp |> Calc.get_saved_opt |> Option.join
          | _ => None
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
                      Direction.Right,
                      "Reflexive(==)",
                    ),
                  );
                },
            ~inject=x => inject(EditorAction(x)),
            ~selected=
              switch (focus) {
              | Some(Here(_)) => true
              | _ => false
              },
            ~selected_id=selected_exp |> Option.map(Exp.rep_id),
            ~overlays=
              switch (model.step_kind) {
              | MissingStep(m)
                  when globals.settings.core.evaluation.enable_proof =>
                MissingStep.View.view_overlay(
                  ~globals,
                  ~info_map=
                    model.editor_info_map
                    |> Calc.get_saved_exc(~print="info_map"),
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
                    | AddAxiomStep(name, idx, e1, dir, eq) =>
                      inject(AddAxiomStep(name, idx, e1, dir, eq))
                    | AddAlgebriteStep(idx, e1, e2) =>
                      inject(AddAlgebriteStep(idx, e1, e2))
                    | TakeStep(i) => inject(StepForward(i)),
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
