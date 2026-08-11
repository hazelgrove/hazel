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
  | EvalStep(EvalStep.model'(step_model))

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
  /* The Proof.t sub-term this step renders (after `Seq` decomposition
   * in `calculate`). `Some` inside a theorem proof, `None` for the
   * cell-level stepper or when a `next_step` has been auto-appended
   * past the end of the proof. Used by the view layer so step kinds
   * can produce `EditorTransform` patches targeting the right
   * `Proof.rep_id` (e.g. MissingStep filling a hole). Derived; not
   * persisted. */
  proof: Calc.saved(option(Proof.t)),
};

let init_step = {
  expr: Calc.Pending,
  editor: Calc.Pending,
  step_kind: MissingStep(MissingStep.Model.init),
  next_step: None,
  hidden: Calc.Pending,
  proof_validity: Calc.Pending,
  editor_info_map: Calc.Pending,
  proof: Calc.Pending,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_action =
  | SingleStep(SingleStep.action'(step_action))
  | InductionStep(InductionStep.action'(step_action))
  | ForallStep(ForallStep.action'(step_action))
  | MissingStep(MissingStep.Update.t)
  | AxiomStep(AxiomStep.action'(step_action))
  | AlgebriteStep(AlgebriteStep.action'(step_action))
  | EvalStep(EvalStep.action'(step_action))

and step_action =
  | StepKindAction(step_kind_action)
  | EditorAction(CodeSelectable.Update.t)
  | NextStep(step_action)
  | RemoveStep
  | StepForward(int)
  | AddInduction(option(Exp.t))
  | AddForall
  | AddAxiomStep(string, int, Exp.t, Direction.t, string)
  | AddAlgebriteStep(int, Exp.t, Exp.t)
  | AddEvalStep(int, Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_focus =
  | SingleStep(SingleStep.focus'(step_focus))
  | InductionStep(InductionStep.focus'(step_focus))
  | ForallStep(ForallStep.focus'(step_focus))
  | MissingStep(MissingStep.Selection.t)
  | AxiomStep(AxiomStep.focus'(step_focus))
  | AlgebriteStep(AlgebriteStep.focus'(step_focus))
  | EvalStep(EvalStep.focus'(step_focus))

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
  module InductionStep = InductionStep.F(Stepper);
  module ForallStep = ForallStep.F(Stepper);
  module MissingStep = MissingStep; // This could be functorized too.
  module AxiomStep = AxiomStep.F(Stepper);
  module AlgebriteStep = AlgebriteStep.F(Stepper);
  module EvalStep = EvalStep.F(Stepper);

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
      | (EvalStep(a), EvalStep(m)) =>
        let* s = EvalStep.update(~settings, a, m);
        (EvalStep(s): model);
      | (
          SingleStep(_) | InductionStep(_) | ForallStep(_) | MissingStep(_) |
          AxiomStep(_) |
          AlgebriteStep(_) |
          EvalStep(_),
          _,
        ) =>
        model |> Updated.raise_invalid_action
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
    | EvalStep(action) => EvalStep.can_undo(action)
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
            ~proof_info_map: Calc.t(Statics.Map.t),
            ~ana,
            ~proof: Calc.t(option(Proof.t)),
            ~proof_map: Calc.t(ProofMap.t),
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
          ~proof_info_map,
          ~ana,
          ~proof,
          ~proof_map,
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
          ~proof_info_map,
          ~ana,
          ~proof,
          ~proof_map,
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
          ~proof_info_map,
          ~ana,
          ~proof,
          ~proof_map,
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
          ~proof_info_map,
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
          ~proof,
          ~proof_map,
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
          ~proof_info_map,
          ~ana,
          ~proof,
          ~proof_map,
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
          ~proof_info_map,
          ~ana,
          ~proof,
          ~proof_map,
          m,
        );
      (AlgebriteStep(m): model, h, e, v);
    | EvalStep(m) =>
      let+ (m, h, e, v) =
        EvalStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~editor,
          ~info_map,
          ~proof_info_map,
          ~ana,
          ~proof,
          ~proof_map,
          m,
        );
      (EvalStep(m): model, h, e, v);
    };

  let get_cursor_info = (~inject, ~focus: focus, model: model) =>
    Cursor.(
      switch (focus, model) {
      | (SingleStep(focus), SingleStep(model)) =>
        let+ focus_info =
          SingleStep.get_cursor_info(
            ~inject=x => inject(SingleStep(x): action),
            ~focus,
            model,
          );
        (SingleStep(focus_info): action);
      | (InductionStep(focus), InductionStep(model)) =>
        let+ focus_info =
          InductionStep.get_cursor_info(
            ~inject=x => inject(InductionStep(x): action),
            ~focus,
            model,
          );
        (InductionStep(focus_info): action);
      | (ForallStep(focus), ForallStep(model)) =>
        let+ focus_info =
          ForallStep.get_cursor_info(
            ~inject=x => inject(ForallStep(x): action),
            ~focus,
            model,
          );
        (ForallStep(focus_info): action);
      | (MissingStep(selection), MissingStep(model)) =>
        let+ focus_info =
          MissingStep.Selection.get_cursor_info(
            ~inject=x => inject(MissingStep(x): action),
            ~selection,
            model,
          );
        (MissingStep(focus_info): action);
      | (AxiomStep(focus), AxiomStep(model)) =>
        let+ focus_info =
          AxiomStep.get_cursor_info(
            ~inject=x => inject(AxiomStep(x): action),
            ~focus,
            model,
          );
        (AxiomStep(focus_info): action);
      | (AlgebriteStep(focus), AlgebriteStep(model)) =>
        let+ focus_info =
          AlgebriteStep.get_cursor_info(
            ~inject=x => inject(AlgebriteStep(x): action),
            ~focus,
            model,
          );
        (AlgebriteStep(focus_info): action);
      | (EvalStep(focus), EvalStep(model)) =>
        let+ focus_info =
          EvalStep.get_cursor_info(
            ~inject=x => inject(EvalStep(x): action),
            ~focus,
            model,
          );
        (EvalStep(focus_info): action);
      | (
          SingleStep(_) | InductionStep(_) | ForallStep(_) | MissingStep(_) |
          AxiomStep(_) |
          AlgebriteStep(_) |
          EvalStep(_),
          _,
        ) => Cursor.empty
      }
    );

  let view_content =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~undo: option(Ui_effect.t(unit)),
        ~is_toplevel: bool,
        ~proof: option(Proof.t),
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        ~main_editor: option(CodeEditable.Channel.t),
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
          (
            ~globals as _,
            ~hide_stepper as _,
            ~undo as _,
            ~is_toplevel as _,
            ~proof as _,
            ~edit_syntax as _,
            ~main_editor as _,
          ) =>
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
      | EvalStep(m) =>
        EvalStep.view_content(
          ~focus=
            switch (focus) {
            | Some(EvalStep(f)) => Some(f)
            | _ => None
            },
          ~inject=x => inject(EvalStep(x)),
          ~take_focus=x => take_focus(EvalStep(x)),
          m,
        )
      };
    f(
      ~globals,
      ~hide_stepper,
      ~undo,
      ~is_toplevel,
      ~proof,
      ~edit_syntax,
      ~main_editor,
    );
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
        ~proof: option(Proof.t),
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
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
        ~proof,
        ~edit_syntax,
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
        ~proof,
        ~edit_syntax,
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
        ~proof,
        ~edit_syntax,
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
        ~proof,
        ~edit_syntax,
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
        ~proof,
        ~edit_syntax,
        m,
      )
    | EvalStep(m) =>
      EvalStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(EvalStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(EvalStep(x)),
        ~take_focus=x => take_focus(EvalStep(x)),
        ~hide_stepper,
        ~undo,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
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
  let get_validity: step_model => option(bool);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = step_model;
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
    proof: Calc.Pending,
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
        | Calc.Pending => model |> raise_invalid_action
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
      | (NextStep(_), _, None) => model |> raise_invalid_action
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
        | None => model |> raise_invalid_action
        };
      | (StepForward(_), _, _) => model |> raise_invalid_action
      | (AddInduction(exp), MissingStep(_), _) =>
        {
          ...model,
          step_kind: InductionStep(InductionStep.init(~exp?, ())),
        }
        |> return
      | (AddInduction(_), _, _) => model |> raise_invalid_action
      | (AddForall, MissingStep(_), _) =>
        {
          ...model,
          step_kind: ForallStep(ForallStep.init(init)),
        }
        |> return
      | (AddForall, _, _) => model |> raise_invalid_action
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
      | (AddAxiomStep(_, _, _, _, _), _, _) => model |> raise_invalid_action
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
      | (AddAlgebriteStep(_, _, _), _, _) => model |> raise_invalid_action
      | (AddEvalStep(at_idx, at_exp), MissingStep(_), _) =>
        {
          ...model,
          step_kind:
            EvalStep({
              at_idx,
              at_exp,
              next_exp: Calc.Pending,
            }),
        }
        |> return
      | (AddEvalStep(_, _), _, _) => model |> raise_invalid_action
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
    | AddEvalStep(_) => true
    | StepKindAction(action) => StepKind.can_undo(action)
    };
  };

  let rec calculate =
          (
            ~settings: Calc.t(CoreSettings.t),
            ~exp as expr: Calc.t(Exp.t),
            ~ctx: Calc.t(SemanticCtx.t),
            ~ana: Calc.t(Typ.t),
            ~proof: Calc.t(option(Proof.t)),
            ~proof_map: Calc.t(ProofMap.t),
            ~proof_info_map: Calc.t(Statics.Map.t)=Calc.OldValue(
                                                      Id.Map.empty,
                                                    ),
            {
              expr: _,
              editor,
              step_kind,
              next_step,
              hidden,
              proof_validity,
              editor_info_map: info_map,
              proof: _,
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
        expr |> CodeWithStatics.Model.mk_from_exp(~settings, ~root=Exp);
      };
    let info_map =
      info_map
      |> {
        let.calc editor: CodeSelectable.Model.t = editor;
        editor.statics.info_map;
      };
    /* Decompose the proof sub-term passed to this step. A `Seq(head, tail)`
     * represents "step then more steps", so the head describes the current
     * step kind and the tail is recursed into `next_step`. A leaf-shaped
     * term (AxiomStep, EvalStep, Forall, Induction, …) describes the
     * current step and has no further chain. None propagates as None
     * (cell-level stepper / out of theorem).  Holes (EmptyHole / Invalid /
     * MultiHole) are forwarded as the current step's proof so the
     * upcoming MissingStep rewire can read them. */
    let split_proof = (p: option(Proof.t)) =>
      switch (p) {
      | Some({term: Seq(head, tail), _}) => (Some(head), Some(tail))
      | _ => (p, None)
      };
    let (proof_head, proof_tail): (
      Calc.t(option(Proof.t)),
      Calc.t(option(Proof.t)),
    ) =
      switch (proof) {
      | OldValue(p) =>
        let (h, t) = split_proof(p);
        (OldValue(h), OldValue(t));
      | NewValue(p) =>
        let (h, t) = split_proof(p);
        (NewValue(h), NewValue(t));
      };
    /* When the proof sub-term contradicts the current step kind (e.g.
     * the proof became `AxiomStep(_)` after a syntax-side patch but the
     * stepper model still carries `MissingStep(_)`), swap the step kind
     * to the proof-implied variant with a placeholder model. The kind's
     * own `calculate` then fills in the model fields from the proof on
     * the same pass (see `AxiomStep.calculate`). Only kinds whose
     * emitters have moved to `EditorTransform` patches participate
     * here; the rest keep the legacy mutate-model path.
     *
     * Important: `SingleStep` is UI-only state (an auto-step via the
     * evaluator) with no Proof.t equivalent, so a proof-side hole shape
     * does NOT pull the kind back to `MissingStep` when the user has
     * already taken an auto-step — only a kind that already represents
     * a proof leaf is collapsed. Otherwise clicking the "Step" button
     * would be silently undone on the next render. */
    let is_proof_leaf_kind = (sk: step_kind_model): bool =>
      switch (sk) {
      | AxiomStep(_)
      | AlgebriteStep(_)
      | EvalStep(_)
      | ForallStep(_)
      | InductionStep(_) => true
      | SingleStep(_)
      | MissingStep(_) => false
      };
    let adapt_step_kind =
        (sk: step_kind_model, proof_h: Calc.t(option(Proof.t)))
        : step_kind_model =>
      switch (Calc.get_value(proof_h), sk) {
      | (Some({term: AxiomStep(_), _}), AxiomStep(_)) => sk
      | (Some({term: AxiomStep(_), _}), _) =>
        AxiomStep({
          name: "",
          at_idx: 0,
          at_exp: Exp.fresh(EmptyHole),
          direction: Direction.Right,
          equality: "",
          next_exp: Calc.Pending,
        })
      | (Some({term: AlgebriteStep(_), _}), AlgebriteStep(_)) => sk
      | (Some({term: AlgebriteStep(_), _}), _) =>
        AlgebriteStep({
          at_idx: 0,
          at_exp: Exp.fresh(EmptyHole),
          with_exp: Exp.fresh(EmptyHole),
          next_exp: Calc.Pending,
        })
      | (Some({term: EvalStep(_), _}), EvalStep(_)) => sk
      | (Some({term: EvalStep(_), _}), _) =>
        EvalStep({
          at_idx: 0,
          at_exp: Exp.fresh(EmptyHole),
          next_exp: Calc.Pending,
        })
      | (Some({term: Forall(_, _), _}), ForallStep(_)) => sk
      | (Some({term: Forall(_, _), _}), _) =>
        ForallStep(ForallStep.init(init))
      | (Some({term: Induction(_, _), _}), InductionStep(_)) => sk
      | (Some({term: Induction(scrut, _), _}), _) =>
        /* Seed the scrutinee editor from the proof's scrutinee (freshening
         * ids so the step's editor is independent of the syntax copy);
         * otherwise the scrutinee shows only in the syntax and the empty
         * editor writes back through, erasing it. */
        InductionStep(
          InductionStep.init(~exp=scrut |> Exp.replace_all_ids, ()),
        )
      | (Some({term: EmptyHole | Invalid(_) | MultiHole(_), _}), _)
          when is_proof_leaf_kind(sk) =>
        MissingStep(MissingStep.Model.init)
      | _ => sk
      };
    let step_kind = adapt_step_kind(step_kind, proof_head);
    let (step_kind, hidden, next_expr, inner_validity) =
      StepKind.calculate(
        ~settings,
        ~ctx,
        ~exp=expr,
        ~hidden,
        ~editor,
        ~info_map,
        ~proof_info_map,
        ~ana,
        ~proof=proof_head,
        ~proof_map,
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
                ~proof_info_map,
                ~ana,
                ~proof=proof_head,
                ~proof_map,
              )
           |> Option.get
         );
    let (next_step, last_expr, next_validity) =
      switch (next_expr) {
      | Some(next_expr) =>
        let next_step = Option.value(~default=init, next_step);
        let (next_step, last_expr, next_validity) =
          calculate(
            ~settings,
            ~exp=next_expr,
            ~ctx,
            ~ana,
            ~proof_info_map,
            ~proof=proof_tail,
            ~proof_map,
            next_step,
          );
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
        /* Save the full input Proof.t at this step (i.e. the Seq /
         * leaf / hole that was passed as `~proof`), not just the head
         * extracted by `split_proof`. The view uses this for both:
         *   - "add a step" patches, where current_proof is a hole and
         *     we replace it with `Seq(new_step, EmptyHole)`; and
         *   - "remove a step" patches, where current_proof is a Seq
         *     or leaf and we replace it with `EmptyHole`.
         * Kind-derivation logic (adapt_step_kind, EvalStep.calculate)
         * gets the split `proof_head` directly so it still sees the
         * leaf shape. */
        proof: proof |> Calc.save,
      },
      last_expr,
      proof_validity,
    );
  };

  let rec get_cursor_info =
          (~inject, ~focus: step_focus, model: step_model)
          : Cursor.cursor(step_action) => {
    Cursor.(
      switch (focus, model.step_kind, model.next_step) {
      | (StepKindFocus(sk), skm, _) =>
        let+ ci =
          StepKind.get_cursor_info(
            ~inject=x => inject(StepKindAction(x)),
            ~focus=sk,
            skm,
          );
        StepKindAction(ci);
      | (Here(a), _, _) =>
        let+ ci =
          StepperEditor.Selection.get_cursor_info(
            ~inject=x => inject(EditorAction(x)),
            ~selection=a,
            model.editor |> Calc.get_saved_exc(~print="Step editor selection"),
          );
        EditorAction(ci);
      | (Next(a), _, Some(next_step)) =>
        let+ ci =
          get_cursor_info(
            ~inject=x => inject(NextStep(x): action),
            ~focus=a,
            next_step,
          );
        NextStep(ci);
      | (Next(_), _, None) => Cursor.empty
      }
    );
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
            /* Forwarded write channel: when this step has a Proof.t
             * sub-term in scope, AddAxiomStep / AddAlgebriteStep /
             * AddInduction / AddForall emissions are rewritten as
             * `ProofPatch`es targeting `Proof.rep_id(model.proof)`
             * instead of going through `inject(AddAxiomStep(...))`.
             * Out-of-theorem callers wire it to a no-op and the legacy
             * stepper-local mutation continues. */
            ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=
                                                                    _ =>
                                                                    Ui_effect.Ignore,
            /* Forwarded main-editor capability handle for step views
             * that render slices of the surrounding syntax as
             * sub-editors (see SubEditor.re / CodeEditable.Channel). */
            ~main_editor: option(CodeEditable.Channel.t)=None,
            /* The proof leaf of the PREVIOUS step in the chain, when
             * this step was synthesized past the end of the written
             * proof (steps no longer leave a trailing `; ?` hole).
             * "Add step" emissions here extend the chain by replacing
             * that leaf with `Seq(leaf, new_step)`. */
            ~extend_proof: option(Proof.t)=None,
            model: step_model,
          ) => {
    let is_last_step = StepKind.is_missing_step(model.step_kind);
    let is_skipped_step = model.hidden == Calc.Calculated(true);
    let showing_skiped_steps =
      globals.settings.core.evaluation.show_hidden_steps;
    let showing_history = globals.settings.core.evaluation.stepper_history;
    /* Proof.t sub-term this step renders, populated by `calculate` after
     * Seq-descent. `Some` inside a theorem proof, `None` for the
     * cell-level result stepper. When `Some(_)`, step "create" emissions
     * (AddAxiomStep / AddAlgebriteStep / AddInduction / AddForall) are
     * rewritten as `ProofPatch`es targeting `Proof.rep_id` so the source
     * of truth stays the syntax tree. */
    let current_proof: option(Proof.t) =
      model.proof |> Calc.get_saved_opt |> Option.join;
    /* Patch that lands a new chained step (AxiomStep / AlgebriteStep /
     * EvalStep) in the proof syntax. Two shapes:
     *   - this step is backed by a written hole (`?`): replace the
     *     hole with the bare step — no trailing `; ?` is appended, the
     *     next-step UI is synthesized by `calculate` regardless;
     *   - this step was synthesized past the end of the chain (no
     *     backing proof): extend the chain by replacing the previous
     *     step's leaf with `Seq(leaf, new_step)`. */
    let add_step_patch =
        (proof_term: TermBase.Proof.term)
        : option(Haz3lcore.EditorTransform.patch) => {
      let head = Proof.fresh(proof_term);
      /* A hole "extends" by being replaced — it stands for "the proof
       * continues here", not for a step of its own. */
      let extend = (prev: Proof.t) =>
        switch (prev.term) {
        | EmptyHole
        | Invalid(_)
        | MultiHole(_) => head
        | _ => Proof.fresh(Seq(prev, head))
        };
      switch (current_proof, extend_proof) {
      | (Some(p), _) =>
        Some(
          Haz3lcore.EditorTransform.mk_proof_patch(
            ~target_id=Proof.rep_id(p),
            head,
          ),
        )
      | (None, Some(prev)) =>
        Some(
          Haz3lcore.EditorTransform.mk_proof_patch(
            ~target_id=Proof.rep_id(prev),
            extend(prev),
          ),
        )
      | (None, None) => None
      };
    };
    /* Compound step kinds (Forall, Induction) contain their own body
     * proof rather than chaining, but they land in the syntax the same
     * way: replace the written hole, or extend the previous leaf. */
    let replace_proof_patch = add_step_patch;
    /* Expressions embedded into a freshly-inserted proof step (at_exp /
     * with_exp / scrut) are sliced from the current step's expression, so:
     *  - their tile ids already occur elsewhere in the rendered theorem —
     *    freshen them, otherwise a tile id is duplicated and
     *    Highlight.of_tile fails with a shard mismatch; and
     *  - they may contain closures the evaluator substituted in, which
     *    have no surface syntax — writing them verbatim corrupts the
     *    program and crashes rendering. Substitution removes them by
     *    inlining each closure's environment into its body. */
    let embed_exp = (e: Exp.t): Exp.t =>
      e |> Substitution.in_exp(Environment.empty) |> Exp.replace_all_ids;
    let axiom_step_proof_term =
        (
          ~at_idx: int,
          ~at_exp: Exp.t,
          ~direction: Direction.t,
          ~equality: string,
        )
        : TermBase.Proof.term =>
      AxiomStep({
        at_idx: Exp.fresh(Atom(Int(Bigint.of_int(at_idx)))),
        at_exp: at_exp |> embed_exp,
        direction,
        equality: Exp.fresh(Var(equality)),
      });
    let eval_step_proof_term =
        (~at_idx: int, ~at_exp: Exp.t): TermBase.Proof.term =>
      EvalStep({
        at_idx: Exp.fresh(Atom(Int(Bigint.of_int(at_idx)))),
        at_exp: at_exp |> embed_exp,
      });
    let algebrite_step_proof_term =
        (~at_idx: int, ~at_exp: Exp.t, ~with_exp: Exp.t): TermBase.Proof.term =>
      AlgebriteStep({
        at_idx: Exp.fresh(Atom(Int(Bigint.of_int(at_idx)))),
        at_exp: at_exp |> embed_exp,
        with_exp: with_exp |> embed_exp,
      });
    let induction_proof_term = (~scrut: option(Exp.t)): TermBase.Proof.term =>
      Induction(
        scrut
        |> Option.map(embed_exp)
        |> Option.value(~default=Exp.fresh(EmptyHole)),
        /* Start with no cases; the user adds them via the
         * InductionStep UI. `MakeTerm.prul` accepts a bare scrutinee
         * with no `| <pat> => <body>` tiles. */
        [],
      );
    let forall_proof_term = (): TermBase.Proof.term =>
      Forall(Pat.fresh(EmptyHole), Proof.fresh(EmptyHole));
    /* AddAxiomStep emission helper used at both the StepperEditor Refl
     * button and the MissingStep overlay signal handler. Falls back to
     * the legacy stepper-local mutation when no proof sub-term is in
     * scope (cell-level stepper / out-of-theorem). */
    let emit_add_axiom_step =
        (
          ~name: string,
          ~at_idx: int,
          ~at_exp: Exp.t,
          ~direction: Direction.t,
          ~equality: string,
        )
        : Ui_effect.t(unit) =>
      switch (
        add_step_patch(
          axiom_step_proof_term(~at_idx, ~at_exp, ~direction, ~equality),
        )
      ) {
      | Some(patch) => edit_syntax(patch)
      | None =>
        inject(AddAxiomStep(name, at_idx, at_exp, direction, equality))
      };
    /* AddAlgebriteStep emission: the "Replace" button in the Algebra
     * dropdown writes an `AlgebriteStep({at_idx, at_exp, with_exp})`
     * leaf wrapped in a `Seq(_, EmptyHole)` so the chain can continue.
     * Falls back to the legacy stepper-local mutation when no proof
     * sub-term is in scope. */
    let emit_add_algebrite_step =
        (~at_idx: int, ~at_exp: Exp.t, ~with_exp: Exp.t): Ui_effect.t(unit) =>
      switch (
        add_step_patch(
          algebrite_step_proof_term(~at_idx, ~at_exp, ~with_exp),
        )
      ) {
      | Some(patch) => edit_syntax(patch)
      | None => inject(AddAlgebriteStep(at_idx, at_exp, with_exp))
      };
    /* AddInduction emission: the "Cases/Induction" button writes an
     * `Induction(scrut, [])` proof node. Cases are added separately
     * (today via the legacy `InductionStep.AddCase` action). Unlike a
     * "leaf" step, Induction is the entire compound step at this
     * level so we use `replace_proof_patch` (no Seq wrap). */
    let emit_add_induction = (~scrut: option(Exp.t)): Ui_effect.t(unit) =>
      switch (replace_proof_patch(induction_proof_term(~scrut))) {
      | Some(patch) => edit_syntax(patch)
      | None => inject(AddInduction(scrut))
      };
    /* AddForall emission: the "Function Body" button writes a
     * `Forall(EmptyHole_pat, EmptyHole_proof)` node. Like Induction
     * this is a compound step (the body proof holds the next chain),
     * so we replace without Seq-wrapping. */
    let emit_add_forall = (): Ui_effect.t(unit) =>
      switch (replace_proof_patch(forall_proof_term())) {
      | Some(patch) => edit_syntax(patch)
      | None => inject(AddForall)
      };
    /* RemoveStep emission: the "remove" button on a child step asks
     * THIS step to drop its proof leaf and become a hole again. In
     * proof mode we rewrite the syntax: replace this step's full
     * input proof (a Seq wrapping this step's leaf + the tail of the
     * chain, or just a leaf) with `EmptyHole`. Out of proof scope the
     * legacy stepper-local `RemoveStep` action runs instead. */
    let emit_remove_step = (): Ui_effect.t(unit) =>
      switch (current_proof) {
      | Some(p) =>
        edit_syntax(
          Haz3lcore.EditorTransform.mk_proof_patch(
            ~target_id=Proof.rep_id(p),
            Proof.fresh(EmptyHole),
          ),
        )
      | None => inject(RemoveStep)
      };
    /* TakeStep emission: in proof mode the "Step" button records an
     * `EvalStep({at_idx, at_exp})` proof node so the syntax tree is the
     * source of truth. Outside proof mode falls back to the legacy
     * stepper-local `StepForward` action which inserts a UI-only
     * `SingleStep` (no Proof.t equivalent). */
    let emit_take_step = (~idx: int): Ui_effect.t(unit) => {
      let msns =
        switch (model.step_kind) {
        | MissingStep(m) =>
          m.next_steps
          |> Calc.get_saved_exc(~print="emit_take_step")
          |> (
            fun
            | EvaluatorStep.AutoStep(_) => []
            | AvailableSteps(steps) => steps
          )
        | _ => []
        };
      switch (List.nth_opt(msns, idx)) {
      | Some(evalobj) =>
        let at_exp = EvaluatorStep.get_at_exp(evalobj);
        let current_expr =
          model.expr |> Calc.get_saved_exc(~print="emit_take_step expr");
        /* Compute the occurrence index relative to the step's *current*
         * incoming expression (the one this MissingStep was created
         * for); fall back to the EvalObj's own ctx-based index when the
         * sub-expression isn't directly findable in current_expr (e.g.
         * due to evaluation context bookkeeping). */
        let at_idx_int =
          try(ProofHacks.exp_idx(at_exp, current_expr)) {
          | _ => EvaluatorStep.get_exp_idx(evalobj)
          };
        switch (
          add_step_patch(eval_step_proof_term(~at_idx=at_idx_int, ~at_exp))
        ) {
        | Some(patch) => edit_syntax(patch)
        | None => inject(StepForward(idx))
        };
      | None => inject(StepForward(idx))
      };
    };
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
              | TakeStep(int) => emit_take_step(~idx=int)
              | Refl(int) => {
                  let refl_exps =
                    switch (model.step_kind) {
                    | MissingStep(m) =>
                      m.refls |> Calc.get_saved_exc(~print="refls")
                    | _ => []
                    };
                  let from_exp = List.nth(refl_exps, int);
                  emit_add_axiom_step(
                    ~name="reflexivity",
                    ~at_idx=
                      ProofHacks.exp_idx(
                        from_exp,
                        model.expr |> Calc.get_saved_exc(~print="expr"),
                      ),
                    ~at_exp=from_exp,
                    ~direction=Direction.Right,
                    ~equality="refl_eq",
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
                    | AddForall => emit_add_forall()
                    | AddInduction(scrut) => emit_add_induction(~scrut)
                    | AddAxiomStep(name, idx, e1, dir, eq) =>
                      emit_add_axiom_step(
                        ~name,
                        ~at_idx=idx,
                        ~at_exp=e1,
                        ~direction=dir,
                        ~equality=eq,
                      )
                    | AddAlgebriteStep(idx, e1, e2) =>
                      emit_add_algebrite_step(
                        ~at_idx=idx,
                        ~at_exp=e1,
                        ~with_exp=e2,
                      )
                    | TakeStep(i) => emit_take_step(~idx=i)
                    | Refl(i) => {
                        let refl_exps =
                          switch (model.step_kind) {
                          | MissingStep(m) =>
                            m.refls |> Calc.get_saved_exc(~print="refls")
                          | _ => []
                          };
                        let from_exp = List.nth(refl_exps, i);
                        emit_add_axiom_step(
                          ~name="reflexivity",
                          ~at_idx=
                            ProofHacks.exp_idx(
                              from_exp,
                              model.expr |> Calc.get_saved_exc(~print="expr"),
                            ),
                          ~at_exp=from_exp,
                          ~direction=Direction.Right,
                          ~equality="refl_eq",
                        );
                      },
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
            ~proof=current_proof,
            ~edit_syntax,
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
            ~proof=current_proof,
            ~edit_syntax,
            ~main_editor,
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
              Some(emit_remove_step());
            },
          ~edit_syntax,
          ~main_editor,
          /* When this step's proof is a leaf (no Seq tail), the next
           * step is synthesized past the end of the written chain: its
           * "add step" emissions extend this leaf. A Seq tail flows to
           * the next step as its own proof instead. Steps that have no
           * proof of their own (auto-taken evaluator steps are UI-only)
           * pass the last known leaf along, so the actionable
           * MissingStep further down can still land its patch. */
          ~extend_proof=
            switch (current_proof) {
            | Some({term: Seq(_), _}) => None
            | Some(_) as leaf => leaf
            | None => extend_proof
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
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=_ =>
                                                                    Ui_effect.Ignore,
        ~main_editor: option(CodeEditable.Channel.t)=None,
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
          ~edit_syntax,
          ~main_editor,
          root_step,
        ),
      ),
    ];
  };
};
