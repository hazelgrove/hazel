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
  | WrittenStep(WrittenStep.model'(step_model))
  | ReparenthesizeStep({
      original_exp: Exp.t,
      reparenthesized_exp: Exp.t,
      selected_id: option(Id.t),
      evaluate_after_parenthesize: bool,
      next_exp: Calc.saved(Exp.t),
    })
  | AutoSimplifyStep({
      original_exp: Exp.t,
      simplified_exp: Exp.t,
      next_exp: Calc.saved(Exp.t),
    })

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
  export_warning: option(string),
};

let init_step = {
  expr: Calc.Pending,
  editor: Calc.Pending,
  step_kind: MissingStep(MissingStep.Model.init),
  next_step: None,
  hidden: Calc.Pending,
  proof_validity: Calc.Pending,
  editor_info_map: Calc.Pending,
  export_warning: None,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_step_kind =
  | SingleStep(SingleStep.persistent'(persistent_step))
  | InductionStep(InductionStep.persistent'(persistent_step))
  | ForallStep(ForallStep.persistent'(persistent_step))
  | MissingStep(MissingStep.Model.persistent)
  | AxiomStep(AxiomStep.persistent'(persistent_step))
  | AlgebriteStep(AlgebriteStep.persistent'(persistent_step))
  | WrittenStep(WrittenStep.persistent'(persistent_step))
  | ReparenthesizeStep(Exp.t, Exp.t) /* original_exp, reparenthesized_exp */
  | AutoSimplifyStep(Exp.t, Exp.t) /* original_exp, simplified_exp */

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
  | WrittenStep(WrittenStep.action'(step_action))

and step_action =
  | StepKindAction(step_kind_action)
  | EditorAction(CodeSelectable.Update.t)
  | NextStep(step_action)
  | RemoveStep
  | StepForward(int)
  | StepForwardOnSelection(list(Id.t), bool)
  | AutoSimplifySelection(Exp.t, Exp.t)
  | AddInduction(option(Exp.t))
  | AddForall
  | AddAxiomStep(string, int, Exp.t, Direction.t, string)
  | AddAlgebriteStep(int, Exp.t, Exp.t)
  | AddReparenthesizedAlgebriteStep(Exp.t, Exp.t, Exp.t)
  | AddReparenthesizedWrittenStep(
      RewriteChecker.trace_summary,
      Exp.t,
      Exp.t,
      Exp.t,
    )
  | AddWrittenStep(RewriteChecker.trace_summary, int, Exp.t, Exp.t)
  | CoqExport;

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_focus =
  | SingleStep(SingleStep.focus'(step_focus))
  | InductionStep(InductionStep.focus'(step_focus))
  | ForallStep(ForallStep.focus'(step_focus))
  | MissingStep(MissingStep.Selection.t)
  | AxiomStep(AxiomStep.focus'(step_focus))
  | AlgebriteStep(AlgebriteStep.focus'(step_focus))
  | WrittenStep(WrittenStep.focus'(step_focus))

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
  let calculate_with_level:
    (
      ~rewrite_level: Axioms.rewrite_level,
      ~settings: Calc.t(CoreSettings.t),
      ~hidden: Calc.saved(bool),
      ~exp: Calc.t(Exp.t),
      ~ctx: Calc.t(SemanticCtx.t),
      ~editor: Calc.t(CodeSelectable.Model.t),
      ~info_map: Calc.t(Statics.Map.t),
      ~ana: Calc.t(Typ.t),
      step_kind_model
    ) =>
    option(
      (
        step_kind_model,
        Calc.t(bool),
        option(Calc.t(Exp.t)),
        Calc.t(option(bool)),
      ),
    );
} = {
  /* The StepKind code here is almost all dispatch to the
     individual step modules. */

  module SingleStep = SingleStep.F(Stepper);
  module InductionStep = InductionStep.F(Stepper);
  module ForallStep = ForallStep.F(Stepper);
  module MissingStep = MissingStep; // This could be functorized too.
  module AxiomStep = AxiomStep.F(Stepper);
  module AlgebriteStep = AlgebriteStep.F(Stepper);
  module WrittenStep = WrittenStep.F(Stepper);

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
    | WrittenStep(m) => WrittenStep(WrittenStep.persist(m))
    | ReparenthesizeStep({original_exp, reparenthesized_exp, _}) =>
      ReparenthesizeStep(original_exp, reparenthesized_exp)
    | AutoSimplifyStep({original_exp, simplified_exp, _}) =>
      AutoSimplifyStep(original_exp, simplified_exp)
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
    | WrittenStep(m) => WrittenStep(WrittenStep.unpersist(m))
    | ReparenthesizeStep(original_exp, reparenthesized_exp) =>
      ReparenthesizeStep({
        original_exp,
        reparenthesized_exp,
        selected_id: None,
        evaluate_after_parenthesize: false,
        next_exp: Calc.Pending,
      })
    | AutoSimplifyStep(original_exp, simplified_exp) =>
      AutoSimplifyStep({
        original_exp,
        simplified_exp,
        next_exp: Calc.Pending,
      })
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
      | (WrittenStep(a), WrittenStep(m)) =>
        let* s = WrittenStep.update(~settings, a, m);
        (WrittenStep(s): model);
      | (
          SingleStep(_) | InductionStep(_) | ForallStep(_) | MissingStep(_) |
          AxiomStep(_) |
          AlgebriteStep(_) |
          WrittenStep(_),
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
    | WrittenStep(action) => WrittenStep.can_undo(action)
    };
  };

  let rec calculate_with_level =
          (
            ~rewrite_level: Axioms.rewrite_level,
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
        calculate_with_level(
          ~rewrite_level,
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
              ~rewrite_level,
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
    | WrittenStep(m) =>
      let+ (m, h, e, v) =
        WrittenStep.calculate(
          ~settings,
          ~hidden,
          ~exp,
          ~ctx,
          ~editor,
          ~info_map,
          ~ana,
          m,
        );
      (WrittenStep(m): model, h, e, v);
    | ReparenthesizeStep({
        original_exp,
        reparenthesized_exp,
        selected_id,
        evaluate_after_parenthesize,
        _,
      }) =>
      let current_exp = exp |> Calc.get_value;
      if (!DHExp.fast_equal(current_exp, original_exp)) {
        None; /* upstream changed; fall back to MissingStep */
      } else {
        let next_exp =
          if (evaluate_after_parenthesize) {
            let steps =
              switch (
                EvaluatorStep.get_status(
                  ~settings=Calc.get_value(settings),
                  reparenthesized_exp,
                  Calc.get_value(ctx) |> SemanticCtx.get_env,
                )
              ) {
              | AutoStep(step) => [step]
              | AvailableSteps(steps) => steps
              };
            let matching_step =
              switch (selected_id) {
              | None => None
              | Some(selected_id) =>
                steps
                |> List.find_opt(step =>
                     switch (
                       EvaluatorStep.get_step_id_in(step, reparenthesized_exp)
                     ) {
                     | Some(id) => id == selected_id
                     | None => EvaluatorStep.get_step_id(step) == selected_id
                     }
                   )
              };
            switch (matching_step) {
            | Some(step) =>
              EvaluatorStep.take_step(step)
              |> Option.value(~default=reparenthesized_exp)
            | None => reparenthesized_exp
            };
          } else {
            reparenthesized_exp;
          };
        Some((
          ReparenthesizeStep({
            original_exp,
            reparenthesized_exp,
            selected_id,
            evaluate_after_parenthesize,
            next_exp: Calc.Calculated(next_exp),
          }): model,
          Calc.set(false, hidden),
          Some(Calc.NewValue(next_exp)),
          Calc.OldValue(None),
        ));
      };
    | AutoSimplifyStep({original_exp, simplified_exp, _}) =>
      let current_exp = exp |> Calc.get_value;
      if (!DHExp.fast_equal(current_exp, original_exp)) {
        None;
      } else {
        Some((
          AutoSimplifyStep({
            original_exp,
            simplified_exp,
            next_exp: Calc.Calculated(simplified_exp),
          }): model,
          Calc.set(false, hidden),
          Some(Calc.NewValue(simplified_exp)),
          Calc.OldValue(None),
        ));
      };
    };

  let calculate =
      (~settings, ~hidden, ~exp, ~ctx, ~editor, ~info_map, ~ana, model) =>
    calculate_with_level(
      ~rewrite_level=Axioms.Arithmetic,
      ~settings,
      ~hidden,
      ~exp,
      ~ctx,
      ~editor,
      ~info_map,
      ~ana,
      model,
    );

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
      | (WrittenStep(focus), WrittenStep(model)) =>
        let+ focus_info =
          WrittenStep.get_cursor_info(
            ~inject=x => inject(WrittenStep(x): action),
            ~focus,
            model,
          );
        (WrittenStep(focus_info): action);
      | (_, ReparenthesizeStep(_))
      | (_, AutoSimplifyStep(_)) => Cursor.empty
      | (
          SingleStep(_) | InductionStep(_) | ForallStep(_) | MissingStep(_) |
          AxiomStep(_) |
          AlgebriteStep(_) |
          WrittenStep(_),
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
      | MissingStep(_)
      | ReparenthesizeStep(_)
      | AutoSimplifyStep(_) => (
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
      | WrittenStep(m) =>
        WrittenStep.view_content(
          ~focus=
            switch (focus) {
            | Some(WrittenStep(f)) => Some(f)
            | _ => None
            },
          ~inject=x => inject(WrittenStep(x)),
          ~take_focus=x => take_focus(WrittenStep(x)),
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
    | ReparenthesizeStep(_) => WebUtil.Node.text("reparenthesize")
    | AutoSimplifyStep(_) => WebUtil.Node.text("auto simplify")
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
    | WrittenStep(m) =>
      WrittenStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(WrittenStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(WrittenStep(x)),
        ~take_focus=x => take_focus(WrittenStep(x)),
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
  let calculate_with_level:
    (
      ~rewrite_level: Axioms.rewrite_level,
      ~settings: Calc.t(CoreSettings.t),
      ~exp: Calc.t(Exp.t),
      ~ctx: Calc.t(SemanticCtx.t),
      ~ana: Calc.t(Typ.t),
      step_model
    ) =>
    (step_model, Calc.t(Exp.t), Calc.t(option(bool)));
  let view_with_automation:
    (
      ~globals: Globals.t,
      ~take_focus: step_focus => Ui_effect.t(unit),
      ~inject: step_action => Ui_effect.t(unit),
      ~hide_stepper: Ui_effect.t(unit),
      ~focus: option(step_focus),
      ~automation_stage: Axioms.automation_stage,
      ~is_toplevel: bool,
      step_model
    ) =>
    list(WebUtil.Node.t);
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
    export_warning: None,
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
      export_warning: None,
    };
  };

  let get_validity = (model: model) =>
    model.proof_validity
    |> Calc.get_saved_exc(
         ~print="get_validity called before calculate on stepper",
       );

  let coq_tactic_for_axiom = name =>
    switch (name) {
    | "Iden(+)L" => "rewrite Z.add_0_l"
    | "Iden(+)R" => "rewrite Z.add_0_r"
    | "Iden(*)L" => "rewrite Z.mul_1_l"
    | "Iden(*)R" => "rewrite Z.mul_1_r"
    | "Zero(*)L" => "rewrite Z.mul_0_l"
    | "Zero(*)R" => "rewrite Z.mul_0_r"
    | "Comm(+)" => "rewrite Z.add_comm"
    | "Assoc(+)" => "rewrite Z.add_assoc"
    | "Comm(*)" => "rewrite Z.mul_comm"
    | "Assoc(*)" => "rewrite Z.mul_assoc"
    | _ => "cbv"
    };

  let single_step_export = (ind, step: step_model, forall_str) => {
    let old_fragment = CoqExport.string_of_d(step.expr |> Calc.get_saved_exc);
    switch (step.next_step) {
    | Some(next) =>
      let new_fragment =
        CoqExport.string_of_d(next.expr |> Calc.get_saved_exc);
      let old_expr = CoqExport.string_of_d(step.expr |> Calc.get_saved_exc);
      let new_expr = CoqExport.string_of_d(next.expr |> Calc.get_saved_exc);
      let rewrite_index =
        switch (step.step_kind) {
        | SingleStep(single_step) =>
          CoqExport.index_of_like_terms(
            EvaluatorStep.get_step_ctx(
              single_step.evalobj |> Calc.get_saved_exc,
            ),
            next.expr |> Calc.get_saved_exc,
          )
        | _ => 1
        };
      Printf.sprintf(
        "Lemma equiv_exp%d:%s%s = %s.\nProof.\nintros.\ncut (%s=%s).\n- intros. rewrite <- H at %d. reflexivity.\n- intros. ring.\nQed.",
        ind,
        forall_str,
        new_expr,
        old_expr,
        old_fragment,
        new_fragment,
        rewrite_index,
      );
    | None => ""
    };
  };

  let rec coq_export_steps = (step: step_model) =>
    switch (step.next_step) {
    | None => []
    | Some(next_step) =>
      switch (Calc.get_saved_exc(step.expr) |> Exp.term_of) {
      | Fun(_, _, _, _) => coq_export_steps(next_step)
      | _ => [step, ...coq_export_steps(next_step)]
      }
    };

  let export_coq = (first_step: step_model): option(string) => {
    let steps = coq_export_steps(first_step);
    if (List.length(steps) == 0) {
      None;
    } else {
      let first_exp = Calc.get_saved_exc(List.nth(steps, 0).expr);
      let unique_vars = CoqExport.unique_vars_in_ast(first_exp);
      let forall_str =
        switch (unique_vars) {
        | [] => ""
        | vars => "forall " ++ String.concat(" ", vars) ++ ","
        };
      let lemmas_and_invocations =
        List.mapi(
          (ind, step) =>
            (
              single_step_export(List.length(steps) - ind, step, forall_str),
              Printf.sprintf(
                "rewrite <- equiv_exp%d.",
                List.length(steps) - ind,
              ),
            ),
          steps,
        );
      let (lemmas, invocations) = List.split(lemmas_and_invocations);
      let first_expr = CoqExport.string_of_d(first_exp);
      let last_step = List.nth(steps, List.length(steps) - 1);
      switch (last_step.next_step) {
      | Some(next) =>
        let final_expr =
          CoqExport.string_of_d(next.expr |> Calc.get_saved_exc);
        Some(
          Printf.sprintf(
            "From Stdlib Require Import ZArith Lia.\nOpen Scope Z_scope.\n%s\nTheorem equiv_exp:%s%s=%s.\nProof.\nintros.\n%s\nreflexivity. Qed.",
            String.concat("\n", lemmas),
            forall_str,
            final_expr,
            first_expr,
            String.concat("\n", invocations),
          ),
        );
      | None => None
      };
    };
  };

  let rec update =
          (~settings, action: step_action, model: step_model)
          : Updated.t(step_model) => {
    Updated.(
      switch (action, model.step_kind, model.next_step) {
      | (CoqExport, _, _) =>
        switch (export_coq(model)) {
        | Some(coq_data) =>
          JsUtil.download_string_file(
            ~filename="stepper_coq_export.v",
            ~content_type="text/plain",
            ~contents=coq_data,
          );
          {
            ...model,
            export_warning: None,
          }
          |> return_quiet;
        | None =>
          {
            ...model,
            export_warning:
              Some("Cannot export Coq proof: this proof has no steps."),
          }
          |> return_quiet
        }
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
      | (
          StepForwardOnSelection(selected_ids, evaluate_after_parenthesize),
          MissingStep(_),
          _,
        ) =>
        let exp =
          model.expr |> Calc.get_saved_exc(~print="StepForwardOnSelection");
        let visible_exp =
          (
            model.editor |> Calc.get_saved_exc(~print="StepForwardOnSelection")
          ).
            statics.
            term;
        let result =
          Reparenthesize.reparenthesize_selection(~selected_ids, visible_exp)
          |> Option.map((result: Reparenthesize.result) => {
               let new_exp = result.exp;
               let selected_id = result.selected_id;
               {
                 ...model,
                 step_kind:
                   ReparenthesizeStep({
                     original_exp: exp,
                     reparenthesized_exp: new_exp,
                     selected_id: Some(selected_id),
                     evaluate_after_parenthesize,
                     next_exp: Calc.Pending,
                   }),
               };
             });
        switch (result) {
        | Some(m) => m |> return
        | None => model |> raise_invalid_action
        };
      | (StepForwardOnSelection(_, _), _, _) => model |> raise_invalid_action
      | (
          AutoSimplifySelection(original_exp, simplified_exp),
          MissingStep(_),
          _,
        ) =>
        {
          ...model,
          step_kind:
            AutoSimplifyStep({
              original_exp,
              simplified_exp,
              next_exp: Calc.Pending,
            }),
        }
        |> return
      | (AutoSimplifySelection(_, _), _, _) => model |> raise_invalid_action
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
      | (
          AddReparenthesizedAlgebriteStep(
            reparenthesized_exp,
            at_exp,
            with_exp,
          ),
          MissingStep(_),
          _,
        ) =>
        let exp =
          model.expr
          |> Calc.get_saved_exc(~print="AddReparenthesizedAlgebriteStep");
        {
          ...model,
          step_kind:
            ReparenthesizeStep({
              original_exp: exp,
              reparenthesized_exp,
              selected_id: None,
              evaluate_after_parenthesize: false,
              next_exp: Calc.Pending,
            }),
          next_step:
            Some({
              ...init,
              step_kind:
                AlgebriteStep({
                  at_idx:
                    try(ProofHacks.exp_idx(at_exp, reparenthesized_exp)) {
                    | _ => 0
                    },
                  at_exp,
                  with_exp,
                  next_exp: Calc.Pending,
                }),
            }),
        }
        |> return;
      | (AddReparenthesizedAlgebriteStep(_, _, _), _, _) =>
        model |> raise_invalid_action
      | (
          AddReparenthesizedWrittenStep(
            trace_summary,
            reparenthesized_exp,
            at_exp,
            with_exp,
          ),
          MissingStep(_),
          _,
        ) =>
        let exp =
          model.expr
          |> Calc.get_saved_exc(~print="AddReparenthesizedWrittenStep");
        {
          ...model,
          step_kind:
            ReparenthesizeStep({
              original_exp: exp,
              reparenthesized_exp,
              selected_id: None,
              evaluate_after_parenthesize: false,
              next_exp: Calc.Pending,
            }),
          next_step:
            Some({
              ...init,
              step_kind:
                WrittenStep({
                  at_idx:
                    try(ProofHacks.exp_idx(at_exp, reparenthesized_exp)) {
                    | _ => 0
                    },
                  at_exp,
                  with_exp,
                  justification:
                    RewriteChecker.trace_summary_label(trace_summary),
                  trace_summary: Some(trace_summary),
                  next_exp: Calc.Pending,
                }),
            }),
        }
        |> return;
      | (AddReparenthesizedWrittenStep(_, _, _, _), _, _) =>
        model |> raise_invalid_action
      | (
          AddWrittenStep(trace_summary, at_idx, at_exp, with_exp),
          MissingStep(_),
          _,
        ) =>
        {
          ...model,
          step_kind:
            WrittenStep({
              at_idx,
              at_exp,
              with_exp,
              justification:
                RewriteChecker.trace_summary_label(trace_summary),
              trace_summary: Some(trace_summary),
              next_exp: Calc.Pending,
            }),
        }
        |> return
      | (AddWrittenStep(_, _, _, _), _, _) => model |> raise_invalid_action
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
    | StepForwardOnSelection(_, _) => true
    | AutoSimplifySelection(_, _) => true
    | AddInduction(_) => true
    | AddForall => true
    | AddAxiomStep(_) => true
    | AddAlgebriteStep(_) => true
    | AddReparenthesizedAlgebriteStep(_) => true
    | AddReparenthesizedWrittenStep(_) => true
    | AddWrittenStep(_) => true
    | CoqExport => false
    | StepKindAction(action) => StepKind.can_undo(action)
    };
  };

  let rec calculate_with_level =
          (
            ~rewrite_level: Axioms.rewrite_level,
            ~settings: Calc.t(CoreSettings.t),
            ~exp as expr: Calc.t(Exp.t),
            ~ctx: Calc.t(SemanticCtx.t),
            ~ana: Calc.t(Typ.t),
            {
              expr: prev_expr,
              editor,
              step_kind,
              next_step,
              hidden,
              proof_validity,
              editor_info_map: info_map,
              export_warning,
            }: step_model,
          )
          : (step_model, Calc.t(Exp.t), Calc.t(option(bool))) => {
    let expr_changed =
      switch (prev_expr) {
      | Calc.Pending => true
      | Calc.Calculated(prev_expr) =>
        !Exp.fast_equal(prev_expr, Calc.get_value(expr))
      };
    let editor =
      switch (editor, expr_changed) {
      | (Calc.Calculated(editor), false) => Calc.OldValue(editor)
      | _ =>
        editor
        |> {
          let.calc settings = settings
          and.calc expr = expr
          and.calc _ctx = ctx
          and.calc _ana = ana;
          expr
          |> CodeWithStatics.Model.mk_from_exp(
               ~settings,
               ~root=Exp,
               ~parenthesization=Haz3lcore.ExpToSegment.Settings.Defensive,
             );
        }
      };
    // TODO: Make editor calculation more incremental
    let editor =
      CodeSelectable.Update.calculate(
        ~is_dynamic_term=true,
        ~settings=Calc.get_value(settings),
        ~is_edited=expr_changed,
        ~ctx=Calc.get_value(ctx) |> SemanticCtx.get_ctx,
        ~dynamics=Dynamics.Map.empty,
        ~ana=Calc.get_value(ana),
        ~stitch=_ => Calc.get_value(expr),
        Calc.get_value(editor),
      );
    let info_map = Calc.NewValue(editor.statics.info_map);
    let editor = Calc.OldValue(editor);
    let (step_kind, hidden, next_expr, inner_validity) =
      StepKind.calculate_with_level(
        ~rewrite_level,
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
           |> StepKind.calculate_with_level(
                ~rewrite_level,
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
          calculate_with_level(
            ~rewrite_level,
            ~settings,
            ~exp=next_expr,
            ~ctx,
            ~ana,
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

    (
      {
        expr: expr |> Calc.save,

        editor: editor |> Calc.save,
        step_kind,
        next_step,
        hidden: hidden |> Calc.save,
        proof_validity: proof_validity |> Calc.save,
        editor_info_map: info_map |> Calc.save,
        export_warning,
      },
      last_expr,
      proof_validity,
    );
  };

  let calculate = (~settings, ~exp, ~ctx, ~ana, model) =>
    calculate_with_level(
      ~rewrite_level=Axioms.Arithmetic,
      ~settings,
      ~exp,
      ~ctx,
      ~ana,
      model,
    );

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
            ~automation_stage: Axioms.automation_stage,
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
        let rendered_expr =
          (model.editor |> Calc.get_saved_exc(~print="editor")).statics.term;
        let step_id_in_expr = step =>
          EvaluatorStep.get_step_id_in(step, rendered_expr)
          |> Option.value(~default=EvaluatorStep.get_step_id(step));
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
            |> List.map(step_id_in_expr)
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
                      try(
                        ProofHacks.exp_idx(
                          from_exp,
                          model.expr |> Calc.get_saved_exc(~print="expr"),
                        )
                      ) {
                      | _ => 0
                      },
                      from_exp,
                      Direction.Right,
                      "Reflexive(==)",
                    ),
                  );
                },
            ~inject=x => inject(EditorAction(x)),
            ~selected=
              switch (focus, model.step_kind) {
              | (Some(Here(_)), _) => true
              | _ => false
              },
            ~selected_id=selected_exp |> Option.map(Exp.rep_id),
            ~overlays=
              switch (model.step_kind) {
              | MissingStep(m)
                  when
                    globals.settings.core.evaluation.enable_proof
                    || globals.settings.core.evaluation.write_out_steps =>
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
                  ~automation_stage,
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
                    | AddReparenthesizedAlgebriteStep(e1, e2, e3) =>
                      inject(AddReparenthesizedAlgebriteStep(e1, e2, e3))
                    | AddReparenthesizedWrittenStep(just, e1, e2, e3) =>
                      inject(AddReparenthesizedWrittenStep(just, e1, e2, e3))
                    | AddWrittenStep(just, idx, e1, e2) =>
                      inject(AddWrittenStep(just, idx, e1, e2))
                    | AutoSimplify(original_exp, simplified_exp) =>
                      inject(
                        AutoSimplifySelection(original_exp, simplified_exp),
                      )
                    | TakeStep(i) => inject(StepForward(i))
                    | StepHere(ids, evaluate_after_parenthesize) =>
                      inject(
                        StepForwardOnSelection(
                          ids,
                          evaluate_after_parenthesize,
                        ),
                      )
                    | Refl(i) => {
                        let refl_exps =
                          switch (model.step_kind) {
                          | MissingStep(m) =>
                            m.refls |> Calc.get_saved_exc(~print="refls")
                          | _ => []
                          };
                        let from_exp = List.nth(refl_exps, i);
                        inject(
                          AddAxiomStep(
                            "reflexivity",
                            try(
                              ProofHacks.exp_idx(
                                from_exp,
                                model.expr
                                |> Calc.get_saved_exc(~print="expr"),
                              )
                            ) {
                            | _ => 0
                            },
                            from_exp,
                            Direction.Right,
                            "Reflexive(==)",
                          ),
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
          ~automation_stage,
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

  let view_with_automation =
      (
        ~globals: Globals.t,
        ~take_focus: step_focus => Ui_effect.t(unit),
        ~inject: step_action => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~focus: option(step_focus),
        ~automation_stage: Axioms.automation_stage,
        ~is_toplevel: bool,
        root_step,
      ) => {
    let export_controls = [
      WebUtil.div_c(
        "stepper-export-controls",
        [
          Widgets.button(
            Icons.export,
            _ => inject(CoqExport),
            ~tooltip="Export steps as Coq proof",
          ),
        ]
        @ (
          switch (root_step.export_warning) {
          | Some(message) => [
              WebUtil.div_c(
                "stepper-export-warning",
                [WebUtil.Node.text(message)],
              ),
            ]
          | None => []
          }
        ),
      ),
    ];
    WebUtil.[
      Node.div(
        ~attrs=[Attr.classes(["stepper", "cell-result"])],
        view_step(
          ~globals,
          ~take_focus,
          ~hide_stepper,
          ~inject,
          ~focus,
          ~automation_stage,
          ~is_toplevel,
          ~undo=None,
          root_step,
        )
        @ export_controls,
      ),
    ];
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
      ) =>
    view_with_automation(
      ~globals,
      ~take_focus,
      ~inject,
      ~hide_stepper,
      ~focus,
      ~automation_stage=Axioms.MultiStepCheck,
      ~is_toplevel,
      root_step,
    );
};
