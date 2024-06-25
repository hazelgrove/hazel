open Util;
open Haz3lcore;
open Sexplib.Std;

module rec Stepper: {
  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;
    let init: unit => t;
    let get_next_steps:
      t => list((FilterAction.action, EvaluatorStep.EvalObj.t));
    let get_elaboration: t => option(Exp.t);
    let get_state: t => EvaluatorState.t;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type persistent;
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;
    let update: (~settings: Settings.t, t, Model.t) => Updated.t(Model.t);
    let calculate: (~settings: CoreSettings.t, Exp.t, Model.t) => Model.t;
  };

  module Selection: {
    open Cursor;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = (int, Step.Selection.t);

    let get_cursor_info: (~selection: t, Model.t) => cursor(Update.t);

    let handle_key_event:
      (~selection: t, ~event: Key.t, Model.t) => option(Update.t);
  };

  module View: {
    type event =
      | MakeActive(Selection.t)
      | HideStepper
      | JumpTo(Haz3lcore.Id.t);

    let view:
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Selection.t),
        ~read_only: bool,
        Model.t
      ) =>
      list(Web.Node.t);
  };
} = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type b = Step.Model.t
    and a = {
      // Calculated:
      expr: Exp.t,
      state: EvaluatorState.t,
      editor: CodeWithStatics.Model.t,
    }
    and t = {
      history: Aba.t(option(a), b), // None means pending
      // Calculated:
      cached_settings: option(CoreSettings.t),
      next_steps: list((FilterAction.action, EvaluatorStep.EvalObj.t)),
    };

    let init = () => {
      history: Aba.singleton(None),
      cached_settings: None,
      next_steps: [],
    };

    let get_next_steps = (model: t) => model.next_steps;

    let get_elaboration = (model: t): option(Exp.t) =>
      model.history |> Aba.last_a |> Option.map(({expr, _}) => expr);

    let get_state = (model: t): EvaluatorState.t =>
      model.history
      |> Aba.hd
      |> Option.map(({state, _}) => state)
      |> Option.value(~default=EvaluatorState.init);

    let can_undo = (model: t) => {
      model.history
      |> Aba.get_bs
      |> List.exists((b: b) =>
           switch (b) {
           | EvalStep({hidden, _}) => !hidden
           | InductionStep(_) => true
           }
         );
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type persistent = list(Haz3lcore.EvaluatorStep.EvalObj.persistent);
  };

  module Update = {
    open Updated;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | StepForward(int)
      | AddInduction
      | StepBackward
      | StepAction(int, Step.Update.t);
    // | Induct

    let update = (~settings, action: t, model: Model.t): Updated.t(Model.t) => {
      switch (action) {
      | StepForward(idx) =>
        {
          ...model,
          history:
            Aba.cons(
              None,
              Step.Model.EvalStep(
                EvalStep.Model.mk({
                  old_id:
                    model
                    |> Model.get_next_steps
                    |> List.nth(_, idx)
                    |> snd
                    |> ((x: EvaluatorStep.EvalObj.t) => x.d_loc |> Exp.rep_id),
                  new_id: Id.mk(),
                }),
              ),
              model.history,
            ),
        }
        |> Updated.return
      | StepBackward =>
        {
          ...model,
          history: {
            let rec step_backward:
              Aba.t(option(Model.a), Model.b) =>
              Aba.t(option(Model.a), Model.b) = (
              fun
              | ([_, ...as_], [EvalStep({hidden: true, _}), ...bs]) =>
                (as_, bs) |> step_backward
              | ([_, ...as_], [_, ...bs]) => (as_, bs)
              | x => x
            );
            step_backward(model.history);
          },
        }
        |> Updated.return
      | AddInduction =>
        {
          ...model,
          history:
            Aba.cons(
              None,
              Step.Model.InductionStep(InductionStep.Model.init),
              model.history,
            ),
        }
        |> Updated.return
      | StepAction(n, a) =>
        let b = List.nth(model.history |> Aba.get_bs, n);
        let* b = Step.Update.update(~settings, a, b);
        let history =
          Aba.mk(
            model.history |> Aba.get_as,
            Util.ListUtil.put_nth(n, b, model.history |> Aba.get_bs),
          );
        {...model, history};
      };
    };

    let calc_b = Step.Update.calculate;

    let get_next_a = (~settings, b: Model.b): Model.a => {
      let expr = Step.Model.get_next_expr(b);
      let state = Step.Model.get_next_state(b);
      let editor =
        CodeWithStatics.Model.mk_from_exp(expr)
        |> CodeWithStatics.Update.calculate(~settings, ~stitch=x => x);
      Model.{expr, state, editor};
    };

    let eval_steps_of = (~settings, expr, state) => {
      EvaluatorStep.decompose(expr, state)
      |> List.map(EvaluatorStep.should_hide_eval_obj(~settings));
    };

    let take_hidden_step = (~settings, model: Model.t) => {
      let hidden_steps =
        List.filter(
          fun
          | (FilterAction.Eval, _) => true
          | (FilterAction.Step, _) => false,
          model.next_steps,
        );
      switch (hidden_steps) {
      | [] => None
      | [x, ..._] =>
        let next_b =
          Step.Model.EvalStep(
            EvalStep.Model.mk(EvaluatorStep.EvalObj.persist(x |> snd)),
          );
        let next_a = get_next_a(~settings, next_b);
        Some((next_a, next_b));
      };
    };

    let rec take_hidden_steps = (~settings, expr, state, model) => {
      switch (take_hidden_step(~settings, model)) {
      | None => model
      | Some((a, b)) =>
        Model.{
          history: Aba.cons(Some(a), b, model.history),
          cached_settings: Some(settings),
          next_steps:
            eval_steps_of(~settings=settings.evaluation, expr, state),
        }
        |> take_hidden_steps(~settings, a.expr, a.state)
      };
    };

    let full_calculate =
        (~settings: CoreSettings.t, elab: Exp.t, model: Model.t): Model.t => {
      Aba.fold_right(
        (_, b: Model.b, (aba, expr, state)) => {
          let options =
            eval_steps_of(~settings=settings.evaluation, expr, state);
          let next_b =
            calc_b(~settings, ~prev_expr=expr, ~state, ~options, b);
          let next_a = get_next_a(~settings, next_b);
          (Aba.cons(Some(next_a), next_b, aba), next_a.expr, next_a.state);
        },
        _ =>
          (
            Some(
              Model.{
                expr: elab,
                state: EvaluatorState.init,
                editor: CodeWithStatics.Model.mk_from_exp(elab),
              },
            )
            |> Aba.singleton,
            elab,
            EvaluatorState.init,
          ),
        model.history,
      )
      |> (
        ((aba, e, s)) =>
          take_hidden_steps(
            ~settings,
            e,
            s,
            {
              history: aba,
              cached_settings: Some(settings),
              next_steps: eval_steps_of(~settings=settings.evaluation, e, s),
            },
          )
      );
    };

    // TODO[Matt]: faster calculation
    // let calculate_pending = (~settings, elab: Exp.t) => {};

    let calculate = (~settings, elab: Exp.t) => {
      full_calculate(~settings, elab);
    };
  };

  module Selection = {
    open Cursor;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = (int, Step.Selection.t);

    let get_cursor_info = (~selection: t, model: Model.t): cursor(Update.t) => {
      let step = model.history |> Aba.get_bs |> List.nth(_, selection |> fst);
      let+ ci =
        Step.Selection.get_cursor_info(~selection=selection |> snd, step);
      Update.StepAction(selection |> fst, ci);
    };

    let handle_key_event =
        (~selection: t, ~event, model: Model.t): option(Update.t) =>
      model.history
      |> Aba.get_bs
      |> List.nth(_, selection |> fst)
      |> Step.Selection.handle_key_event(~selection=selection |> snd, ~event)
      |> Option.map(x => Update.StepAction(selection |> fst, x));
  };

  module View = {
    open Virtual_dom.Vdom;
    open Node;

    type event =
      | MakeActive(Selection.t)
      | HideStepper
      | JumpTo(Haz3lcore.Id.t);

    let view =
        (
          ~globals as {settings, inject_global, _} as globals: Globals.t,
          ~signal: event => Ui_effect.t(unit),
          ~inject: Update.t => Ui_effect.t(unit),
          ~selected: option(Selection.t),
          ~read_only: bool,
          stepper: Model.t,
        ) => {
      let button_back =
        Widgets.button_d(
          Icons.undo,
          inject(StepBackward),
          ~disabled=!Model.can_undo(stepper),
          ~tooltip="Step Backwards",
        );
      let button_induction =
        Widgets.button_d(
          Icons.star,
          inject(AddInduction),
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
          settings.core.evaluation.stepper_history,
          _ =>
          inject_global(Set(Evaluation(ShowRecord)))
        );
      let eval_settings =
        Widgets.button(Icons.gear, _ =>
          inject_global(Set(Evaluation(ShowSettings)))
        );
      let previous_steps = {
        stepper.history
        |> Aba.aba_triples
        |> List.mapi((i, x) => (i, x))
        |> (settings.core.evaluation.stepper_history ? x => x : (_ => []))
        |> (
          settings.core.evaluation.show_hidden_steps
            ? x => x
            : List.filter(((_, (_, b: Model.b, _))) =>
                !Step.View.is_hidden(b)
              )
        )
        |> List.map(((i, (_, b: Model.b, a: option(Model.a)))) =>
             switch (a) {
             | Some(a) =>
               [
                 div(
                   ~attr=
                     Attr.classes(
                       ["cell-item", "cell-result"]
                       @ (Step.View.is_hidden(b) ? ["hidden"] : []),
                     ),
                   [
                     div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
                     StepperEditor.Stepped.view(
                       ~globals,
                       ~overlays=[],
                       {editor: a.editor, step_id: Step.View.stepped_id(b)},
                     ),
                     div(
                       ~attr=Attr.classes(["stepper-justification"]),
                       [Step.View.get_text(b)],
                     ),
                   ],
                 ),
               ]
               @ Step.View.view(
                   ~globals,
                   ~selection=
                     switch (selected) {
                     | Some((i', x)) when i' == i => Some(x)
                     | _ => None
                     },
                   ~signal=(MakeActive(x)) => signal(MakeActive((i, x))),
                   ~inject=a => inject(StepAction(i, a)),
                   b,
                 )
             | None => [
                 div(~attr=Attr.class_("cell-item"), [text("...")]),
               ]
             }
           )
        |> List.rev
        |> List.flatten;
      };
      let current_step = {
        let model = stepper.history |> Aba.hd;
        div(
          ~attr=Attr.classes(["cell-item", "cell-result"]),
          (
            switch (model) {
            | Some(model) => [
                div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
                StepperEditor.Steppable.view(
                  ~globals,
                  ~signal=(TakeStep(x)) => inject(Update.StepForward(x)),
                  ~overlays=[],
                  {
                    editor: model.editor,
                    next_steps:
                      List.map(
                        ((_, eo: EvaluatorStep.EvalObj.t)) =>
                          eo.d_loc |> Exp.rep_id,
                        stepper.next_steps,
                      ),
                  },
                ),
              ]
            | None => [div(~attr=Attr.class_("cell-item"), [text("...")])]
            }
          )
          @ (
            read_only
              ? []
              : [
                button_back,
                button_induction,
                eval_settings,
                toggle_show_history,
                button_hide_stepper,
              ]
          ),
        );
      };
      let settings_modal =
        settings.core.evaluation.show_settings
          ? SettingsModal.view(
              ~inject=u => inject_global(Set(u)),
              settings.core.evaluation,
            )
          : [];
      previous_steps @ [current_step] @ settings_modal;
    };
  };
} /* ope*/

and Step: {
  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | EvalStep(EvalStep.Model.t)
      | InductionStep(InductionStep.Model.t);

    let get_next_expr: t => Exp.t;
    let get_next_state: t => EvaluatorState.t;
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;
    let update: (~settings: Settings.t, t, Model.t) => Updated.t(Model.t);
    let calculate:
      (
        ~settings: CoreSettings.t,
        ~prev_expr: Exp.t,
        ~options: list((FilterAction.action, EvaluatorStep.EvalObj.t)),
        ~state: EvaluatorState.t,
        Model.t
      ) =>
      Model.t;
  };

  module Selection: {
    open Cursor;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | EvalStep
      | InductionStep(InductionStep.Selection.t);

    let get_cursor_info: (~selection: t, Model.t) => cursor(Update.t);

    let handle_key_event:
      (~selection: t, ~event: Key.t, Model.t) => option(Update.t);
  };

  module View: {
    type signal =
      | MakeActive(Selection.t);
    let is_hidden: Model.t => bool;
    let stepped_id: Model.t => option(Haz3lcore.Id.t);
    let view:
      (
        ~globals: Globals.t,
        ~selection: option(Selection.t),
        ~signal: signal => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        Model.t
      ) =>
      list(Web.Node.t);
    let get_text: Model.t => Web.Node.t;
  };
} = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | EvalStep(EvalStep.Model.t)
      | InductionStep(InductionStep.Model.t);

    let get_next_expr = (model: t): Exp.t =>
      switch (model) {
      | EvalStep(m) => m |> EvalStep.Model.get_next_expr
      | InductionStep(m) => m |> InductionStep.Model.get_next_expr
      };

    let get_next_state = (model: t): EvaluatorState.t =>
      switch (model) {
      | EvalStep(m) => m |> EvalStep.Model.get_next_state
      | InductionStep(m) => m |> InductionStep.Model.get_next_state
      };
  };

  module Update = {
    open Updated;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | EvalStep(EvalStep.Update.t)
      | InductionStep(InductionStep.Update.t);

    let update = (~settings, update, model: Model.t): Updated.t(Model.t) => {
      switch (model, update) {
      | (EvalStep(m), EvalStep(u)) =>
        let* m = EvalStep.Update.update(~settings, u, m);
        Model.EvalStep(m);
      | (InductionStep(m), InductionStep(u)) =>
        let* m = InductionStep.Update.update(~settings, u, m);
        Model.InductionStep(m);
      | _ => model |> Updated.return_quiet
      };
    };

    let calculate =
        (~settings, ~prev_expr, ~options, ~state): (Model.t => Model.t) =>
      fun
      | EvalStep(m) =>
        EvalStep(
          EvalStep.Update.calculate(
            ~settings,
            m,
            ~prev_expr,
            ~options,
            ~state,
          ),
        )
      | InductionStep(m) => {
          InductionStep(
            InductionStep.Update.calculate(~settings, m, ~prev_expr, ~state),
          );
        }; // TODO[Matt]: Calculate
  };

  module Selection = {
    open Cursor;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | EvalStep
      | InductionStep(InductionStep.Selection.t);

    let get_cursor_info = (~selection: t, model: Model.t): cursor(Update.t) => {
      switch (selection, model) {
      | (EvalStep, EvalStep(_)) => Cursor.empty
      | (EvalStep, _) => Cursor.empty
      | (InductionStep(s), InductionStep(m)) =>
        let+ ci = InductionStep.Selection.get_cursor_info(~selection=s, m);
        Update.InductionStep(ci);
      | (InductionStep(_), _) => Cursor.empty
      };
    };

    let handle_key_event =
        (~selection: t, ~event, model: Model.t): option(Update.t) =>
      switch (selection, model) {
      | (EvalStep, _) => None
      | (InductionStep(s), InductionStep(model)) =>
        InductionStep.Selection.handle_key_event(~selection=s, ~event, model)
        |> Option.map(x => Update.InductionStep(x))
      | (InductionStep(_), _) => None
      };
  };

  module View = {
    type signal =
      | MakeActive(Selection.t);

    let is_hidden: Model.t => bool =
      fun
      | EvalStep(m) => EvalStep.View.is_hidden(m)
      | InductionStep(_) => false;

    let stepped_id: Model.t => option(Haz3lcore.Id.t) =
      fun
      | EvalStep(m) => EvalStep.View.stepped_id(m)
      | InductionStep(_) => None;

    let view =
        (~globals, ~selection: option(Selection.t), ~signal, ~inject)
        : (Model.t => 'a) =>
      fun
      | EvalStep(m) => EvalStep.View.view(m)
      | InductionStep(m) =>
        InductionStep.View.view(
          ~globals,
          ~selection=
            switch (selection) {
            | Some(InductionStep(s)) => Some(s)
            | _ => None
            },
          ~signal=(MakeActive(a)) => signal(MakeActive(InductionStep(a))),
          ~inject=a => inject(Update.InductionStep(a)),
          m,
        );

    let get_text: Model.t => 'a =
      fun
      | EvalStep(m) => EvalStep.View.get_text(m)
      | InductionStep(_) => Web.Node.text("By induction");
  };
}

and InductionStep: {
  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let init: t;
    let get_next_expr: t => Exp.t;
    let get_next_state: t => EvaluatorState.t;
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let update: (~settings: Settings.t, t, Model.t) => Updated.t(Model.t);
    let calculate:
      (
        ~settings: CoreSettings.t,
        ~prev_expr: Exp.t,
        ~state: EvaluatorState.t,
        Model.t
      ) =>
      Model.t;
  };

  module Selection: {
    open Cursor;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | ScrutEditor
      | CasePatternEditor(int)
      | CaseStepper(int, Stepper.Selection.t);

    let get_cursor_info: (~selection: t, Model.t) => cursor(Update.t);

    let handle_key_event:
      (~selection: t, ~event: Key.t, Model.t) => option(Update.t);
  };

  module View: {
    type signal =
      | MakeActive(Selection.t);

    let view:
      (
        ~globals: Globals.t,
        ~selection: option(Selection.t),
        ~signal: signal => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        Model.t
      ) =>
      list(Web.Node.t);
  };
} = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      // Updated:
      scrut: CodeEditable.Model.t,
      cases: list((CodeEditable.Model.t, Stepper.Model.t)),
      result_editor: CodeEditable.Model.t,
      // Calculated:
      result: Haz3lcore.Exp.t,
      result_state: EvaluatorState.t,
      induction_valid: bool,
    };

    let init = {
      scrut: CodeEditable.Model.mk_from_exp(Exp.EmptyHole |> Exp.fresh),
      cases: [],
      result_editor:
        CodeEditable.Model.mk_from_exp(Exp.EmptyHole |> Exp.fresh),
      result: Exp.EmptyHole |> Exp.fresh,
      result_state: EvaluatorState.init,
      induction_valid: false,
    };

    let get_next_expr = (model: t): Exp.t => model.result;
    let get_next_state = (model: t): EvaluatorState.t => model.result_state;
  };

  module Update = {
    open Updated;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | ScrutUpdate(CodeEditable.Update.t)
      | CasePatternUpdate(int, CodeEditable.Update.t)
      | CaseStepperUpdate(int, Stepper.Update.t)
      | AddCase;

    let update = (~settings, update, model: Model.t) => {
      switch (update) {
      | ScrutUpdate(update) =>
        let* scrut =
          CodeEditable.Update.update(~settings, update, model.scrut);
        {...model, scrut};
      | CasePatternUpdate(n, update) =>
        let (p, st) = List.nth(model.cases, n);
        let* p = CodeEditable.Update.update(~settings, update, p);
        let cases = Util.ListUtil.put_nth(n, (p, st), model.cases);
        {...model, cases};
      | CaseStepperUpdate(n, update) =>
        let (p, st) = List.nth(model.cases, n);
        let* st = Stepper.Update.update(~settings, update, st);
        let cases = Util.ListUtil.put_nth(n, (p, st), model.cases);
        {...model, cases};
      | AddCase =>
        let new_case = (
          CodeEditable.Model.mk_from_exp(Exp.EmptyHole |> Exp.fresh),
          Stepper.Model.init(),
        );
        let cases = [new_case, ...model.cases];
        {...model, cases} |> Updated.return;
      };
    };

    let calculate =
        (~settings, ~prev_expr, ~state as _, model: Model.t): Model.t => {
      // TODO[Matt]: something about stitching with contexts.
      let scrut =
        CodeEditable.Update.calculate(~settings, ~stitch=x => x, model.scrut);
      let cases =
        model.cases
        |> List.map(((p, st)) => {
             // TODO[Matt:] Calculate should take account of sort
             let p: CodeEditable.Model.t =
               CodeEditable.Update.calculate(~settings, ~stitch=x => x, p);
             // TODO[Matt]: THIS IS NOT SOUND
             let goal =
               Exp.map_term(
                 ~f_exp=
                   (cont, e) =>
                     if (Exp.fast_equal(e, scrut.statics.term)) {
                       p.statics.term;
                     } else {
                       cont(e);
                     },
                 prev_expr,
               );
             let st = Stepper.Update.calculate(~settings, goal, st);
             (p, st);
           });
      {...model, cases, scrut};
    };
  };

  module Selection = {
    open Cursor;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | ScrutEditor
      | CasePatternEditor(int)
      | CaseStepper(int, Stepper.Selection.t);

    let get_cursor_info = (~selection: t, model: Model.t): cursor(Update.t) => {
      switch (selection) {
      | ScrutEditor =>
        let+ ci =
          CodeEditable.Selection.get_cursor_info(~selection=(), model.scrut);
        Update.ScrutUpdate(ci);
      | CasePatternEditor(n) =>
        let (p, _) = List.nth(model.cases, n);
        let+ ci = CodeEditable.Selection.get_cursor_info(~selection=(), p);
        Update.CasePatternUpdate(n, ci);
      | CaseStepper(n, s) =>
        let (_, st) = List.nth(model.cases, n);
        let+ ci = Stepper.Selection.get_cursor_info(~selection=s, st);
        Update.CaseStepperUpdate(n, ci);
      };
    };

    let handle_key_event =
        (~selection: t, ~event, model: Model.t): option(Update.t) =>
      switch (selection) {
      | ScrutEditor =>
        CodeEditable.Selection.handle_key_event(
          ~selection=(),
          model.scrut,
          event,
        )
        |> Option.map(x => Update.ScrutUpdate(x))
      | CasePatternEditor(n) =>
        let (p, _) = List.nth(model.cases, n);
        CodeEditable.Selection.handle_key_event(~selection=(), p, event)
        |> Option.map(x => Update.CasePatternUpdate(n, x));
      | CaseStepper(n, s) =>
        let (_, st) = List.nth(model.cases, n);
        Stepper.Selection.handle_key_event(~selection=s, ~event, st)
        |> Option.map(x => Update.CaseStepperUpdate(n, x));
      };
  };

  module View = {
    type signal =
      | MakeActive(Selection.t);

    let view = (~globals, ~selection, ~signal, ~inject, model: Model.t) =>
      [
        Web.Node.text("Induction Step not implemented yet!"),
        CodeEditable.View.view(
          ~globals,
          ~signal=
            fun
            | MakeActive => signal(MakeActive(ScrutEditor)),
          ~inject=a => inject(Update.ScrutUpdate(a)),
          ~selected=selection == Some(Selection.ScrutEditor),
          model.scrut,
        ),
        Widgets.button_d(
          Icons.star,
          inject(AddCase),
          ~disabled=false,
          ~tooltip="Add a case to the induction",
        ),
      ]
      @ (
        model.cases
        |> List.mapi((i, (p, c)) =>
             [
               CodeEditable.View.view(
                 ~globals,
                 ~signal=
                   fun
                   | MakeActive => signal(MakeActive(CasePatternEditor(i))),
                 ~inject=a => inject(Update.CasePatternUpdate(i, a)),
                 ~selected=selection == Some(Selection.CasePatternEditor(i)),
                 ~sort=Sort.Pat,
                 p,
               ),
             ]
             @ Stepper.View.view(
                 ~globals,
                 ~selected=
                   switch (selection) {
                   | Some(CaseStepper(n, x)) when n == 0 => Some(x)
                   | _ => None
                   },
                 ~signal=
                   fun
                   | MakeActive(x) => signal(MakeActive(CaseStepper(i, x)))
                   | HideStepper => Ui_effect.Ignore
                   | JumpTo(_) => Ui_effect.Ignore,
                 ~inject=a => inject(Update.CaseStepperUpdate(i, a)),
                 ~read_only=false,
                 c,
               )
           )
        |> List.flatten
      );
  };
};

include Stepper;
