open Util;
open Haz3lcore;
open Sexplib.Std;
open OptUtil.Syntax;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type b = {
    // Updated:
    step: Haz3lcore.EvaluatorStep.EvalObj.t,
    // Calculated:
    to_ids: list(Id.t),
    hidden: bool,
    valid: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type a =
    | PendingStep
    | A({
        // Calculated:
        expr: Exp.t,
        state: EvaluatorState.t,
        editor: CodeWithStatics.Model.t,
        previous_substitutions: list(Id.t),
        next_steps: list(b),
      });

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    history: Aba.t(a, b),
    cached_settings: option(CoreSettings.t),
  };

  let init = () => {
    history: Aba.singleton(PendingStep),
    cached_settings: None,
  };

  let get_next_steps = (model: Aba.t(a, b)): list(b) =>
    model
    |> Aba.hd
    |> (
      fun
      | A({next_steps, _}) => next_steps
      | PendingStep => []
    );

  let get_state = (model: Aba.t(a, b)): EvaluatorState.t =>
    model
    |> Aba.last_a
    |> (
      fun
      | A({state, _}) => state
      | PendingStep => EvaluatorState.init
    );

  let get_previous_substitutions = (model: Aba.t(a, b)): list(Id.t) =>
    model
    |> Aba.last_a
    |> (
      fun
      | A({previous_substitutions, _}) => previous_substitutions
      | PendingStep => []
    );

  let get_elaboration = (model: t): option(Exp.t) =>
    model.history
    |> Aba.hd
    |> (
      fun
      | A({expr, _}) => Some(expr)
      | _ => None
    );

  let can_undo = (model: t) => {
    model.history |> Aba.get_bs |> List.exists((b: b) => !b.hidden);
  };

  type persistent = list(Haz3lcore.EvaluatorStep.EvalObj.persistent);
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | StepForward(int)
    | StepBackward;

  let update = (action: t, model: Model.t): Updated.t(Model.t) => {
    switch (action) {
    | StepForward(idx) =>
      {
        ...model,
        history:
          Aba.cons(
            Model.PendingStep,
            model.history |> Model.get_next_steps |> List.nth(_, idx),
            model.history,
          ),
      }
      |> Updated.return
    | StepBackward =>
      {
        ...model,
        history: {
          let rec step_backward:
            Aba.t(Model.a, Model.b) => Aba.t(Model.a, Model.b) = (
            fun
            | ([_, ...as_], [{hidden: true, _}, ...bs]) =>
              (as_, bs) |> step_backward
            | ([_, ...as_], [_, ...bs]) => (as_, bs)
            | x => x
          );
          step_backward(model.history);
        },
      }
      |> Updated.return
    };
  };

  let calc_a = (~settings, expr: Exp.t, state, previous_substitutions) => {
    let editor =
      CodeWithStatics.Model.mk_from_exp(expr)
      |> CodeWithStatics.Update.calculate(~settings, ~stitch=x => x);
    let next_steps =
      EvaluatorStep.decompose(expr, state)
      |> List.map(
           EvaluatorStep.should_hide_eval_obj(~settings=settings.evaluation),
         )
      |> List.map(
           fun
           | (FilterAction.Step, x) =>
             Model.{hidden: false, step: x, to_ids: [Id.mk()], valid: true}
           | (FilterAction.Eval, x) =>
             Model.{hidden: true, step: x, to_ids: [Id.mk()], valid: true},
         );
    Model.A({expr, state, previous_substitutions, editor, next_steps});
  };

  let get_next_a =
      (~settings, history: Aba.t(Model.a, Model.b), b: Model.b)
      : option(Model.a) => {
    let state = ref(Model.get_state(history));
    let+ next_expr = EvaluatorStep.take_step(state, b.step.env, b.step.d_loc);
    let next_expr = {...next_expr, ids: b.to_ids};
    let next_state = state^;
    let previous_substitutions =
      (b.step.knd == Transition.VarLookup ? [b.step.d_loc |> Exp.rep_id] : [])
      @ (
        Model.get_previous_substitutions(history)
        |> List.map((id: Id.t) =>
             if (id == (b.step.d_loc |> Exp.rep_id)) {
               next_expr |> Exp.rep_id;
             } else {
               id;
             }
           )
      );
    let next_expr = EvalCtx.compose(b.step.ctx, next_expr);
    calc_a(~settings, next_expr, next_state, previous_substitutions);
  };

  let rec take_hidden_steps =
          (~settings, history: Aba.t(Model.a, Model.b))
          : Aba.t(Model.a, Model.b) => {
    let next_steps = Model.get_next_steps(history);
    let hidden_steps = List.filter((s: Model.b) => s.hidden, next_steps);
    switch (hidden_steps) {
    | [] => history
    | [x, ..._] =>
      switch (get_next_a(~settings, history, x)) {
      | Some(a') => take_hidden_steps(~settings, Aba.cons(a', x, history))
      | None => failwith("Unable to take step!")
      }
    };
  };

  let full_calculate = (~settings, elab: Exp.t, model: Model.t): Model.t => {
    {
      history:
        Aba.fold_right(
          (_, b: Model.b, c) => {
            let b' = {
              let (let&) = (x, y) => Util.OptUtil.get(y, x);
              let options = Model.get_next_steps(c);
              let correct_id =
                List.filter(
                  (b': Model.b) => b'.step.d_loc.ids == b.step.d_loc.ids,
                  options,
                );
              let& () = List.nth_opt(correct_id, 0);
              {...b, valid: false};
            };
            switch (get_next_a(~settings, model.history, b'), b'.valid) {
            | (Some(a'), true) => Aba.cons(a', b', c)
            | (None, _)
            | (_, false) => c
            };
          },
          _ =>
            calc_a(~settings, elab, EvaluatorState.init, []) |> Aba.singleton,
          model.history,
        )
        |> take_hidden_steps(~settings),
      cached_settings: Some(settings),
    };
  };

  // TODO[Matt]: faster calculation
  // let calculate_pending = (~settings, elab: Exp.t) => {};

  let calculate = (~settings, elab: Exp.t) => {
    let elab = elab |> DHExp.repair_ids;
    full_calculate(~settings, elab);
  };
};

module View = {
  open Virtual_dom.Vdom;
  open Node;

  type event =
    | HideStepper
    | JumpTo(Haz3lcore.Id.t);

  let view =
      (
        ~globals as {settings, inject_global, _} as globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
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
      |> (settings.core.evaluation.stepper_history ? x => x : (_ => []))
      |> (
        settings.core.evaluation.show_hidden_steps
          ? x => x : List.filter(((_, b: Model.b, _)) => !b.hidden)
      )
      |> List.map(((_, b: Model.b, a: Model.a)) =>
           switch (a) {
           | A(a) => [
               div(
                 ~attrs=[
                   Attr.classes(
                     ["cell-item", "cell-result"]
                     @ (b.hidden ? ["hidden"] : []),
                   ),
                 ],
                 [
                   div(~attrs=[Attr.class_("equiv")], [Node.text("≡")]),
                   StepperEditor.Stepped.view(
                     ~globals,
                     ~overlays=[],
                     {
                       editor: a.editor,
                       step_id: Some(b.step.d_loc |> Exp.rep_id),
                     },
                   ),
                   div(
                     ~attrs=[Attr.classes(["stepper-justification"])],
                     [
                       b.step.knd
                       |> Transition.stepper_justification
                       |> Node.text,
                     ],
                   ),
                 ],
               ),
             ]
           | PendingStep => [
               div(~attrs=[Attr.class_("cell-item")], [text("...")]),
             ]
           }
         )
      |> List.flatten
      |> List.rev;
    };
    let current_step = {
      let model = stepper.history |> Aba.hd;
      div(
        ~attrs=[Attr.classes(["cell-item", "cell-result"])],
        (
          switch (model) {
          | A(model) => [
              div(~attrs=[Attr.class_("equiv")], [Node.text("≡")]),
              StepperEditor.Steppable.view(
                ~globals,
                ~signal=(TakeStep(x)) => inject(Update.StepForward(x)),
                ~overlays=[],
                {
                  editor: model.editor,
                  next_steps:
                    List.map(
                      (option: Model.b) => option.step.d_loc |> Exp.rep_id,
                      model.next_steps,
                    ),
                },
              ),
            ]
          | PendingStep => [
              div(~attrs=[Attr.class_("cell-item")], [text("...")]),
            ]
          }
        )
        @ (
          read_only
            ? []
            : [
              button_back,
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
