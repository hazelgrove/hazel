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
      | A({next_steps, _}) => {
          next_steps;
        }
      | PendingStep => []
    );

  let get_state = (model: Aba.t(a, b)): EvaluatorState.t =>
    model
    |> Aba.hd
    |> (
      fun
      | A({state, _}) => state
      | PendingStep => EvaluatorState.init
    );

  let get_previous_substitutions = (model: Aba.t(a, b)): list(Id.t) =>
    model
    |> Aba.hd
    |> (
      fun
      | A({previous_substitutions, _}) => previous_substitutions
      | PendingStep => []
    );

  let get_elaboration = (model: t): option(Exp.t) =>
    model.history
    |> Aba.last_a
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
    | StepperEditor(int, StepperEditor.Update.t)
    | StepForward(int)
    | StepBackward;

  let update = (~settings, action: t, model: Model.t): Updated.t(Model.t) => {
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
    | StepperEditor(idx, x) =>
      {
        ...model,
        history:
          model.history
          |> Aba.get_as
          |> List.mapi((i, a: Model.a) =>
               if (i == idx) {
                 switch (a) {
                 | A(a) =>
                   let editor =
                     CodeSelectable.Update.update(~settings, x, a.editor)
                     |> ((u: Updated.t('a)) => u.model);
                   Model.A({...a, editor});
                 | PendingStep => PendingStep
                 };
               } else {
                 a;
               }
             )
          |> Aba.mk(_, model.history |> Aba.get_bs),
      }
      |> Updated.return(~is_edit=false)
    };
  };

  let calc_a =
      (~settings: CoreSettings.t, expr: Exp.t, state, previous_substitutions) => {
    let editor = CodeWithStatics.Model.mk_from_exp(~settings, expr);
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
          (old_a: Model.a, b: Model.b, c) => {
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
            switch (
              old_a,
              get_next_a(~settings, model.history, b'),
              b'.valid,
            ) {
            | (A({expr, _}), Some(A({expr: expr', _})), true)
                when {
                  DHExp.fast_equal(expr, expr');
                } =>
              Aba.cons(old_a, b', c)
            | (_, Some(a'), true) => Aba.cons(a', b', c)
            | (_, None, _)
            | (_, _, false) => c
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

  /* fast_calculate is a bit hacky, and relies on that fact that steps are currently
     only added, deleted, or the elbaoration of the whole thing is changed. If the user
     were able to change the properties of some step, this code would break */

  let fast_calculate = (~settings: CoreSettings.t, model: Model.t): Model.t => {
    {
      history:
        Aba.fold_right(
          fun
          | (PendingStep: Model.a) => (
              (b: Model.b, c) => {
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
                | _ => c
                };
              }
            )
          | a => (
              (b, c) => {
                Aba.cons(a, b, c);
              }
            ),
          a => Aba.singleton(a),
          model.history,
        )
        |> take_hidden_steps(~settings),
      cached_settings: Some(settings),
    };
  };

  let calculate = (~settings, elab: Exp.t, model: Model.t) => {
    let elab = elab |> DHExp.repair_ids;
    let elab_changed =
      switch (Model.get_elaboration(model)) {
      | None => true
      | Some(e) => !DHExp.fast_equal(elab, e)
      };
    let m =
      elab_changed
        ? full_calculate(~settings, elab, model)
        : fast_calculate(~settings, model);
    m;
  };
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | A(int, StepperEditor.Selection.t);
};

module View = {
  open Virtual_dom.Vdom;
  open Node;

  type event =
    | HideStepper
    | JumpTo(Haz3lcore.Id.t)
    | MakeActive(Selection.t);

  let view =
      (
        ~globals as {settings, inject_global, _} as globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selection: option(Selection.t),
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
      |> List.mapi((i, (_, b: Model.b, a: Model.a)) =>
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
                   StepperEditor.View.view(
                     ~globals,
                     ~overlays=[],
                     ~selected=selection == Some(A(i, ())),
                     ~inject=
                       (x: StepperEditor.Update.t) =>
                         inject(StepperEditor(i, x)),
                     ~signal=
                       fun
                       | TakeStep(_) => Ui_effect.Ignore
                       | MakeActive => signal(MakeActive(A(i, ()))),
                     {
                       editor: a.editor,
                       next_steps: [],
                       taken_steps: [b.step.d_loc |> Exp.rep_id],
                     },
                   )
                   |> (x => [x])
                   |> Web.div_c("result"),
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
      let current_n = stepper.history |> Aba.get_bs |> List.length;
      div(
        ~attrs=[Attr.classes(["cell-item", "cell-result"])],
        (
          switch (model) {
          | A(model) => [
              div(~attrs=[Attr.class_("equiv")], [Node.text("≡")]),
              StepperEditor.View.view(
                ~globals,
                ~selected=selection == Some(A(current_n, ())),
                ~inject=
                  (x: StepperEditor.Update.t) =>
                    inject(StepperEditor(current_n, x)),
                ~signal=
                  fun
                  | TakeStep(x) => inject(Update.StepForward(x))
                  | MakeActive => signal(MakeActive(A(current_n, ()))),
                ~overlays=[],
                {
                  editor: model.editor,
                  next_steps:
                    List.map(
                      (option: Model.b) => option.step.d_loc |> Exp.rep_id,
                      model.next_steps,
                    ),
                  taken_steps: [],
                },
              )
              |> (x => [x])
              |> Web.div_c("result"),
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
