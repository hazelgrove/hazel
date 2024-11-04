open Util;
open Haz3lcore;
open Sexplib.Std;
open OptUtil.Syntax;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type b = {
    // Constants:
    step: Haz3lcore.EvaluatorStep.EvalObj.t,
    to_ids: list(Id.t),
    // Calculated:
    hidden: bool // Depends on settings
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type a' = {
    // Constants:
    expr: Exp.t,
    state: EvaluatorState.t,
    previous_substitutions: list(Id.t),
    next_steps: list(b),
    // Updated & Calculated:
    editor: Calc.t(CodeSelectable.Model.t),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type a = Calc.saved(a');

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Calculated & Updated:
    history: Aba.t(a, b),
    // Calculated:
    cached_settings: Calc.saved(CoreSettings.t),
  };

  let init = () => {
    history: Aba.singleton(Calc.Pending),
    cached_settings: Calc.Pending,
  };

  let get_next_steps = (model: Aba.t(a, b)): list(b) =>
    model
    |> Aba.hd
    |> (
      fun
      | Calculated({next_steps, _}) => {
          next_steps;
        }
      | Pending => []
    );

  let get_state = (model: Aba.t(a, b)): EvaluatorState.t =>
    model
    |> Aba.hd
    |> (
      fun
      | Calculated({state, _}) => state
      | Pending => EvaluatorState.init
    );

  let get_previous_substitutions = (model: Aba.t(a, b)): list(Id.t) =>
    model
    |> Aba.hd
    |> (
      fun
      | Calculated({previous_substitutions, _}) => previous_substitutions
      | Pending => []
    );

  let get_elaboration = (model: t): option(Exp.t) =>
    model.history
    |> Aba.last_a
    |> (
      fun
      | Calculated({expr, _}) => Some(expr)
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
            Calc.Pending,
            Model.get_next_steps(model.history) |> List.nth(_, idx),
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
          |> ListUtil.map_nth(
               idx,
               Calc.map_saved((a: Model.a') => {
                 let editor =
                   CodeSelectable.Update.update(
                     ~settings,
                     x,
                     a.editor |> Calc.get_value,
                   )
                   |> ((u: Updated.t('a)) => u.model);
                 let editor = Calc.NewValue(editor);
                 {...a, editor};
               }),
             )
          |> Aba.mk(_, model.history |> Aba.get_bs),
      }
      |> Updated.return(~is_edit=false)
    };
  };

  open Calc.Syntax;

  let calc_next_steps = (settings: CoreSettings.t, expr, state) =>
    EvaluatorStep.decompose(expr, state)
    |> List.map(
         EvaluatorStep.should_hide_eval_obj(~settings=settings.evaluation),
       )
    |> List.map(
         fun
         | (FilterAction.Step, x) =>
           Model.{hidden: false, step: x, to_ids: [Id.mk()]}
         | (FilterAction.Eval, x) =>
           Model.{hidden: true, step: x, to_ids: [Id.mk()]},
       );

  let get_next_a =
      (
        ~settings: Calc.t('a),
        prev_a: Calc.t(Model.a'),
        b: Model.b,
        old_a: Calc.saved(Model.a'),
      ) => {
    old_a
    |> Calc.map_saved(Option.some)
    // Only perform below if either previous a or settings have changed
    |> {
      let.calc {expr: _, state, previous_substitutions, next_steps, _} = prev_a
      and.calc settings = settings;

      // Check b is valid
      let* b =
        List.find_opt(
          (b': Model.b) => b'.step.d_loc.ids == b.step.d_loc.ids,
          next_steps,
        );

      // Use b
      let state = ref(state);
      let+ next_expr =
        EvaluatorStep.take_step(state, b.step.env, b.step.d_loc);
      let next_expr = {...next_expr, ids: b.to_ids};
      let next_state = state^;
      let previous_substitutions =
        (
          b.step.knd == Transition.VarLookup
            ? [b.step.d_loc |> Exp.rep_id] : []
        )
        @ (
          previous_substitutions
          |> List.map((id: Id.t) =>
               if (id == (b.step.d_loc |> Exp.rep_id)) {
                 next_expr |> Exp.rep_id;
               } else {
                 id;
               }
             )
        );
      let next_expr = EvalCtx.compose(b.step.ctx, next_expr);
      let editor = CodeWithStatics.Model.mk_from_exp(~settings, next_expr);
      let next_steps = calc_next_steps(settings, next_expr, next_state);
      (
        {
          expr: next_expr,
          state: next_state,
          previous_substitutions,
          editor: Calc.NewValue(editor),
          next_steps,
        }: Model.a'
      );
    };
  };

  let rec take_hidden_steps =
          (
            ~settings,
            prev_a: Calc.t(Model.a'),
            history: Aba.t(Model.a, Model.b),
          )
          : Aba.t(Model.a, Model.b) => {
    let next_steps = Model.get_next_steps(history);
    let hidden_steps = List.filter((s: Model.b) => s.hidden, next_steps);
    switch (hidden_steps) {
    | [] => history
    | [x, ..._] =>
      switch (
        get_next_a(~settings, prev_a, x, Calc.Pending) |> Calc.to_option
      ) {
      | Some(a') =>
        take_hidden_steps(
          ~settings,
          a',
          Aba.cons(a' |> Calc.save, x, history),
        )
      | None => failwith("Unable to take step!")
      }
    };
  };

  let calculate_editors =
      (~settings, history: Aba.t(Model.a, Model.b)): Aba.t(Model.a, Model.b) => {
    history
    |> Aba.map_a(
         Calc.map_saved((Model.{editor, _} as a) => {
           editor
           |> Calc.map_if_new(
                CodeSelectable.Update.calculate(
                  ~settings=settings |> Calc.get_value,
                  ~is_edited=false,
                  ~stitch=x =>
                  x
                ),
              )
           |> (editor => {...a, editor})
         }),
       );
  };

  let calculate =
      (~settings, elab: Exp.t, {history, cached_settings}: Model.t) => {
    let settings = cached_settings |> Calc.set(settings);
    let elab =
      history
      |> Aba.last_a
      |> Calc.map_saved((u: Model.a') => u.expr)
      |> Calc.set(~eq=Exp.fast_equal, elab);

    let (prev_a, history) =
      Aba.fold_right(
        (a: Model.a, b: Model.b, (prev_a: Calc.t(Model.a'), history)) => {
          print_endline("X " ++ string_of_bool(prev_a |> Calc.is_new));
          let next_a = get_next_a(~settings, prev_a, b, a) |> Calc.to_option;
          switch (next_a) {
          | None => (prev_a, history)
          | Some(next_a) =>
            print_endline(
              "naed "
              ++ string_of_bool(
                   (next_a |> Calc.get_value).editor |> Calc.is_new,
                 ),
            );
            (next_a, Aba.cons(next_a |> Calc.save, b, history));
          };
        },
        (old_a: Model.a) => {
          let new_a =
            old_a
            |> {
              let.calc elab = elab
              and.calc settings = settings;
              print_endline("New!");
              let editor = CodeWithStatics.Model.mk_from_exp(~settings, elab);
              let next_steps =
                calc_next_steps(settings, elab, EvaluatorState.init);
              Model.{
                expr: elab,
                state: EvaluatorState.init,
                previous_substitutions: [],
                editor: Calc.NewValue(editor),
                next_steps,
              };
            };
          (new_a, Aba.singleton(new_a |> Calc.save));
        },
        history,
      );

    Model.{
      history:
        history
        |> take_hidden_steps(~settings, prev_a)
        |> calculate_editors(~settings),
      cached_settings: settings |> Calc.save,
    };
  };
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | A(int, StepperEditor.Selection.t);

  let handle_key_event =
      (~selection: t, ~event, mr: Model.t): option(Update.t) => {
    let A(i, s) = selection;
    let a: Model.a = mr.history |> Aba.get_as |> List.nth(_, i);
    switch (a) {
    | Calculated(a) =>
      let+ x =
        StepperEditor.Selection.handle_key_event(
          ~selection=s,
          a.editor |> Calc.get_value,
          event,
        );
      Update.StepperEditor(i, x);
    | Pending => None
    };
  };
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
           | Calculated(a) =>
             [
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
                     ~selected=selection == Some(A(i + 1, ())),
                     ~inject=
                       (x: StepperEditor.Update.t) =>
                         inject(StepperEditor(i + 1, x)),
                     ~signal=
                       fun
                       | TakeStep(_) => Ui_effect.Ignore
                       | MakeActive => signal(MakeActive(A(i + 1, ()))),
                     {
                       editor: a.editor |> Calc.get_value,
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
             |> List.rev
           | Pending => [
               div(~attrs=[Attr.class_("cell-item")], [text("...")]),
             ]
           }
         )
      |> List.rev
      |> List.flatten;
    };
    let current_step = {
      let model = stepper.history |> Aba.hd;
      let current_n = 0;
      div(
        ~attrs=[Attr.classes(["cell-item", "cell-result"])],
        (
          switch (model) {
          | Calculated(model) => [
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
                  editor: model.editor |> Calc.get_value,
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
          | Pending => [
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
