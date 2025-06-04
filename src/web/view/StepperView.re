open Util;
open Language;
open Sexplib.Std;
open OptUtil.Syntax;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type b = {
    // Constants:
    step: EvaluatorStep.step,
    to_ids: list(Id.t),
    // Calculated:
    hidden: bool // Depends on settings
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type a' = {
    // Constants:
    expr: Exp.t,
    state: EvaluatorState.t,
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
    cached_elab: Calc.saved(Exp.t),
  };

  let init = () => {
    history: Aba.singleton(Calc.Pending),
    cached_settings: Calc.Pending,
    cached_elab: Calc.Pending,
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
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    // int here should include hidden steps
    // Note this int is backwards compared to the selection (0 is the most recent step)
    | StepperEditor(int, StepperEditor.Update.t)
    | StepForward(int)
    | StepBackward;

  let can_undo = (action: t) => {
    switch (action) {
    | StepperEditor(_, action) => StepperEditor.Update.can_undo(action)
    | StepForward(_) => true
    | StepBackward => true
    };
  };

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
                 {
                   ...a,
                   editor,
                 };
               }),
             )
          |> Aba.mk(_, model.history |> Aba.get_bs),
      }
      |> Updated.return(~is_edit=false)
    };
  };

  open Calc.Syntax;

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
      let.calc {expr, state, _} = prev_a
      and.calc settings: Calc.t(CoreSettings.t) = settings;

      let* step = EvaluatorStep.refresh_step(~settings, expr, state, b.step);
      let* (next_expr, next_state) = EvaluatorStep.take_step(step);

      let next_status =
        EvaluatorStep.get_status(~settings, next_expr, next_state);
      let next_steps =
        switch (next_status) {
        | AutoStep(step) => [
            Model.{
              step,
              to_ids: [Id.mk()],
              hidden: true,
            },
          ]
        | AvailableSteps(steps) =>
          List.map(
            step => {
              Model.{
                step,
                to_ids: [Id.mk()],
                hidden: false,
              }
            },
            steps,
          )
        };
      let editor = CodeWithStatics.Model.mk_from_exp(~settings, next_expr);
      Some(
        {
          expr: next_expr,
          state: next_state,
          editor: Calc.NewValue(editor),
          next_steps,
        }: Model.a',
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
    print_endline("AAAAA");
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
                  ~is_dynamic_term=true,
                  ~is_edited=false,
                  ~dynamics=Dynamics.Map.empty, // No projectors in stepper atm
                  ~stitch=x =>
                  x
                ),
              )
           |> (
             editor => {
               ...a,
               editor,
             }
           )
         }),
       );
  };

  let calculate =
      (
        ~settings,
        elab: Exp.t,
        {history, cached_settings, cached_elab}: Model.t,
      ) => {
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

    let (prev_a, history) =
      Aba.fold_right(
        (a: Model.a, b: Model.b, (prev_a: Calc.t(Model.a'), history)) => {
          let next_a = get_next_a(~settings, prev_a, b, a) |> Calc.to_option;
          switch (next_a) {
          | None => (prev_a, history)
          | Some(next_a) => (
              next_a,
              Aba.cons(next_a |> Calc.save, b, history),
            )
          };
        },
        (old_a: Model.a) => {
          let new_a =
            old_a
            |> {
              let.calc elab = elab
              and.calc settings = settings;
              let elab = elab |> Typ.replace_temp_exp;
              let editor = CodeWithStatics.Model.mk_from_exp(~settings, elab);
              let next_status =
                EvaluatorStep.get_status(
                  ~settings,
                  elab,
                  EvaluatorState.init,
                );
              let next_steps =
                switch (next_status) {
                | AutoStep(step) => [
                    Model.{
                      step,
                      to_ids: [Id.mk()],
                      hidden: true,
                    },
                  ]
                | AvailableSteps(steps) =>
                  List.map(
                    step => {
                      Model.{
                        step,
                        to_ids: [Id.mk()],
                        hidden: false,
                      }
                    },
                    steps,
                  )
                };
              Model.{
                expr: elab,
                state: EvaluatorState.init,
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
      cached_elab: elab |> Calc.save,
    };
  };
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    // int here should include hidden steps
    // Note this int is backwards compared to the editors (so that 0 is the oldest step, and selections are preserved)
    | A(int, StepperEditor.Selection.t);

  let get_cursor_info = (~selection: t, mr: Model.t): Cursor.cursor(Update.t) => {
    Cursor.(
      switch (selection) {
      | A(n, editor_selection) =>
        let a: option(Model.a) =
          mr.history
          |> Aba.get_as
          |> ListUtil.nth_opt(List.length(mr.history |> Aba.get_as) - n - 1);
        switch (a) {
        | Some(Calculated(a)) =>
          let+ x =
            StepperEditor.Selection.get_cursor_info(
              ~selection=editor_selection,
              a.editor |> Calc.get_value,
            );
          Update.StepperEditor(n, x);
        | None
        | Some(Pending) => empty
        };
      }
    );
  };

  let handle_key_event =
      (~selection: t, ~event, mr: Model.t): option(Update.t) => {
    let A(i, s) = selection;
    let a: option(Model.a) =
      mr.history
      |> Aba.get_as
      |> ListUtil.nth_opt(List.length(mr.history |> Aba.get_as) - i - 1);
    switch (a) {
    | Some(Calculated(a)) =>
      let+ x =
        StepperEditor.Selection.handle_key_event(
          ~selection=s,
          a.editor |> Calc.get_value,
          event,
        );
      Update.StepperEditor(i, x);
    | Some(Pending)
    | None => None
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
      |> List.mapi((i, x) => (i, x))
      |> (
        settings.core.evaluation.show_hidden_steps
          ? x => x : List.filter(((_, (_, b: Model.b, _))) => !b.hidden)
      )
      |> List.map(((i, (_, b: Model.b, a: Model.a))) =>
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
                     ~selected=
                       selection
                       == Some(
                            A(
                              List.length(stepper.history |> Aba.get_as)
                              - (i + 1)
                              - 1,
                              (),
                            ),
                          ),
                     ~inject=
                       (x: StepperEditor.Update.t) =>
                         inject(StepperEditor(i + 1, x)),
                     ~signal=
                       fun
                       | TakeStep(_) => Ui_effect.Ignore
                       | MakeActive =>
                         signal(
                           MakeActive(
                             A(
                               List.length(stepper.history |> Aba.get_as)
                               - (i + 1)
                               - 1,
                               (),
                             ),
                           ),
                         ),
                     {
                       editor: a.editor |> Calc.get_value,
                       next_steps: [],
                       taken_steps: [b.step |> EvaluatorStep.get_step_id],
                     },
                   )
                   |> (x => [x])
                   |> WebUtil.div_c("result"),
                   div(
                     ~attrs=[Attr.classes(["stepper-justification"])],
                     [
                       b.step
                       |> EvaluatorStep.get_step_kind
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
                ~selected=
                  selection
                  == Some(
                       A(
                         List.length(stepper.history |> Aba.get_as)
                         - current_n
                         - 1,
                         (),
                       ),
                     ),
                ~inject=
                  (x: StepperEditor.Update.t) =>
                    inject(StepperEditor(current_n, x)),
                ~signal=
                  fun
                  | TakeStep(x) =>
                    Effect.Many([
                      inject(Update.StepForward(x)),
                      Effect.Stop_propagation,
                    ])
                  | MakeActive =>
                    signal(
                      MakeActive(
                        A(
                          List.length(stepper.history |> Aba.get_as)
                          - current_n
                          - 1,
                          (),
                        ),
                      ),
                    ),
                ~overlays=[],
                {
                  editor: model.editor |> Calc.get_value,
                  next_steps:
                    List.map(
                      (option: Model.b) =>
                        option.step |> EvaluatorStep.get_step_id,
                      model.next_steps,
                    ),
                  taken_steps: [],
                },
              )
              |> (x => [x])
              |> WebUtil.div_c("result"),
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
