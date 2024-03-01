open Virtual_dom.Vdom;
open Node;
open Haz3lcore;

let settings_modal = (~inject, settings: CoreSettings.Evaluation.t) => {
  let modal = div(~attr=Attr.many([Attr.class_("settings-modal")]));
  let setting = (icon, name, current, action: UpdateAction.settings_action) =>
    div(
      ~attr=Attr.many([Attr.class_("settings-toggle")]),
      [
        Widgets.toggle(~tooltip=name, icon, current, _ =>
          inject(Update.Set(action))
        ),
        text(name),
      ],
    );
  [
    modal([
      div(
        ~attr=Attr.many([Attr.class_("settings-modal-top")]),
        [
          Widgets.button(Icons.x, _ =>
            inject(Update.Set(Evaluation(ShowSettings)))
          ),
        ],
      ),
      setting(
        "h",
        "show full step trace",
        settings.stepper_history,
        Evaluation(ShowRecord),
      ),
      setting(
        "|",
        "show case clauses",
        settings.show_case_clauses,
        Evaluation(ShowCaseClauses),
      ),
      setting(
        "λ",
        "show function bodies",
        settings.show_fn_bodies,
        Evaluation(ShowFnBodies),
      ),
      setting(
        "x",
        "show fixpoints",
        settings.show_fixpoints,
        Evaluation(ShowFixpoints),
      ),
      setting(
        Unicode.castArrowSym,
        "show casts",
        settings.show_casts,
        Evaluation(ShowCasts),
      ),
      setting(
        "🔍",
        "show lookup steps",
        settings.show_lookup_steps,
        Evaluation(ShowLookups),
      ),
      setting(
        "⏯️",
        "show stepper filters",
        settings.show_stepper_filters,
        Evaluation(ShowFilters),
      ),
      setting(
        "🤫",
        "show hidden steps",
        settings.show_hidden_steps,
        Evaluation(ShowHiddenSteps),
      ),
    ]),
    div(
      ~attr=
        Attr.many([
          Attr.class_("modal-back"),
          Attr.on_mousedown(_ =>
            inject(Update.Set(Evaluation(ShowSettings)))
          ),
        ]),
      [],
    ),
  ];
};

let stepper_view =
    (
      ~inject,
      ~settings: CoreSettings.Evaluation.t,
      ~font_metrics,
      ~result_key,
      stepper: Stepper.t,
    ) => {
  let button_back =
    Widgets.button_d(
      Icons.undo,
      inject(UpdateAction.StepperAction(result_key, StepBackward)),
      ~disabled=Stepper.undo_point(~settings, stepper.previous) == None,
      ~tooltip="Step Backwards",
    );
  let (hidden, previous) =
    if (settings.stepper_history) {
      Stepper.get_history(~settings, stepper);
    } else {
      ([], []);
    };
  let dh_code_previous = (step_with_previous: Stepper.step_with_previous) =>
    div(
      ~attr=Attr.classes(["result"]),
      [
        DHCode.view(
          ~inject,
          ~settings,
          ~selected_hole_instance=None,
          ~font_metrics,
          ~width=80,
          ~previous_step=step_with_previous.previous,
          ~chosen_step=Some(step_with_previous.step),
          ~hidden_steps=step_with_previous.hidden,
          ~result_key,
          step_with_previous.step.d,
        ),
      ],
    );
  let hide_stepper =
    Widgets.toggle(~tooltip="Show Stepper", "s", true, _ =>
      inject(UpdateAction.ToggleStepper(result_key))
    );
  let show_history =
    Widgets.toggle(~tooltip="Show History", "h", settings.stepper_history, _ =>
      inject(Set(Evaluation(ShowRecord)))
    );
  let eval_settings =
    Widgets.button(Icons.gear, _ => inject(Set(Evaluation(ShowSettings))));

  let rec previous_step =
          (~hidden=false, step: Stepper.step_with_previous): list(Node.t) => {
    [
      div(
        ~attr=
          Attr.classes(
            ["cell-item", "cell-result"] @ (hidden ? ["hidden"] : []),
          ),
        [
          div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
          dh_code_previous(step),
          div(
            ~attr=Attr.classes(["stepper-justification"]),
            [
              Node.text(
                Stepper.get_justification(step.step.knd)
                ++ (hidden ? " (hidden)" : ""),
              ),
            ],
          ),
        ],
      ),
    ]
    @ (
      (
        settings.show_hidden_steps
          ? List.map(
              step =>
                {step, previous: None, hidden: []}
                |> previous_step(~hidden=true),
              step.hidden,
            )
          : []
      )
      |> List.flatten
    );
  };
  let dh_code_current = d =>
    div(
      ~attr=Attr.classes(["result"]),
      [
        DHCode.view(
          ~inject,
          ~settings,
          ~selected_hole_instance=None,
          ~font_metrics,
          ~width=80,
          ~previous_step=
            previous
            |> List.nth_opt(_, 0)
            |> Option.map((x: Stepper.step_with_previous) => x.step),
          ~next_steps=Stepper.get_next_steps(stepper),
          ~hidden_steps=hidden,
          ~result_key,
          d,
        ),
      ],
    );

  let current =
    (
      (
        settings.show_hidden_steps
          ? List.map(
              step =>
                {step, previous: None, hidden: []}
                |> previous_step(~hidden=true),
              hidden,
            )
          : []
      )
      |> List.flatten
      |> List.rev
    )
    @ [
      switch (stepper.current) {
      | StepperOK(d, _) =>
        div(
          ~attr=Attr.classes(["cell-item", "cell-result"]),
          [
            div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
            dh_code_current(d),
            button_back,
            eval_settings,
            show_history,
            hide_stepper,
          ],
        )
      // TODO[Matt]: show errors and waiting
      | StepTimeout
      | StepPending(_, _, _) => div([])
      },
    ];
  let nodes_previous = List.map(previous_step, previous) |> List.flatten;
  List.fold_left((x, y) => List.cons(y, x), current, nodes_previous)
  @ (settings.show_settings ? settings_modal(~inject, settings) : []);
};
