open Virtual_dom.Vdom;
open Node;
let logo_panel =
  a(
    [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
    [text("Hazel")],
  );

let top_bar = (~inject: ModelAction.t => Ui_event.t, ~model: Model.t) => {
  div(
    [Attr.classes(["top-bar"])],
    [
      logo_panel,
      CardsPanel.view(~inject, ~model),
      ActionMenu.view(~inject),
    ],
  );
};

let prev_eva_button = (~inject, model: Model.t) => {
  let show_button = model.result_states == [] ? [Attr.disabled] : [];
  Node.button(
    [
      Attr.id("step-mode-prev-button"),
      Attr.on_click(_ => inject(ModelAction.PrevEvaluate)),
      ...show_button,
    ],
    [Node.text("Previous Evaluation")],
  );
};

let cell_status_panel = (~settings: Settings.t, ~model: Model.t, ~inject) => {
  let program = Model.get_program(model);
  let selected_instance = Model.get_selected_hole_instance(model);
  let (_, ty, _) = program.edit_state;
  let result =
    settings.evaluation.show_unevaluated_expansion
      ? program |> Program.get_expansion
      : settings.evaluation.evaluator_type == Evaluator
          ? program |> Program.get_result |> Result.get_dhexp
          : Model.get_result_state(model);
  div(
    [],
    [
      div(
        [Attr.classes(["cell-status"])],
        [
          div(
            [Attr.classes(["type-indicator"])],
            [
              div(
                [Attr.classes(["type-label"])],
                [text("Result of type: ")],
              ),
              div([Attr.classes(["htype-view"])], [HTypCode.view(ty)]),
            ],
          ),
        ],
      ),
      Node.div(
        [Attr.classes(["result-view"])],
        [
          DHCode.view(
            ~inject,
            ~selected_instance,
            ~settings=settings.evaluation,
            ~show_steppable=true,
            ~width=80,
            ~font_metrics=model.font_metrics,
            result,
          ),
        ],
      ),
      Node.div(
        [Attr.classes(["step-evaluate-control"])],
        settings.evaluation.evaluator_type != Evaluator
        && settings.evaluation.stepper_mode
          ? [prev_eva_button(~inject, model)] : [],
      ),
    ]
    @ (
      if (settings.evaluation.show_evaluate_steps) {
        List.map(
          d =>
            Node.div(
              [Attr.classes(["step-view"])],
              [
                DHCode.view(
                  ~inject,
                  ~selected_instance,
                  ~settings=settings.evaluation,
                  ~width=80,
                  ~font_metrics=model.font_metrics,
                  d,
                ),
              ],
            ),
          program
          |> Program.get_evaluate_steps(
               _,
               settings.evaluation.step_evaluator_option,
             ),
        );
      } else {
        [];
      }
    ),
  );
};

let left_sidebar = (~inject: ModelAction.t => Event.t, ~model: Model.t) =>
  Sidebar.left(~inject, ~is_open=model.left_sidebar_open, () =>
    [ActionPanel.view(~inject, model)]
  );

let right_sidebar = (~inject: ModelAction.t => Event.t, ~model: Model.t) => {
  let settings = model.settings;
  let program = Model.get_program(model);
  let selected_instance = Model.get_selected_hole_instance(model);
  Sidebar.right(~inject, ~is_open=model.right_sidebar_open, () =>
    [
      CursorInspector.view(~inject, model),
      ContextInspector.view(
        ~inject,
        ~selected_instance,
        ~settings=settings.evaluation,
        ~font_metrics=model.font_metrics,
        program,
      ),
      UndoHistoryPanel.view(~inject, model),
      SettingsPanel.view(~inject, settings),
    ]
  );
};

let view = (~inject: ModelAction.t => Event.t, model: Model.t) => {
  let settings = model.settings;
  TimeUtil.measure_time(
    "Page.view",
    settings.performance.measure && settings.performance.page_view,
    () => {
      let card_caption = Model.get_card(model).info.caption;
      let cell_status =
        !settings.evaluation.evaluate
          ? div([], []) : cell_status_panel(~settings, ~model, ~inject);
      div(
        [Attr.id("root")],
        [
          top_bar(~inject, ~model),
          div(
            [Attr.classes(["main-area"])],
            [
              left_sidebar(~inject, ~model),
              div(
                [Attr.classes(["flex-wrapper"])],
                [
                  div(
                    [Attr.id("page-area")],
                    [
                      div(
                        [Attr.classes(["page"])],
                        [
                          div(
                            [Attr.classes(["card-caption"])],
                            [card_caption],
                          ),
                          Cell.view(~inject, model),
                          cell_status,
                        ],
                      ),
                      div(
                        [
                          Attr.style(
                            Css_gen.(
                              white_space(`Pre) @> font_family(["monospace"])
                            ),
                          ),
                        ],
                        [],
                      ),
                    ],
                  ),
                ],
              ),
              right_sidebar(~inject, ~model),
            ],
          ),
        ],
      );
    },
  );
};
