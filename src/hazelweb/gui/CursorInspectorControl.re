open Virtual_dom.Vdom;
open Node;

let string_of_cursor_inspector_mode =
    (mode: option(Model.cursor_inspector_mode)) =>
  switch (mode) {
  | Some(Simple) => "inspector"
  | Some(Assistant) => "assistant"
  | Some(Tutor) => "tutor"
  | None => "off"
  };

let radio_button = checked =>
  create(
    "img",
    [
      Attr.classes(["radio-button"] @ (checked ? ["checked"] : [])),
      Attr.create(
        "src",
        "imgs/assistant/radio_button_" ++ (checked ? "on" : "off") ++ ".svg",
      ),
    ],
    [],
  );

let ci_mode_radio =
    (
      mode,
      current_mode,
      ~body: list(Node.t)=[],
      ~sort: TermSort.t,
      ~inject: ModelAction.t => Event.t,
    ) => {
  let set_mode = _ =>
    Event.Many([
      Event.Prevent_default,
      Event.Stop_propagation,
      inject(
        Chain([FocusCell(MainProgram), SetCursorInspectorMode(mode)]),
      ),
    ]);
  let mode_str = string_of_cursor_inspector_mode(mode);
  let sort_str = TermSort.to_string(sort);
  div(
    [Attr.classes(["mode", sort_str]), Attr.on_click(set_mode)],
    [radio_button(mode == current_mode), span([], [text(mode_str)])]
    @ body,
  );
};

let view =
    (
      curent_mode: option(Model.cursor_inspector_mode),
      ~sort: TermSort.t,
      ~inject,
    )
    : Node.t => {
  let menu_option = (~body=[], mode) =>
    ci_mode_radio(mode, curent_mode, ~sort, ~body, ~inject);
  let menu_title =
    div(
      [Attr.id("ci-control-pane-mode-switch")],
      [
        text("Toggle inspector"),
        div([Attr.class_("key")], [text("CTRL-SPACE")]),
      ],
    );
  let close_key =
    div(
      [Attr.id("ci-control-pane-close"), Attr.class_("key")],
      [text("ESC")],
    );
  let pane_contents = [
    menu_title,
    hr([]),
    menu_option(Some(Assistant)),
    menu_option(Some(Tutor)),
    menu_option(Some(Simple)),
    menu_option(None, ~body=[close_key]),
  ];
  div(
    [Attr.class_("ci-control-pane-wrapper")],
    [
      div(
        [Attr.class_("speech-bubble-wrapper")],
        [
          div(
            [Attr.id("ci-mode"), Attr.class_("ci-control-pane")],
            pane_contents,
          ),
        ],
      ),
    ],
  );
};
