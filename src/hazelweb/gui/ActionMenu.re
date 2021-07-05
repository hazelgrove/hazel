open Virtual_dom.Vdom;
open Node;

type menu_entry = {
  label: string,
  shortcut: option(string),
  action: ModelAction.t,
};

let menu_entries = (is_mac: bool): list(menu_entry) => [
  {
    label: "Serialize to console",
    shortcut: Some(HazelKeyCombos.name(Ctrl_S, is_mac)),
    action: SerializeToConsole,
  },
  {label: "Toggle left sidebar", shortcut: None, action: ToggleLeftSidebar},
  {label: "Toggle right sidebar", shortcut: None, action: ToggleRightSidebar},
];

let dropdown_option = (~inject, {label, shortcut, action}: menu_entry) => {
  let shortcut_view =
    switch (shortcut) {
    | None => []
    | Some(s) => [div([Attr.classes(["shortcut"])], [text(s)])]
    };
  li([Attr.on_click(_ => inject(action))], [text(label)] @ shortcut_view);
};

let dropdown_options = (~inject, ~is_mac) =>
  List.map(dropdown_option(~inject), menu_entries(is_mac));

let dropdown = (~inject: ModelAction.t => Ui_event.t, ~model: Model.t) => {
  create(
    "details",
    [],
    [
      create("summary", [], [text("☰")]),
      ul(
        [Attr.classes(["dropdown-content"])],
        dropdown_options(~inject, ~is_mac=model.is_mac),
      ),
    ],
  );
};

let view = (~inject: ModelAction.t => Ui_event.t, ~model: Model.t) =>
  div([Attr.classes(["dropdown"])], [dropdown(~inject, ~model)]);
