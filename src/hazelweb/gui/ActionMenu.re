open Virtual_dom.Vdom;
open Node;

type menu_entry = {
  label: string,
  shortcut: option(string),
  action: ModelAction.t,
};

let menu_entries: list(menu_entry) = [
  {
    label: "Serialize to console",
    shortcut: Some(HazelKeyCombos.name(Ctrl_S)),
    action: SerializeToConsole(UHExp),
  },
  {
    label: "Serialize result to console",
    shortcut: None,
    action: SerializeToConsole(DHExp),
  },
  {
    label: "Serialize Zexp to console",
    shortcut: Some(HazelKeyCombos.name(Ctrl_Shift_S)),
    action: SerializeToConsole(ZExp),
  },
  {label: "Toggle left sidebar", shortcut: None, action: ToggleLeftSidebar},
  {label: "Toggle right sidebar", shortcut: None, action: ToggleRightSidebar},
  {label: "Open import popup", shortcut: None, action: ToggleImportPopup},
  {label: "Open export popup", shortcut: None, action: ToggleExportPopup},
];

let dropdown_option = (~inject, {label, shortcut, action}: menu_entry) => {
  let shortcut_view =
    switch (shortcut) {
    | None => []
    | Some(s) => [div([Attr.classes(["shortcut"])], [text(s)])]
    };
  li([Attr.on_click(_ => inject(action))], [text(label)] @ shortcut_view);
};

let dropdown_options = (~inject) =>
  List.map(dropdown_option(~inject), menu_entries);

let dropdown = (~inject: ModelAction.t => Ui_event.t) => {
  create(
    "details",
    [],
    [
      create("summary", [], [text("☰")]),
      ul([Attr.classes(["dropdown-content"])], dropdown_options(~inject)),
    ],
  );
};

let view = (~inject: ModelAction.t => Ui_event.t) =>
  div([Attr.classes(["dropdown"])], [dropdown(~inject)]);
