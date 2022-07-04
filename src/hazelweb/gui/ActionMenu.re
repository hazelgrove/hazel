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
  {
    label: "Serialize compile result to console",
    shortcut: None,
    action: SerializeToConsole(Grain),
  },
  {label: "Toggle left sidebar", shortcut: None, action: ToggleLeftSidebar},
  {label: "Toggle right sidebar", shortcut: None, action: ToggleRightSidebar},
  {label: "Open text editor", shortcut: None, action: ToggleTextEditorPopup},
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

let settings_panel =
    (~inject: ModelAction.t => Ui_event.t, settings: Settings.t) => {
  SettingsPanel.view(~inject, settings);
};

let separator = hr([Attr.classes(["separator"])]);

let dropdown = (~inject: ModelAction.t => Ui_event.t, settings: Settings.t) => {
  create(
    "details",
    [],
    [
      create("summary", [], [text("☰")]),
      ul(
        [Attr.classes(["dropdown-content"])],
        dropdown_options(~inject)
        @ [separator, settings_panel(~inject, settings)],
      ),
    ],
  );
};

let view = (~inject: ModelAction.t => Ui_event.t, settings: Settings.t) =>
  div([Attr.classes(["dropdown"])], [dropdown(~inject, settings)]);
