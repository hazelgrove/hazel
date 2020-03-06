module Vdom = Virtual_dom.Vdom;
module KeyCombo = JSUtil.KeyCombo;

// Mini-DSL for the panel UI
let ui_elements = {
  KeyCombo.[
    `Panel((
      "Movement",
      [`Info("Move using arrow keys"), `Combo(ShiftTab), `Combo(Tab)],
    )),
    `Panel((
      "Arithmetic",
      [
        `Info("Enter number literals directly"),
        `Combo(Plus),
        `Combo(Minus),
        `Combo(Asterisk),
      ],
    )),
    `Panel((
      "Variables",
      [
        `Info("Enter variables directly"),
        `Info("Type \"let\" to enter a let expression"),
      ],
    )),
    `Panel((
      "Booleans",
      [
        `Info("Enter boolean literals directly \"true\", \"false\""),
        `Combo(GT),
        `Combo(LT),
        `Combo(Ampersand),
        `Combo(Equals),
      ],
    )),
    `Panel(("Lists", [`Combo(LeftBracket), `Combo(Semicolon)])),
    `Panel(("Injections", [`Combo(Alt_L), `Combo(Alt_R)])),
    `Panel((
      "Miscellaneous",
      [
        `Combo(LeftParen),
        `Combo(VBar),
        `Combo(Enter),
        `Combo(Backslash),
        `Combo(Space),
        `Combo(Comma),
        `Combo(Alt_C),
        `Combo(Delete),
        `Combo(Backspace),
        `Combo(Colon),
      ],
    )),
  ];
};

let action_panel = (children: list(Vdom.Node.t)): Vdom.Node.t => {
  open Vdom;
  let panel_title =
    Node.div(
      [Attr.classes(["panel-title-bar", "title-bar"])],
      [Node.text("Edit Actions")],
    );

  let panel_body =
    Node.div([Attr.classes(["action-panel-body", "panel-body"])], children);

  Node.div(
    [Attr.classes(["action-panel", "panel"])],
    [panel_title, panel_body],
  );
};

let sub_panel = (title: string, children: list(Vdom.Node.t)): Vdom.Node.t => {
  Vdom.(
    Node.div(
      [Attr.classes(["sub-panel", "sub-panel-default"])],
      [
        Node.div([Attr.classes(["sub-panel-title"])], [Node.text(title)]),
        Node.div([Attr.classes(["sub-panel-body"])], children),
      ],
    )
  );
};

let info_button = (can_perform, lbl) =>
  Vdom.(
    Node.div(
      [
        Attr.classes(
          can_perform
            ? ["action-panel-entry", "action-enabled"]
            : ["action-panel-entry", "action-disabled"],
        ),
      ],
      [
        Node.div(
          [Attr.classes(["action-label", "info-label"])],
          [Node.text(lbl)],
        ),
      ],
    )
  );

let action_button =
    (
      is_action_allowed: Action.t => bool,
      inject: Update.Action.t => Vdom.Event.t,
      a: Action.t,
      lbl,
      key_combo,
    ) => {
  let can_perform = is_action_allowed(a);
  Vdom.(
    Node.div(
      [
        Attr.classes(
          can_perform
            ? ["action-panel-entry", "action-enabled"]
            : ["action-panel-entry", "action-disabled"],
        ),
        Attr.on_click(_ => inject(Update.Action.EditAction(a))),
        Attr.on_keydown(evt =>
          if (KeyCombo.Details.matches(key_combo, evt)) {
            Event.Many([
              inject(Update.Action.EditAction(a)),
              Event.Prevent_default,
            ]);
          } else {
            Event.Prevent_default;
          }
        ),
      ],
      [
        Node.div([Attr.classes(["action-label"])], [lbl]),
        Node.div(
          [Attr.classes(["keyboard-shortcut"])],
          [Node.text(KeyCombo.Details.name(key_combo))],
        ),
      ],
    )
  );
};

let view = (~inject: Update.Action.t => Vdom.Event.t, model: Model.t) => {
  let edit_state = Model.get_edit_state(model);
  let cursor_info = Model.get_cursor_info(model);

  let is_action_allowed = (a: Action.t): bool => {
    switch (Action.Exp.syn_perform(Contexts.empty, a, edit_state)) {
    | Failed => false
    | CursorEscaped(_)
    | Succeeded(_) => true
    };
  };

  let action_button = action_button(is_action_allowed, inject);

  let panel_body_to_div = body => {
    switch (body) {
    | `Info(text) => info_button(true, text)
    | `Combo(combo) =>
      let info = Hashtbl.find(Cell.kc_actions, combo);
      let action = info.action_fn(cursor_info);
      let description = Vdom.Node.text(info.description);
      action_button(action, description, KeyCombo.get_details(combo));
    };
  };

  let ui_element_to_div = element => {
    switch (element) {
    | `Panel(title, children) =>
      let children = List.map(panel_body_to_div, children);
      sub_panel(title, children);
    };
  };

  let body = List.map(ui_element_to_div, ui_elements);

  action_panel(body);
};
