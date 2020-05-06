module Vdom = Virtual_dom.Vdom;
module KeyCombo = JSUtil.KeyCombo;

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
      [Node.div([Attr.classes(["action-label", "info-label"])], lbl)],
    )
  );

let mono_text = content => {
  Vdom.(
    Node.span(
      [Attr.classes(["code"]), Attr.style(Css_gen.font_size(`Inherit))],
      [Node.text(content)],
    )
  );
};

let action_button =
    (
      is_action_allowed: Action.t => bool,
      inject: Update.Action.t => Vdom.Event.t,
      a: Action.t,
      lbl: list(Vdom.Node.t),
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
        Node.div([Attr.classes(["action-label"])], lbl),
        Node.div(
          [Attr.classes(["keyboard-shortcut"])],
          [Node.text(KeyCombo.Details.name(key_combo))],
        ),
      ],
    )
  );
};

let action_list =
    (
      is_action_allowed: Action.t => bool,
      inject: Update.Action.t => Vdom.Event.t,
      actions: list((KeyCombo.Details.t, Action.t)),
      label: string,
    ) => {
  let item = ((combo, action)) => {
    Vdom.(
      Node.div(
        [
          Attr.classes(
            is_action_allowed(action)
              ? ["keyboard-shortcut", "action-enabled"]
              : ["keyboard-shortcut", "action-disabled"],
          ),
          Attr.on_click(_ => inject(Update.Action.EditAction(action))),
          Attr.on_keydown(evt =>
            if (KeyCombo.Details.matches(combo, evt)) {
              Event.Many([
                inject(Update.Action.EditAction(action)),
                Event.Prevent_default,
              ]);
            } else {
              Event.Prevent_default;
            }
          ),
        ],
        [Node.text(KeyCombo.Details.name(combo))],
      )
    );
  };
  let flex_grow = Vdom.Attr.style(Css_gen.(flex_item(~grow=1., ())));
  let display_flex =
    Vdom.Attr.style(Css_gen.create(~field="display", ~value="flex"));
  let label =
    Vdom.(
      Node.div(
        [Attr.classes(["action-label"]), flex_grow],
        [Node.text(label)],
      )
    );
  let items = List.map(item, actions);
  Vdom.(
    Node.div(
      [Attr.classes(["action-panel-entry"]), display_flex],
      [label, ...items],
    )
  );
};

let generate_panel_body = (is_action_allowed, cursor_info, inject) => {
  // Helper functions
  let text = Vdom.Node.text;
  let simple = desc => [Vdom.Node.text(desc)];

  let section = (title, children) => {
    sub_panel(title, children);
  };

  let combo_element = (is_allowed_action, combo, description) => {
    let action = Hashtbl.find(Cell.kc_actions, combo);
    let action = action(cursor_info);
    action_button(
      is_allowed_action,
      inject,
      action,
      description,
      KeyCombo.get_details(combo),
    );
  };

  let combo = combo_element(is_action_allowed);

  let info = text => {
    info_button(true, text);
  };

  let info_action = (text, action) => {
    info_button(is_action_allowed(action), text);
  };

  let combo_and_cursor_on_type = (combo, description) => {
    let is_allowed = action => {
      switch (cursor_info.typed) {
      | OnType => is_action_allowed(action)
      | _ => false
      };
    };
    combo_element(is_allowed, combo, description);
  };

  let combo_and_cursor_not_on_type = (combo, description) => {
    let is_allowed = action => {
      switch (cursor_info.typed) {
      | OnType => false
      | _ => is_action_allowed(action)
      };
    };
    combo_element(is_allowed, combo, description);
  };

  let operator_list = combos => {
    let actions =
      List.map(
        combo => {
          let action = Hashtbl.find(Cell.kc_actions, combo, cursor_info);
          (KeyCombo.get_details(combo), action);
        },
        combos,
      );
    action_list(is_action_allowed, inject, actions, "Operators");
  };

  KeyCombo.[
    section(
      "Movement",
      [
        info([text("Move using arrow keys")]),
        combo(ShiftTab, simple("Move to previous hole")),
        combo(Tab, simple("Move to next hole")),
        combo(Ctrl_Alt_Up, simple("Swap expression up")),
        combo(Ctrl_Alt_Down, simple("Swap expression down")),
        combo(Ctrl_Alt_Left, simple("Swap expression left")),
        combo(Ctrl_Alt_Right, simple("Swap expression right")),
        combo(Enter, simple("Create new line")),
        combo(Delete, simple("Delete expression")),
      ],
    ),
    section(
      "Arithmetic",
      [
        info([text("Enter number literals directly")]),
        operator_list([Plus, Minus, Asterisk]),
      ],
    ),
    section(
      "Variables",
      [
        info([text("Enter variables directly")]),
        info_action(
          [
            text("Type "),
            mono_text("\"let \""),
            text(" to enter a let expression"),
          ],
          Action.Construct(SLet),
        ),
        combo(Backspace, simple("Delete character")),
        combo(Colon, simple("Type ascription")),
      ],
    ),
    section(
      "Booleans",
      [
        info([
          text("Enter boolean literals "),
          mono_text("\"true\", \"false\""),
          text(" directly"),
        ]),
        operator_list([LT, GT, Ampersand, Equals]),
      ],
    ),
    section(
      "Lists",
      [
        combo_and_cursor_not_on_type(
          LeftBracket,
          simple("Insert Empty List (nil)"),
        ),
        combo(Semicolon, simple("Cons operator")),
      ],
    ),
    section(
      "Injections",
      [
        combo(Alt_L, simple("Left injunction")),
        combo(Alt_R, simple("Right injunction")),
      ],
    ),
    section(
      "Types",
      [
        info_action(
          [text("Use Shift+N to insert the Num type")],
          Action.Construct(SChar("N")),
        ),
        info_action(
          [text("Use Shift+B to insert the Bool type")],
          Action.Construct(SChar("B")),
        ),
        combo(Colon, simple("Type ascription")),
        combo(VBar, simple("Insert | operator")),
        combo_and_cursor_on_type(LeftBracket, simple("Insert type List")),
        combo_and_cursor_on_type(GT, [text("Create a type arrow")]),
      ],
    ),
    section(
      "Functions",
      [
        combo(Backslash, simple("Insert Lambda expression")),
        combo(Space, simple("Separate terms")),
      ],
    ),
    section(
      "Miscellaneous",
      [
        combo(LeftParen, simple("Parenthesize expression")),
        combo(Comma, simple("Insert , operator")),
        combo(Alt_C, simple("Insert case expression")),
      ],
    ),
  ];
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

  let body = generate_panel_body(is_action_allowed, cursor_info, inject);

  action_panel(body);
};
