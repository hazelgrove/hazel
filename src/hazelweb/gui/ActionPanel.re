open Virtual_dom.Vdom;

let action_panel = (children: list(Node.t)): Node.t => {
  let panel_title =
    Node.div(
      [Attr.classes(["panel-title-bar", "title-bar"])],
      [Node.text("Available Edit Actions")],
    );

  let panel_body =
    Node.div([Attr.classes(["action-panel-body", "panel-body"])], children);

  Node.div(
    [Attr.classes(["action-panel", "panel"])],
    [panel_title, panel_body],
  );
};

let sub_panel = (title: string, children: list(Node.t)): Node.t => {
  Node.div(
    [Attr.classes(["sub-panel", "sub-panel-default"])],
    [
      Node.div([Attr.classes(["sub-panel-title"])], [Node.text(title)]),
      Node.div([Attr.classes(["sub-panel-body"])], children),
    ],
  );
};

let action_label = (~attrs=[], children) => {
  Node.div(
    [Attr.classes(["action-label", "info-label"]), ...attrs],
    children,
  );
};

let info_button = (can_perform, lbl) =>
  Node.div(
    [
      Attr.classes(
        can_perform
          ? ["action-panel-entry", "action-enabled"]
          : ["action-panel-entry", "action-disabled"],
      ),
    ],
    [action_label(lbl)],
  );

let mono_text = content => {
  Node.span(
    [Attr.classes(["code"]), Attr.style(Css_gen.font_size(`Inherit))],
    [Node.text(content)],
  );
};

let action_button =
    (
      is_action_allowed: Action.t => bool,
      inject: ModelAction.t => Event.t,
      a: Action.t,
      lbl: list(Node.t),
      key_combo: KeyCombo.t,
    ) => {
  let can_perform = is_action_allowed(a);
  Node.div(
    [
      Attr.classes(
        can_perform
          ? ["action-panel-entry", "action-enabled"]
          : ["action-panel-entry", "action-disabled"],
      ),
      Attr.on_click(_ => inject(ModelAction.EditAction(a))),
      Attr.on_keydown(evt =>
        if (KeyCombo.matches(key_combo, evt)) {
          Event.Many([
            inject(ModelAction.EditAction(a)),
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
        [Node.text(KeyCombo.name(key_combo))],
      ),
    ],
  );
};

let brown_label = body => {
  Node.div(
    [
      Attr.classes(["keyboard-shortcut", "action-enabled"]),
      Attr.style(
        Css_gen.(
          create(~field="display", ~value="inline-block")
          @> create(~field="border-bottom", ~value="none")
        ),
      ),
    ],
    body,
  );
};

let keyboard_button = (is_action_allowed, ~inject, ~action, ~combo) => {
  Node.div(
    [
      Attr.classes(
        is_action_allowed(action)
          ? ["keyboard-shortcut", "action-enabled"]
          : ["keyboard-shortcut", "action-disabled"],
      ),
      Attr.on_click(_ => inject(ModelAction.EditAction(action))),
      Attr.style(Css_gen.create(~field="display", ~value="inline-block")),
      Attr.on_keydown(evt =>
        if (KeyCombo.matches(combo, evt)) {
          Event.Many([
            inject(ModelAction.EditAction(action)),
            Event.Prevent_default,
          ]);
        } else {
          Event.Prevent_default;
        }
      ),
    ],
    [Node.text(KeyCombo.name(combo))],
  );
};

let flex_grow = Attr.style(Css_gen.(flex_item(~grow=1., ())));

let action_list =
    (
      is_action_allowed: Action.t => bool,
      inject: ModelAction.t => Event.t,
      actions: list((KeyCombo.t, Action.t)),
      label: string,
    ) => {
  let item = ((combo, action)) => {
    keyboard_button(is_action_allowed, ~inject, ~action, ~combo);
  };
  let display_flex =
    Attr.style(Css_gen.create(~field="display", ~value="flex"));

  let label =
    Node.div(
      [Attr.classes(["action-label"]), flex_grow],
      [Node.text(label)],
    );
  let items = List.map(item, actions);
  Node.div(
    [Attr.classes(["action-panel-entry"]), display_flex],
    [label, ...items],
  );
};

let generate_panel_body = (is_action_allowed, cursor_info, inject) => {
  let text = Node.text;
  let simple = desc => [Node.text(desc)];

  let section = (title, children) => {
    sub_panel(title, children);
  };

  let action_of_combo = combo =>
    switch (KeyComboAction.get_model_action_from_kc(cursor_info, combo)) {
    | Some(EditAction(action)) => action
    | _ =>
      failwith(
        __LOC__
        ++ ": "
        ++ HazelKeyCombos.name(combo)
        ++ " does not correspond to an EditAction in KeyComboAction.get_model_action_from_kc",
      )
    };

  let combo_element = (is_allowed_action, combo, description) => {
    let action = action_of_combo(combo);
    action_button(
      is_allowed_action,
      inject,
      action,
      description,
      HazelKeyCombos.get_details(combo),
    );
  };

  let combo = combo_element(is_action_allowed);

  let info = text => {
    info_button(true, text);
  };

  let info_action = (text, action) => {
    info_button(is_action_allowed(action), text);
  };

  let is_action_allowed_with_on_type_check = (~on_type, action) => {
    switch (cursor_info.typed) {
    | OnType when on_type => is_action_allowed(action)
    | OnType => false
    | _ when on_type => false
    | _ => is_action_allowed(action)
    };
  };

  let combo_and_cursor = (~on_type, combo, description) => {
    let is_allowed = is_action_allowed_with_on_type_check(~on_type);
    combo_element(is_allowed, combo, description);
  };

  let operator_list = (~on_type, text, combos) => {
    let actions =
      List.map(
        combo => {
          let action = action_of_combo(combo);
          (HazelKeyCombos.get_details(combo), action);
        },
        combos,
      );
    let is_action_allowed = is_action_allowed_with_on_type_check(~on_type);
    action_list(is_action_allowed, inject, actions, text);
  };

  let keyboard_button = combo => {
    let action = action_of_combo(combo);
    let combo = HazelKeyCombos.get_details(combo);
    keyboard_button(is_action_allowed, ~inject, ~combo, ~action);
  };

  let display_inline_block =
    Attr.(style(Css_gen.(create(~field="display", ~value="inline-block"))));

  let spaced_line = children => {
    Node.div(
      Attr.[
        classes(["action-panel-entry"]),
        style(
          Css_gen.(
            create(~field="display", ~value="flex")
            @> create(~field="justify-content", ~value="space-between")
          ),
        ),
      ],
      children,
    );
  };

  let single_line_multiple_actions = (description, elems) => {
    let label = action_label(~attrs=[flex_grow], [text(description)]);
    let elems = Node.div([display_inline_block], elems);
    spaced_line([label, elems]);
  };

  HazelKeyCombos.[
    section(
      "Movement",
      [
        info([text("Move using arrow keys")]),
        single_line_multiple_actions(
          "Move to next / previous hole",
          [keyboard_button(Tab), keyboard_button(ShiftTab)],
        ),
      ],
    ),
    section(
      "General Editing",
      [
        single_line_multiple_actions(
          "Backspace / Delete",
          [keyboard_button(Backspace), keyboard_button(Delete)],
        ),
        single_line_multiple_actions(
          "Swap line up / down",
          [keyboard_button(Alt_Up), keyboard_button(Alt_Down)],
        ),
        single_line_multiple_actions(
          "Swap operand left / right",
          [keyboard_button(Alt_Left), keyboard_button(Alt_Right)],
        ),
        combo(Enter, simple("Create new line ")),
        single_line_multiple_actions(
          "Create new comment line",
          [keyboard_button(Pound), keyboard_button(Shift_Enter)],
        ),
        combo(LeftParen, simple("Parenthesize")),
      ],
    ),
    section(
      "Variables",
      [
        info([
          text("Variable regex: "),
          mono_text("[_a-zA-Z][_a-zA-Z0-9']*"),
        ]),
        info_action(
          [
            text("Type "),
            mono_text("\"let \""),
            text(" to enter a let expression"),
          ],
          Action.Construct(SLet),
        ),
        combo(Colon, simple("Type annotation")),
      ],
    ),
    section(
      "Booleans",
      [
        info([
          text("Enter boolean literals "),
          mono_text("true"),
          text(" and "),
          mono_text("false"),
          text(" directly"),
        ]),
        info_action(
          [
            text("Type "),
            mono_text("\"B\""),
            text(" to insert a Bool type"),
          ],
          Action.Construct(SChar("B")),
        ),
        operator_list(~on_type=false, "Operators", [Ampersand, VBar]),
      ],
    ),
    section(
      "Arithmetic",
      [
        info([
          text("Enter number literals directly e.g. "),
          mono_text("1.0, 2"),
        ]),
        info_action(
          [
            text("Type "),
            mono_text("\"I\""),
            text(" to insert an "),
            mono_text("Int"),
            text(" type"),
          ],
          Action.Construct(SChar("I")),
        ),
        info_action(
          [
            text("Type "),
            mono_text("\"F\""),
            text(" to insert a "),
            mono_text("Float"),
            text(" type"),
          ],
          Action.Construct(SChar("F")),
        ),
        operator_list(
          ~on_type=false,
          "Integer operators",
          [Plus, Minus, Asterisk, Slash, LT, GT, Equals],
        ),
        single_line_multiple_actions(
          "Floating point operators",
          [
            brown_label([mono_text("+.")]),
            brown_label([mono_text("-.")]),
            brown_label([mono_text("*.")]),
            brown_label([mono_text("/.")]),
            brown_label([mono_text("<.")]),
            brown_label([mono_text(">.")]),
            brown_label([mono_text("=.")]),
          ],
        ),
      ],
    ),
    section(
      "Lists",
      [
        combo_and_cursor(
          ~on_type=false,
          LeftBracket,
          [text("Insert "), mono_text("[] (nil)")],
        ),
        combo(Semicolon, simple("Cons operator")),
        combo_and_cursor(
          ~on_type=true,
          LeftBracket,
          simple("Insert type List"),
        ),
      ],
    ),
    section(
      "Sum Types",
      [
        combo(Alt_L, simple("Left injection")),
        combo(Alt_R, simple("Right injection")),
        combo(VBar, simple("Insert | type operator")),
      ],
    ),
    section(
      "Functions",
      [
        combo(Backslash, simple("Insert lambda function")),
        combo(Space, simple("Apply function")),
        combo_and_cursor(~on_type=true, GT, [text("Create an arrow type")]),
      ],
    ),
    section(
      "Tuples",
      [
        combo(Comma, simple("Create a tuple")),
        combo_and_cursor(
          ~on_type=true,
          Comma,
          [text("Create a tuple type")],
        ),
      ],
    ),
    section(
      "Pattern Matching",
      [
        info([
          text("Type \""),
          mono_text("case "),
          text("\" to add a case expression"),
        ]),
        combo(Alt_C, simple("Insert case expression")),
        combo(Enter, simple("Add new rule")),
      ],
    ),
  ];
};

let view = (~inject: ModelAction.t => Event.t, model: Model.t) => {
  let edit_state = Model.get_edit_state(model);
  let cursor_info = Model.get_cursor_info(model);

  let is_action_allowed = (a: Action.t): bool => {
    switch (Action_Exp.syn_perform(Contexts.initial, a, edit_state)) {
    | Failed => false
    | CursorEscaped(_)
    | Succeeded(_) => true
    };
  };

  let body = generate_panel_body(is_action_allowed, cursor_info, inject);

  action_panel(body);
} /* This function is unused at runtime, its purpose is to catch
  * new cases added to the Acion.t type, but forgotten about in this
  * side pane. If you add a new action, please update the code above
  * inside generate_panel_body with a description of the new action.
  * Afterwards the below function can be updated to not error anymore.
  *
  * Also consider looking at Cell.re to see if a keyboard shortcut
  * should be added for that particular action as well.
 */;

type ack_checkin =
  | Added;

let _check_actions = (a: Action.t) =>
  switch (a) {
  /* Used */
  | Backspace => Added
  | Delete => Added
  | MoveToPrevHole => Added
  | MoveToNextHole => Added
  | Construct(SOp(SArrow)) => Added
  | Construct(SOp(SGreaterThan)) => Added
  | Construct(SOp(SAnd)) => Added
  | Construct(SOp(SOr)) => Added
  | Construct(SParenthesized) => Added
  | Construct(SCloseParens) => Added
  | Construct(SCloseBraces) => Added
  | Construct(SCloseSquareBracket) => Added
  | Construct(SAnn) => Added
  | Construct(SOp(SEquals)) => Added
  | Construct(SLine) => Added
  | Construct(SCommentLine) => Added
  | Construct(SFun) => Added
  | Construct(SOp(SPlus)) => Added
  | Construct(SOp(SMinus)) => Added
  | Construct(SOp(STimes)) => Added
  | Construct(SOp(SDivide)) => Added
  | Construct(SOp(SLessThan)) => Added
  | Construct(SOp(SSpace)) => Added
  | Construct(SOp(SComma)) => Added
  | Construct(SList) => Added
  | Construct(SListNil) => Added
  | Construct(SOp(SCons)) => Added
  | Construct(SInj(L)) => Added
  | Construct(SInj(R)) => Added
  | Construct(SCase) => Added
  | Construct(SLet) => Added
  | Construct(SOp(SVBar)) => Added
  | Construct(SChar(_)) => Added
  | SwapUp => Added
  | SwapDown => Added
  | SwapLeft => Added
  | SwapRight => Added
  | MoveLeft => Added
  | MoveRight => Added /* Not added */
  | MoveTo(_) => Added
  | Init => Added
  };
