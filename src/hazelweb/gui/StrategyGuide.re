open Virtual_dom.Vdom;

let code_node = text =>
  Node.div([Attr.classes(["code-font"])], [Node.text(text)]);

let example_lit_node = text =>
  Node.div([Attr.classes(["code-font", "example"])], [Node.text(text)]);

let keyword_node = text =>
  Node.div([Attr.classes(["code-font", "keyword"])], [Node.text(text)]);

let option = nodes => Node.div([Attr.classes(["option"])], nodes);
let mini_option = nodes => Node.div([Attr.classes(["mini-option"])], nodes);
let fill_space = Node.span([Attr.classes(["filler"])], []);
let lit_msg = (ty: HTyp.t) => {
  let int_lit =
    option([
      Node.text("Enter an Integer Literal"),
      fill_space,
      Node.text("(e.g. "),
      example_lit_node("1"),
      Node.text(")"),
    ]);
  let float_lit =
    option([
      Node.text("Enter a Floating Point Literal"),
      fill_space,
      Node.text("(e.g. "),
      example_lit_node("1.0"),
      Node.text(")"),
    ]);
  let bool_lit =
    option([
      Node.text("Enter a Boolean Literal"),
      fill_space,
      Node.text("(e.g. "),
      example_lit_node("true"),
      Node.text(")"),
    ]);
  let fun_lit =
    option([
      Node.text("Enter a Function Literal"),
      fill_space,
      AssistantView_common.kc_shortcut_node(HazelKeyCombos.Backslash),
      Node.text(" or "),
      example_lit_node("fun "),
    ]);
  let sum_lit =
    option([
      Node.text("Enter an Injection Literal"),
      fill_space,
      AssistantView_common.kc_shortcut_node(HazelKeyCombos.Alt_L),
      Node.text("or"),
      AssistantView_common.kc_shortcut_node(HazelKeyCombos.Alt_R),
    ]);
  let prod_lit =
    option([
      Node.text("Enter a Tuple Literal"),
      fill_space,
      AssistantView_common.kc_shortcut_node(HazelKeyCombos.LeftParen),
    ]);
  let list_lit =
    option([
      Node.text("Enter an Empty List Literal"),
      fill_space,
      AssistantView_common.kc_shortcut_node(HazelKeyCombos.LeftBracket),
    ]);
  switch (ty) {
  | Hole => [
      int_lit,
      float_lit,
      bool_lit,
      fun_lit,
      sum_lit,
      prod_lit,
      list_lit,
    ]
  | Int => [int_lit]
  | Float => [float_lit]
  | Bool => [bool_lit]
  | Arrow(_, _) => [fun_lit]
  | Sum(_, _) => [sum_lit]
  | Prod(_) => [prod_lit]
  | List(_) => [list_lit]
  };
};

/**
 * Create a list of divs for the var options that will be shown.
 * Return list of Node.t
 */
let list_vars_view = (vars: VarCtx.t) => {
  let b =
    VarMap.map(
      ((var, ty)) => {
        Node.div(
          [Attr.classes(["option"])],
          [code_node(var), Node.text(" : "), HTypCode.view(ty)],
        )
      },
      vars,
    );
  List.map(((_, b)) => {b}, b);
};

/**
 * Create a div containing divs for all operator options that will be shown.
 * Return a Node.t
 */
let operator_options = cursor_info => {
  let int_options = [
    AssistantView_common.kc_shortcut_node(HazelKeyCombos.Plus),
    AssistantView_common.kc_shortcut_node(HazelKeyCombos.Minus),
    AssistantView_common.kc_shortcut_node(HazelKeyCombos.Asterisk),
    AssistantView_common.kc_shortcut_node(HazelKeyCombos.Slash),
  ];
  let int_to_bool_options = [
    AssistantView_common.kc_shortcut_node(HazelKeyCombos.LT),
    AssistantView_common.kc_shortcut_node(HazelKeyCombos.GT),
    AssistantView_common.kc_shortcut_node(HazelKeyCombos.Equals),
  ];
  let float_options = [
    AssistantView_common.text_shortcut_node("+."),
    AssistantView_common.text_shortcut_node("-."),
    AssistantView_common.text_shortcut_node("*."),
    AssistantView_common.text_shortcut_node("/."),
  ];
  let float_to_bool_options = [
    AssistantView_common.text_shortcut_node("<."),
    AssistantView_common.text_shortcut_node(">."),
    AssistantView_common.text_shortcut_node("=."),
  ];

  let int_operators_wrapper = options =>
    mini_option([Node.text("Integer Operation"), fill_space, ...options]);

  let float_operators_wrapper = options =>
    mini_option([
      Node.text("Floating Point Operation"),
      fill_space,
      ...options,
    ]);

  let arithmetic_options_wrapper = options =>
    Node.div(
      [Attr.classes(["option"])],
      [
        Node.div(
          [Attr.classes(["sub-options"])],
          [Node.text("Arithmetic Operation")] @ options,
        ),
      ],
    );

  let boolean_options =
    Node.div(
      [Attr.classes(["option"])],
      [
        Node.text("Boolean Operation"),
        fill_space,
        AssistantView_common.kc_shortcut_node(HazelKeyCombos.Ampersand),
        AssistantView_common.kc_shortcut_node(HazelKeyCombos.VBar),
      ],
    );

  let list_options =
    Node.div(
      [Attr.classes(["option"])],
      [
        Node.text("List Operation"),
        fill_space,
        AssistantView_common.kc_shortcut_node(HazelKeyCombos.Semicolon),
      ],
    );

  switch (Assistant_common.get_type(cursor_info)) {
  | Some(Hole) => [
      arithmetic_options_wrapper([
        int_operators_wrapper(int_options @ int_to_bool_options),
        float_operators_wrapper(float_options @ float_to_bool_options),
      ]),
      boolean_options,
      list_options,
    ]
  | Some(Int) => [
      arithmetic_options_wrapper([int_operators_wrapper(int_options)]),
    ]
  | Some(Float) => [
      arithmetic_options_wrapper([float_operators_wrapper(float_options)]),
    ]
  | Some(Bool) => [
      arithmetic_options_wrapper([
        int_operators_wrapper(int_to_bool_options),
        float_operators_wrapper(float_to_bool_options),
      ]),
      boolean_options,
    ]
  | Some(List(_)) => [list_options]
  | _ => []
  };
};

let add_rule_after_option =
  Node.div(
    [Attr.classes(["option"])],
    [
      Node.text("Add rule after"),
      fill_space,
      AssistantView_common.kc_shortcut_node(HazelKeyCombos.Enter),
    ],
  );

let comment_line_option =
  Node.div(
    [Attr.classes(["option"])],
    [
      Node.text("Create new comment line"),
      fill_space,
      AssistantView_common.kc_shortcut_node(HazelKeyCombos.Pound),
      Node.text(" or "),
      AssistantView_common.kc_shortcut_node(HazelKeyCombos.Shift_Enter),
    ],
  );

let type_driven = body => Node.div([Attr.classes(["type-driven"])], body);

let exp_hole_view =
    (
      ~inject: ModelAction.t => Event.t,
      cursor_inspector: CursorInspectorModel.t,
      cursor_info: CursorInfo.t,
    ) => {
  let lit_open = cursor_inspector.strategy_guide_lit;
  let var_open = cursor_inspector.strategy_guide_var;
  let fun_open = cursor_inspector.strategy_guide_fun;
  let branch_open = cursor_inspector.strategy_guide_branch;
  let new_var_open = cursor_inspector.strategy_guide_new_var;
  let other_open = cursor_inspector.strategy_guide_other;

  let ctx = cursor_info.ctx;

  let typ =
    switch (Assistant_common.get_type(cursor_info)) {
    | Some(ty) => ty
    | None =>
      raise(
        Invalid_argument(
          "Strategy Guide should have type information at the cursor",
        ),
      )
    };

  let subsection_header = (toggle, text, open_section) => {
    let subsection_arrow =
      if (open_section) {
        Icons.down_arrow(["fill-arrow"]);
      } else {
        Icons.left_arrow(["fill-arrow"]);
      };
    Node.div(
      [
        Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
        Attr.on_click(_ => {
          Event.Many([
            Event.Prevent_default,
            Event.Stop_propagation,
            inject(ModelAction.UpdateCursorInspector(toggle)),
          ])
        }),
      ],
      [Node.text(text), subsection_arrow],
    );
  };

  let var_ctx = Assistant_common.extract_vars(ctx, typ);

  let fill_hole_msg =
    Node.div(
      [Attr.classes(["title-bar", "panel-title-bar", "main-fill"])],
      [
        Node.div(
          [Attr.classes(["words"])],
          [Node.text("Here are the options at this position")],
        ),
      ],
    );

  let lit =
    subsection_header(
      Toggle_strategy_guide_lit,
      "Will "
      ++ Assistant_common.type_to_str(typ)
      ++ " literal give what you need?",
      lit_open,
    );
  let lit_body =
    Node.div(
      [Attr.classes(["panel-title-bar", "body-bar"])],
      [Node.div([Attr.classes(["options"])], lit_msg(typ))],
    );

  let vars_view =
    if (VarMap.is_empty(var_ctx)) {
      Node.div(
        [Attr.classes(["option"])],
        [Node.text("No variables of expected type in context")],
      );
    } else {
      Node.div([Attr.classes(["options"])], list_vars_view(var_ctx));
    };
  let var =
    subsection_header(
      Toggle_strategy_guide_var,
      "Is there a variable that represents what you need?",
      var_open,
    );
  let var_body =
    Node.div([Attr.classes(["panel-title-bar", "body-bar"])], [vars_view]);

  let fun_h =
    subsection_header(
      Toggle_strategy_guide_fun,
      "Is there a function that will calculate what you need?",
      fun_open,
    );
  let fun_ctx = Assistant_common.fun_vars(ctx, typ);
  let fun_ap_opt =
    option([
      Node.text("Apply a Function"),
      fill_space,
      AssistantView_common.kc_shortcut_node(HazelKeyCombos.Space),
    ]);
  let fun_view =
    if (VarMap.is_empty(fun_ctx)) {
      [
        Node.div(
          [Attr.classes(["option"])],
          [
            Node.text("No functions with expected resulting type in context"),
          ],
        ),
        fun_ap_opt,
      ];
    } else {
      [fun_ap_opt, ...list_vars_view(Assistant_common.fun_vars(ctx, typ))];
    };
  let fun_body =
    Node.div(
      [Attr.classes(["panel-title-bar", "body-bar"])],
      [
        Node.div(
          [Attr.classes(["options"])],
          fun_view @ operator_options(cursor_info),
        ),
      ],
    );

  let branch =
    subsection_header(
      Toggle_strategy_guide_branch,
      "Are there different cases to consider?",
      branch_open,
    );
  let branch_body =
    Node.div(
      [Attr.classes(["panel-title-bar", "body-bar"])],
      [
        Node.div(
          [Attr.classes(["option"])],
          [
            Node.text("Consider by "),
            keyword_node("case"),
            fill_space,
            example_lit_node("\"case \""),
            Node.text(" or "),
            AssistantView_common.kc_shortcut_node(HazelKeyCombos.Alt_C),
          ],
        ),
      ],
    );

  let new_var =
    subsection_header(
      Toggle_strategy_guide_new_var,
      "Do you want to create a new variable?",
      new_var_open,
    );
  let new_var_body =
    Node.div(
      [Attr.classes(["panel-title-bar", "body-bar"])],
      [
        Node.div(
          [Attr.classes(["option"])],
          [
            Node.text("Create "),
            keyword_node("let"),
            Node.text(" binding"),
            fill_space,
            example_lit_node("\"let \""),
          ],
        ),
      ],
    );

  let other =
    subsection_header(
      Toggle_strategy_guide_other,
      "Other Actions",
      other_open,
    );
  let other_main_options = [
    Node.div(
      [Attr.classes(["option"])],
      [
        Node.text("Parenthesize"),
        fill_space,
        AssistantView_common.kc_shortcut_node(HazelKeyCombos.LeftParen),
      ],
    ),
    Node.div(
      [Attr.classes(["option"])],
      [
        Node.text("Move to next/previous hole"),
        fill_space,
        AssistantView_common.kc_shortcut_node(HazelKeyCombos.Tab),
        AssistantView_common.kc_shortcut_node(HazelKeyCombos.ShiftTab),
      ],
    ),
    Node.div(
      [Attr.classes(["option"])],
      [
        Node.text("Swap line up/down"),
        fill_space,
        AssistantView_common.kc_shortcut_node(HazelKeyCombos.Alt_Up),
        AssistantView_common.kc_shortcut_node(HazelKeyCombos.Alt_Down),
      ],
    ),
    Node.div(
      [Attr.classes(["option"])],
      [
        Node.text("Swap operand left/right"),
        fill_space,
        AssistantView_common.kc_shortcut_node(HazelKeyCombos.Alt_Left),
        AssistantView_common.kc_shortcut_node(HazelKeyCombos.Alt_Right),
      ],
    ),
  ];
  let other_options =
    switch (cursor_info.parent_info) {
    | AfterBranchClause =>
      List.append(other_main_options, [add_rule_after_option])
    | BeforeEmptyHoleLine =>
      List.append(other_main_options, [comment_line_option])
    | NoParentInfo => other_main_options
    };
  let other_body =
    Node.div(
      [Attr.classes(["panel-title-bar", "body-bar"])],
      other_options,
    );
  let body =
    if (lit_open) {
      [fill_hole_msg, lit, lit_body];
    } else {
      [fill_hole_msg, lit];
    };
  let body =
    if (var_open) {
      body @ [var, var_body];
    } else {
      body @ [var];
    };
  let body =
    if (fun_open) {
      body @ [fun_h, fun_body];
    } else {
      body @ [fun_h];
    };
  let body =
    if (branch_open) {
      body @ [branch, branch_body];
    } else {
      body @ [branch];
    };
  let body =
    if (new_var_open) {
      body @ [new_var, new_var_body];
    } else {
      body @ [new_var];
    };
  let body =
    if (other_open) {
      body @ [other, other_body];
    } else {
      body @ [other];
    };
  type_driven(body);
};

let rules_view = (cursor_info: CursorInfo.t) => {
  switch (cursor_info.cursor_term, cursor_info.parent_info) {
  | (Rule(OnDelim(0, After), _), _)
  | (ExpOperand(OnDelim(1, Before), Case(_)), _) =>
    Some(
      type_driven([
        Node.div(
          [Attr.classes(["panel-title-bar", "body-bar"])],
          [
            Node.div(
              [Attr.classes(["option"])],
              [
                Node.text("Add rule before"),
                fill_space,
                AssistantView_common.kc_shortcut_node(HazelKeyCombos.Enter),
              ],
            ),
          ],
        ),
      ]),
    )
  | (Rule(OnDelim(1, Before), _), _)
  | (_, AfterBranchClause) =>
    Some(
      type_driven([
        Node.div(
          [Attr.classes(["panel-title-bar", "body-bar"])],
          [add_rule_after_option],
        ),
      ]),
    )
  | _ => None
  };
};

let lines_view = (suggest_comment: bool) => {
  let new_line =
    Node.div(
      [Attr.classes(["option"])],
      [
        Node.text("Create new line"),
        fill_space,
        AssistantView_common.kc_shortcut_node(HazelKeyCombos.Enter),
      ],
    );
  let body = suggest_comment ? [new_line, comment_line_option] : [new_line];
  type_driven([
    Node.div([Attr.classes(["panel-title-bar", "body-bar"])], body),
  ]);
};
