module Vdom = Virtual_dom.Vdom;

/**
 * Extract from the context the variables that are consistent with the type that
 * we are looking for.
 * Return a VarCtx.t
 */
let extract_vars = (ctx: Contexts.t, typ: HTyp.t) => {
  let (vars, _) = ctx;
  let can_extract = ((_, ty: HTyp.t)) => {
    HTyp.consistent(ty, typ);
  };
  vars |> VarMap.filter(can_extract);
};

/**
 * Filter the variables that are functions that have the correct resulting type
 */
let fun_vars = (ctx: Contexts.t, typ: HTyp.t) => {
  let (vars, _) = ctx;
  let rec compatible_funs = right_ty =>
    if (HTyp.consistent(right_ty, typ)) {
      true;
    } else {
      switch (right_ty) {
      | Arrow(_, right_ty) => compatible_funs(right_ty)
      | _ => false
      };
    };
  let can_extract = ((_, ty: HTyp.t)) => {
    switch (ty) {
    | Arrow(_, t2) => compatible_funs(t2)
    | _ => false
    };
  };
  vars |> VarMap.filter(can_extract);
};

/**
 * Gets the type of the expression at the cursor.
 * Return HTyp.t
 */
let get_type = (cursor_info: CursorInfo.t) => {
  let rec my_type = (typed: CursorInfo.typed) =>
    switch (typed) {
    | Analyzed(ty) => Some(ty)
    | AnaAnnotatedLambda(expected_ty, _) => Some(expected_ty)
    | AnaSubsumed(expected_ty, _) => Some(expected_ty)
    | Synthesized(ty) => Some(ty)
    | SynMatchingArrow(syn_ty, _) => Some(syn_ty)
    | SynBranchClause(join, typed, _) =>
      switch (join, typed) {
      | (JoinTy(ty), Synthesized(got_ty)) =>
        switch (HTyp.consistent(ty, got_ty), HTyp.eq(ty, got_ty)) {
        | (true, true) => Some(ty)
        | (true, false) => Some(ty)
        | _ => None
        }
      | (NoBranches, _) => my_type(typed)
      | _ => None
      }
    | _ => None
    };
  my_type(cursor_info.typed);
};

/**
 * Gets the type in string format.
 * Return string
 */
let type_to_str = (~empty_hole=false, ty: option(HTyp.t)) => {
  switch (ty) {
  | Some(Hole) => empty_hole ? "" : "a"
  | Some(Int) => "Integer"
  | Some(Float) => "Float"
  | Some(Bool) => "Boolean"
  | Some(Arrow(_, _)) => "Function"
  | Some(Sum(_, _)) => "Sum"
  | Some(Prod(_)) => "Product"
  | Some(List(_)) => "List"
  | _ => raise(Invalid_argument("No Literal"))
  };
};

let code_node = text =>
  Vdom.Node.div(
    [Vdom.Attr.classes(["code-font"])],
    [Vdom.Node.text(text)],
  );

let shortcut_node = text =>
  Vdom.Node.div(
    [Vdom.Attr.classes(["code-font", "shortcut"])],
    [Vdom.Node.text(text)],
  );

let example_lit_node = text =>
  Vdom.Node.div(
    [Vdom.Attr.classes(["code-font", "example"])],
    [Vdom.Node.text(text)],
  );

let keyword_node = text =>
  Vdom.Node.div(
    [Vdom.Attr.classes(["code-font", "keyword"])],
    [Vdom.Node.text(text)],
  );

let option = nodes => Vdom.(Node.div([Attr.classes(["option"])], nodes));
let mini_option = nodes =>
  Vdom.(Node.div([Attr.classes(["mini-option"])], nodes));
let fill_space = Vdom.(Node.span([Attr.classes(["filler"])], []));
let lit_msg = (ty: HTyp.t) => {
  let int_lit =
    option([
      Vdom.Node.text("Enter an Integer Literal"),
      fill_space,
      Vdom.Node.text("(e.g. "),
      example_lit_node("1"),
      Vdom.Node.text(")"),
    ]);
  let float_lit =
    option([
      Vdom.Node.text("Enter a Floating Point Literal"),
      fill_space,
      Vdom.Node.text("(e.g. "),
      example_lit_node("1.0"),
      Vdom.Node.text(")"),
    ]);
  let bool_lit =
    option([
      Vdom.Node.text("Enter a Boolean Literal"),
      fill_space,
      Vdom.Node.text("(e.g. "),
      example_lit_node("true"),
      Vdom.Node.text(")"),
    ]);
  let fun_lit =
    option([
      Vdom.Node.text("Enter a Function Literal"),
      fill_space,
      shortcut_node("\\"),
    ]);
  let sum_lit =
    option([
      Vdom.Node.text("Enter an Injection Literal"),
      fill_space,
      shortcut_node("Alt+l"),
      Vdom.Node.text("or"),
      shortcut_node("Alt+r"),
    ]);
  let prod_lit =
    option([
      Vdom.Node.text("Enter a Tuple Literal"),
      fill_space,
      shortcut_node("("),
    ]);
  let list_lit =
    option([
      Vdom.Node.text("Enter an Empty List Literal"),
      fill_space,
      shortcut_node("["),
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
        Vdom.(
          Node.div(
            [Attr.classes(["option"])],
            [code_node(var), Node.text(" : "), HTypCode.view(ty)],
          )
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
  open Vdom;
  let int_options = [
    shortcut_node("+"),
    shortcut_node("-"),
    shortcut_node("*"),
    shortcut_node("/"),
  ];
  let int_to_bool_options = [
    shortcut_node("<"),
    shortcut_node(">"),
    shortcut_node("="),
  ];
  let float_options = [
    shortcut_node("+."),
    shortcut_node("-."),
    shortcut_node("*."),
    shortcut_node("/."),
  ];
  let float_to_bool_options = [
    shortcut_node("<."),
    shortcut_node(">."),
    shortcut_node("=."),
  ];

  let int_operators_wrapper = options =>
    mini_option([
      Vdom.Node.text("Integer Operation"),
      fill_space,
      ...options,
    ]);

  let float_operators_wrapper = options =>
    mini_option([
      Vdom.Node.text("Floating Point Operation"),
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
        shortcut_node("&&"),
        shortcut_node("||"),
      ],
    );

  let list_options =
    Node.div(
      [Attr.classes(["option"])],
      [Node.text("List Operation"), fill_space, shortcut_node(";")],
    );

  switch (get_type(cursor_info)) {
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

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
    ) => {
  let lit_open = cursor_inspector.type_assist_lit;
  let var_open = cursor_inspector.type_assist_var;
  let fun_open = cursor_inspector.type_assist_fun;
  let branch_open = cursor_inspector.type_assist_branch;
  let other_open = cursor_inspector.type_assist_other;

  let ty = get_type(cursor_info);
  let ctx = cursor_info.ctx;

  let typ =
    switch (ty) {
    | Some(my_ty) => my_ty
    | None => raise(Invalid_argument("Should have a type..."))
    };

  let subsection_header = (setting, text, open_section) => {
    let subsection_arrow =
      if (open_section) {
        Icons.down_arrow(["fill-arrow"]);
      } else {
        Icons.left_arrow(["fill-arrow"]);
      };
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ => {
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(ModelAction.UpdateSettings(CursorInspector(setting))),
            ])
          }),
        ],
        [Node.text(text), subsection_arrow],
      )
    );
  };

  let var_ctx = extract_vars(ctx, typ);

  let fill_hole_msg =
    Vdom.(
      Node.div(
        [Attr.classes(["title-bar", "panel-title-bar", "main-fill"])],
        [
          Node.div(
            [Attr.classes(["words"])],
            [Node.text("Here are the options at this position")],
          ),
        ],
      )
    );

  let lit =
    subsection_header(
      Toggle_type_assist_lit,
      /* TODO: a vs an*/
      "Will a "
      ++ type_to_str(~empty_hole=true, ty)
      ++ " literal give what you need?",
      lit_open,
    );
  let lit_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [Node.div([Attr.classes(["options"])], lit_msg(typ))],
      )
    );

  let vars_view =
    if (VarMap.is_empty(var_ctx)) {
      Vdom.(
        Node.div(
          [Attr.classes(["option"])],
          [Node.text("No variables of expected type in context")],
        )
      );
    } else {
      Vdom.(
        Node.div([Attr.classes(["options"])], list_vars_view(var_ctx))
      );
    };
  let var =
    subsection_header(
      Toggle_type_assist_var,
      "Is there a variable that represents what you need?",
      var_open,
    );
  let var_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [vars_view],
      )
    );

  let fun_h =
    subsection_header(
      Toggle_type_assist_fun,
      "Is there a function that will calculate what you need?",
      fun_open,
    );
  let fun_ctx = fun_vars(ctx, typ);
  let fun_ap_opt =
    option([
      Vdom.Node.text("Apply a Function"),
      fill_space,
      shortcut_node("Space"),
    ]);
  let fun_view =
    if (VarMap.is_empty(fun_ctx)) {
      [
        Vdom.(
          Node.div(
            [Attr.classes(["option"])],
            [
              Node.text(
                "No functions with expected resulting type in context",
              ),
            ],
          )
        ),
        fun_ap_opt,
      ];
    } else {
      [fun_ap_opt, ...list_vars_view(fun_vars(ctx, typ))];
    };
  let fun_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Node.div(
            [Attr.classes(["options"])],
            fun_view @ operator_options(cursor_info),
          ),
        ],
      )
    );

  let branch =
    subsection_header(
      Toggle_type_assist_branch,
      "Are there different cases to consider?",
      branch_open,
    );
  let branch_body =
    Vdom.(
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
            ],
          ),
        ],
      )
    );

  let other =
    subsection_header(
      Toggle_type_assist_other,
      "Do you want to create a new variable first?",
      other_open,
    );
  let other_body =
    Vdom.(
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
      )
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
    if (other_open) {
      body @ [other, other_body];
    } else {
      body @ [other];
    };

  Vdom.(Node.div([Attr.classes(["type-driven"])], body));
};
