module Vdom = Virtual_dom.Vdom;

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

let keyword_node = text =>
  Vdom.Node.div(
    [Vdom.Attr.classes(["code-font", "keyword"])],
    [Vdom.Node.text(text)],
  );

let option = nodes => Vdom.(Node.div([Attr.classes(["option"])], nodes));

let lit_msg = (ty: HTyp.t) => {
  let int_lit =
    option([
      Vdom.Node.text("Enter an Integer (e.g. "),
      code_node("1"),
      Vdom.Node.text(")"),
    ]);
  let float_lit =
    option([
      Vdom.Node.text("Enter a Float (e.g. "),
      code_node("1.0"),
      Vdom.Node.text(")"),
    ]);
  let bool_lit =
    option([
      Vdom.Node.text("Enter a Boolean (e.g. "),
      code_node("true"),
      Vdom.Node.text(")"),
    ]);
  let fun_lit =
    option([
      Vdom.Node.text("Enter a Function (enter "),
      shortcut_node("\\"),
      Vdom.Node.text(")"),
    ]);
  let sum_lit =
    option([
      Vdom.Node.text("Enter an Injection (enter "),
      shortcut_node("Alt + l"),
      Vdom.Node.text("or"),
      shortcut_node("Alt + r"),
      Vdom.Node.text(")"),
    ]);
  let prod_lit =
    option([
      Vdom.Node.text("Enter a Tuple (enter "),
      shortcut_node(")"),
      Vdom.Node.text(")"),
    ]);
  let list_lit =
    option([
      Vdom.Node.text("Enter a List (enter "),
      shortcut_node("["),
      Vdom.Node.text(")"),
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
 * Create a div containing divs for all arithmetic operation options that will be shown.
 * Return a Node.t
 */
let other_arithmetic_options = cursor_info => {
  open Vdom;
  let int_options =
    ["+", "-", "*", "/"]
    |> List.map(s => {
         Node.div(
           [Attr.classes(["mini-option"])],
           [
             code_node(s),
             Node.text(":"),
             HTypCode.view(HTyp.Arrow(HTyp.Int, HTyp.Arrow(Int, Int))),
           ],
         )
       });
  let float_options =
    ["+.", "-.", "*.", "/."]
    |> List.map(s => {
         Node.div(
           [Attr.classes(["mini-option"])],
           [
             code_node(s),
             Node.text(":"),
             HTypCode.view(HTyp.Arrow(Float, Arrow(Float, Float))),
           ],
         )
       });
  let int_options_2 =
    ["<", ">", "=="]
    |> List.map(s => {
         Node.div(
           [Attr.classes(["mini-option"])],
           [
             code_node(s),
             Node.text(":"),
             HTypCode.view(HTyp.Arrow(HTyp.Int, HTyp.Arrow(Int, Bool))),
           ],
         )
       });
  let float_options_2 =
    ["<.", ">.", "==."]
    |> List.map(s => {
         Node.div(
           [Attr.classes(["mini-option"])],
           [
             code_node(s),
             Node.text(":"),
             HTypCode.view(HTyp.Arrow(Float, Arrow(Float, Bool))),
           ],
         )
       });
  let boolean_options =
    ["&&", "||"]
    |> List.map(s => {
         Node.div(
           [Attr.classes(["mini-option"])],
           [
             code_node(s),
             Node.text(":"),
             HTypCode.view(HTyp.Arrow(Bool, Arrow(Bool, Bool))),
           ],
         )
       });

  let list_options = t2 =>
    ["::"]
    |> List.map(s => {
         Node.div(
           [Attr.classes(["mini-option"])],
           [
             code_node(s),
             Node.text(":"),
             HTypCode.view(HTyp.Arrow(t2, Arrow(List(t2), List(t2)))),
           ],
         )
       });

  let arithmetic_options_wrapper = options => [
    Node.div(
      [Attr.classes(["option"])],
      [
        Node.div(
          [Attr.classes([])],
          [Node.text("Arithmetic operation")] @ options,
        ),
      ],
    ),
  ];

  let boolean_options_wrapper = options => [
    Node.div(
      [Attr.classes(["option"])],
      [
        Node.div(
          [Attr.classes([])],
          [Node.text("Boolean operation")] @ options,
        ),
      ],
    ),
  ];

  let list_options_wrapper = options => [
    Node.div(
      [Attr.classes(["option"])],
      [
        Node.div(
          [Attr.classes([])],
          [Node.text("List operation")] @ options,
        ),
      ],
    ),
  ];

  let ty: option(HTyp.t) = AssistantCommon.get_type(cursor_info);
  switch (ty) {
  | Some(Hole) => arithmetic_options_wrapper(int_options @ float_options)
  | Some(Int) => arithmetic_options_wrapper(int_options)
  | Some(Float) => arithmetic_options_wrapper(float_options)
  | Some(Bool) =>
    boolean_options_wrapper(int_options_2 @ float_options_2 @ boolean_options)
  | Some(List(t2)) => list_options_wrapper(list_options(t2))
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

  let ty = AssistantCommon.get_type(cursor_info);
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

  let var_ctx = AssistantCommon.extract_vars(ctx, typ);

  let fill_hole_msg =
    Vdom.(
      Node.div(
        [Attr.classes(["title-bar", "panel-title-bar", "main-fill"])],
        [
          Node.div(
            [Attr.classes(["words"])],
            [Node.text("Which strategy do you want to try?")],
          ),
        ],
      )
    );

  let lit =
    subsection_header(
      Toggle_type_assist_lit,
      "Fill with " ++ AssistantCommon.type_to_str(ty) ++ " literal",
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
      "Fill with a Variable",
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
    subsection_header(Toggle_type_assist_fun, "Apply a Function", fun_open);
  let fun_ctx = AssistantCommon.fun_vars(ctx, typ);
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
      ];
    } else {
      list_vars_view(AssistantCommon.fun_vars(ctx, typ));
    };
  let fun_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Node.div(
            [Attr.classes(["options"])],
            fun_view @ other_arithmetic_options(cursor_info),
          ),
        ],
      )
    );

  let branch =
    subsection_header(
      Toggle_type_assist_branch,
      "Consider by cases",
      branch_open,
    );
  let branch_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Node.div(
            [Attr.classes(["option"])],
            [Node.text("Create "), keyword_node("case")],
          ),
        ],
      )
    );

  let other =
    subsection_header(Toggle_type_assist_other, "Other", other_open);
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
