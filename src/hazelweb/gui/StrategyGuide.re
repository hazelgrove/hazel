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
 * Extract from the context the variables we offer to branch on.
 * Return a VarCtx.t
 */
let branch_vars = (ctx: Contexts.t) => {
  let (vars, _) = ctx;
  let can_branch_on = ((_, ty: HTyp.t)) => {
    switch (ty) {
    | Hole
    | Int
    | Float
    | Bool
    | Sum(_, _)
    | Prod(_)
    | List(_) => true
    | Arrow(_, _) => false
    };
  };
  let branchable_vars = vars |> VarMap.filter(can_branch_on);
  branchable_vars;
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
let type_to_str = (ty: option(HTyp.t)) => {
  switch (ty) {
  | Some(Hole) => "a"
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

let lit_msg = (ty: HTyp.t) => {
  open Vdom;

  let msg =
    switch (ty) {
    | Hole => [Vdom.Node.text("Enter a literal: ")]
    | Int => [
        Vdom.Node.text("Enter an Integer (e.g. "),
        code_node("1"),
        Node.text("): "),
      ]
    | Float => [
        Vdom.Node.text("Enter a Float (e.g. "),
        code_node("1.0"),
        Node.text("): "),
      ]
    | Bool => [
        Vdom.Node.text("Enter a Boolean (e.g. "),
        code_node("true"),
        Node.text("): "),
      ]
    | Arrow(_, _) => [
        Vdom.Node.text("Enter a function (enter "),
        shortcut_node("\\"),
        Node.text("): "),
      ]
    | Sum(_, _) => [
        Vdom.Node.text("Enter a Sum (enter "),
        shortcut_node("Alt + l"),
        Node.text("or"),
        shortcut_node("Alt + r"),
        Node.text("): "),
      ]
    | Prod(_) => [Vdom.Node.text("Enter a Product (enter `,`): ")]
    | List(_) => [
        Vdom.Node.text("Enter a List (enter "),
        shortcut_node("["),
        Node.text("): "),
      ]
    };
  msg @ [HTypCode.view(ty)];
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
 * Create a list of divs for the branch var options that will be shown.
 * Return list of Node.t
 */
let branch_vars_view = (ctx: Contexts.t) => {
  let vars = branch_vars(ctx);
  let mini_options =
    List.map(
      ((var, ty)) => {
        Vdom.(
          Node.div(
            [Attr.classes(["mini-option"])],
            [code_node(var), Node.text(":"), HTypCode.view(ty)],
          )
        )
      },
      vars,
    )
    |> List.append([
         Vdom.(
           Node.div(
             [Attr.classes(["mini-option"])],
             [Node.text("Empty hole" ++ ":"), HTypCode.view(HTyp.Hole)],
           )
         ),
       ]);
  [
    Vdom.(
      Node.div(
        [Attr.classes(["option"])],
        [Node.div([], [code_node("case ...")] @ mini_options)],
      )
    ),
  ];
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

  let ty: option(HTyp.t) = get_type(cursor_info);
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
            [Node.text("Which strategy do you want to try?")],
          ),
        ],
      )
    );

  let lit =
    subsection_header(
      Toggle_type_assist_lit,
      "Fill with " ++ type_to_str(ty) ++ " literal",
      lit_open,
    );
  let lit_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [Node.div([Attr.classes(["option"])], lit_msg(typ))],
      )
    );

  let vars_view =
    if (VarMap.is_empty(var_ctx)) {
      Vdom.(
        Node.div(
          [Attr.classes(["option", "empty-vars"])],
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
  let fun_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Node.div(
            [Attr.classes(["options"])],
            list_vars_view(fun_vars(ctx, typ))
            @ [
              Node.div(
                [Attr.classes(["option"])],
                [
                  Node.text("Create and apply new function: "),
                  HTypCode.view(Arrow(Hole, typ)),
                ],
              ),
            ]
            @ other_arithmetic_options(cursor_info),
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
            [Attr.classes(["options"])],
            branch_vars_view(cursor_info.ctx),
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
            [Node.text("New let binding")],
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
