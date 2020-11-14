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
    | Int
    | Float
    | Bool
    | Sum(_, _)
    | Prod(_)
    | List(_) => true
    | _ => false
    };
  };
  let branchable_vars = vars |> VarMap.filter(can_branch_on);
  branchable_vars;
};

/**
 * Gets the type of the expression at the cursor.
 * Return HTyp.t
 */
let get_type = (cursor_info: CursorInfo_common.t) => {
  let rec my_type = (typed: CursorInfo_common.typed) =>
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
  | Some(Hole) => "A"
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
        Vdom.Node.text("Enter a function (type "),
        shortcut_node("\\"),
        Node.text("): "),
      ]
    | Sum(_, _) => [
        Vdom.Node.text("Enter a Sum (type "),
        shortcut_node("Alt + l"),
        Node.text("or"),
        shortcut_node("Alt + r"),
        Node.text("): "),
      ]
    | Prod(_) => [Vdom.Node.text("Enter a Product (type `,`): ")]
    | List(_) => [
        Vdom.Node.text("Enter a List (type "),
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
        [
          Node.div(
            [Attr.classes([])],
            [code_node("case ...")] @ mini_options,
          ),
        ],
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

  let ty: option(HTyp.t) = get_type(cursor_info);
  let options =
    switch (ty) {
    | Some(Hole) => int_options @ float_options
    | Some(Int) => int_options
    | Some(Float) => float_options
    | _ => []
    };

  [
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
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      cursor_inspector: Model.cursor_inspector,
      cursor_info: CursorInfo_common.t,
    ) => {
  let lit_t = cursor_inspector.type_assist_lit;
  let var_t = cursor_inspector.type_assist_var;
  let func_t = cursor_inspector.type_assist_fun;
  let branch_t = cursor_inspector.type_assist_branch;
  let other_t = cursor_inspector.type_assist_other;

  let ty = get_type(cursor_info);
  let ctx = cursor_info.ctx;

  let _ = ty;
  let _ = ctx;

  let typ =
    switch (ty) {
    | Some(my_ty) => my_ty
    | None => raise(Invalid_argument("Should have a type..."))
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
  let lit_arrow =
    if (lit_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
    };
  let lit =
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(ModelAction.ToggleTypeAssistLit),
            ])
          ),
        ],
        [
          Node.text("Fill with " ++ type_to_str(ty) ++ " literal"),
          lit_arrow,
        ],
      )
    );
  let lit_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [Node.div([Attr.classes(["option"])], lit_msg(typ))],
      )
    );

  let var_arrow =
    if (var_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
    };
  let vars_view =
    if (VarMap.is_empty(var_ctx)) {
      [
        Vdom.(
          Node.div(
            [Attr.classes(["option", "empty-vars"])],
            [Node.text("No variables of expected type in context")],
          )
        ),
      ];
    } else {
      list_vars_view(var_ctx);
    };
  let var =
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(ModelAction.ToggleTypeAssistVar),
            ])
          ),
        ],
        [Node.text("Fill with a Variable"), var_arrow],
      )
    );
  let var_body =
    Vdom.(
      Node.div([Attr.classes(["panel-title-bar", "body-bar"])], vars_view)
    );
  let _ = var_body;
  let arrow_func =
    if (func_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
    };
  let func =
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(ModelAction.ToggleTypeAssistFun),
            ])
          ),
        ],
        [Node.text("Apply a Function"), arrow_func],
      )
    );
  let func_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
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
      )
    );
  let arrow_branch =
    if (branch_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
    };
  let branch =
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ => {
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(ModelAction.ToggleTypeAssistBranch),
            ])
          }),
        ],
        [Node.text("Consider by cases"), arrow_branch],
      )
    );
  let branch_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        branch_vars_view(cursor_info.ctx),
      )
    );
  let arrow_other =
    if (other_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
    };
  let other =
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ => {
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(ModelAction.ToggleTypeAssistOther),
            ])
          }),
        ],
        [Node.text("Other"), arrow_other],
      )
    );
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
    if (lit_t) {
      List.append([fill_hole_msg], [lit, lit_body]);
    } else {
      List.append([fill_hole_msg], [lit]);
    };
  let body =
    if (var_t) {
      List.append(body, [var, var_body]);
    } else {
      List.append(body, [var]);
    };
  let body =
    if (func_t) {
      List.append(body, [func, func_body]);
    } else {
      List.append(body, [func]);
    };
  let body =
    if (branch_t) {
      List.append(body, [branch, branch_body]);
    } else {
      List.append(body, [branch]);
    };
  let body =
    if (other_t) {
      List.append(body, [other, other_body]);
    } else {
      List.append(body, [other]);
    };

  Vdom.(Node.div([Attr.classes(["type-driven"])], body));
};
