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
  let my_type =
    switch (cursor_info.typed) {
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
      | _ => None
      }
    | _ => None
    };
  my_type;
};

/**
 * Gets the type in string format.
 * Return string
 */
let type_to_str = (ty: option(HTyp.t)) => {
  switch (ty) {
  | Some(Hole) => "Hole"
  | Some(Int) => "Integer"
  | Some(Float) => "Boolean"
  | Some(Arrow(_, _)) => "Function"
  | Some(Sum(_, _)) => "Sum"
  | Some(Prod(_)) => "Product"
  | Some(List(_)) => "List"
  | _ => raise(Invalid_argument("No Literal"))
  };
};

/**
 * Gets the type from the option type argument.
 * Return HTyp.t
 */
let extract_literal = (ty: option(HTyp.t)) => {
  switch (ty) {
  | Some(ty_) => ty_
  | _ => raise(Invalid_argument("No Literal"))
  };
};

/**
 * Create a list of divs for the var options that will be shown.
 * Return list of Node.t
 */
let list_vars_view = (vars: VarCtx.t) => {
  VarMap.map(
    ((var, ty)) => {
      Vdom.(
        Node.div([Attr.classes(["option"])], [Node.text(var ++ " : ")]),
        HTypCode.view(ty),
      )
    },
    vars,
  );
};

/**
 * Create a list of divs for the branch var options that will be shown.
 * Return list of Node.t
 */
let branch_vars_view = (ctx: Contexts.t) => {
  let (vars, _) = ctx;
  List.map(
    ((var, ty)) => {
      Vdom.(
        Node.div(
          [Attr.classes(["mini-option"])],
          [Node.text(var ++ " : "), HTypCode.view(ty)],
        )
      )
    },
    vars,
  )
  |> List.append([
       Vdom.(
         Node.div(
           [Attr.classes(["mini-option"])],
           [Node.text("Empty hole" ++ " : "), HTypCode.view(HTyp.Hole)],
         )
       ),
     ]);
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
           [Node.text(s ++ " : "), HTypCode.view(HTyp.Int)],
         )
       });
  let float_options =
    ["+.", "-.", "*.", "/."]
    |> List.map(s => {
         Node.div(
           [Attr.classes(["mini-option"])],
           [Node.text(s ++ " : "), HTypCode.view(HTyp.Float)],
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
  let other_t = cursor_inspector.type_assist_other;

  let ty = get_type(cursor_info);
  let ctx = cursor_info.ctx;

  let _ = ty;
  let _ = ctx;

  let typ =
    switch (ty) {
    | Some(my_ty) => my_ty
    | None => print_endline("None")
    };

  let var_ctx = extract_vars(ctx, typ);
  let list_vars = list_vars_view(var_ctx);

  let fill_hole_msg =
    Vdom.(
      Node.div(
        [Attr.classes(["title-bar", "panel-title-bar", "main-fill"])],
        [
          Node.div(
            [Attr.classes(["words"])],
            [
              Node.text(
                "Which of the following will have the intended behavior?",
              ),
            ],
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
        [Node.text(type_to_str(ty) ++ " literal"), lit_arrow],
      )
    );
  let lit_body_1 =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Node.div(
            [Attr.classes(["option"])],
            [
              Node.text("Create new " ++ type_to_str(ty) ++ " : "),
              HTypCode.view(extract_literal(ty)),
            ],
          ),
        ],
      )
    );
  let lit_body_2 =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Node.div(
            [Attr.classes(["option"])],
            [
              Node.text("Create new function : "),
              HTypCode.view(Arrow(Prod([Float, Bool]), Float)),
            ],
          ),
        ],
      )
    );
  let _ = if (true) {lit_body_1} else {lit_body_2};

  let var_arrow =
    if (var_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
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
        [Node.text("Variable"), var_arrow],
      )
    );
  let var_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [list_vars],
      )
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
        [Node.text("Function Application"), arrow_func],
      )
    );
  let func_body_1 =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Node.div(
            [Attr.classes(["option"])],
            [
              Node.text("Apply "),
              Node.div(
                [Attr.classes(["code-font"])],
                [Node.text("map : ")],
              ),
              HTypCode.view(
                Arrow(
                  Arrow(
                    Arrow(Prod([Float, Bool]), Float),
                    List(Prod([Float, Bool])),
                  ),
                  List(Float),
                ),
              ),
            ],
          ),
          Node.div(
            [Attr.classes(["option"])],
            [
              Node.text("Create and apply new function : "),
              HTypCode.view(Arrow(Hole, Hole)),
            ],
          ),
        ],
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
            print_endline("Clicked");
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(ModelAction.ToggleTypeAssistOther),
            ]);
          }),
        ],
        [Node.text("Other"), arrow_other],
      )
    );
  let other_body_1 =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Node.div(
            [Attr.classes(["option"])],
            [
              Node.div(
                [Attr.classes([])],
                [Node.text("Branch on...")]
                @ branch_vars_view(cursor_info.ctx),
              ),
            ],
          ),
        ]
        @ other_arithmetic_options(cursor_info)
        @ [
          Node.div(
            [Attr.classes(["option"])],
            [Node.text("New let binding")],
          ),
        ],
      )
    );
  let body =
    if (lit_t) {
      List.append([fill_hole_msg], [lit, lit_body_1]);
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
      List.append(body, [func, func_body_1]);
    } else {
      List.append(body, [func]);
    };
  let body =
    if (other_t) {
      List.append(body, [other, other_body_1]);
    } else {
      List.append(body, [other]);
    };

  let _ = extract_vars;
  let _ = branch_vars;
  let _ = get_type;
  let _ = list_vars_view;

  Vdom.(
    Node.div(
      [
        Attr.classes(["type-driven"]),
        Attr.on_click(_ => {
          print_endline("Clicked");
          Event.Ignore;
        }),
      ],
      body,
    )
  );
};
