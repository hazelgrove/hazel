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
    | PatAnalyzed(ty) => Some(ty)
    | PatAnaSubsumed(expected_ty, _) => Some(expected_ty)
    | PatSynthesized(ty) => Some(ty)
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

let keyword_node = text =>
  Vdom.Node.div(
    [Vdom.Attr.classes(["code-font", "keyword"])],
    [Vdom.Node.text(text)],
  );

let option = nodes => Vdom.(Node.div([Attr.classes(["option"])], nodes));

let lit_msg_exp = (ty: HTyp.t) => {
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
      shortcut_node("("),
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

let lit_msg_pat = (ty: HTyp.t) => {
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
  switch (ty) {
  | Hole => [int_lit, float_lit, bool_lit, fun_lit, sum_lit]
  | Int => [int_lit]
  | Float => [float_lit]
  | Bool => [bool_lit]
  | Arrow(_, _) => [fun_lit]
  | Sum(_, _) => [sum_lit]
  | Prod(_)
  | List(_) => [option([Vdom.Node.text("No suggestions.")])]
  };
};

let pat_msg = (ty: HTyp.t) => {
  let prod_pat =
    option([
      Vdom.Node.text("Enter a Tuple pattern (enter "),
      shortcut_node("("),
      Vdom.Node.text(")"),
    ]);
  let sum_pat =
    option([
      Vdom.Node.text("Enter an Injection (enter "),
      shortcut_node("Alt + l"),
      Vdom.Node.text("or"),
      shortcut_node("Alt + r"),
      Vdom.Node.text(")"),
    ]);
  let list_pat =
    option([
      Vdom.Node.text("Enter a nonempty List pattern (enter "),
      shortcut_node(";"),
      Vdom.Node.text(")"),
    ]);
  let list_lit_pat =
    option([
      Vdom.Node.text("Enter a List (enter "),
      shortcut_node("["),
      Vdom.Node.text(")"),
    ]);
  switch (ty) {
  | Hole => [prod_pat, list_pat, list_lit_pat]
  | Prod(_) => [prod_pat]
  | List(_) => [list_pat, list_lit_pat]
  | Sum(_) => [sum_pat]
  | _ => []
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
 * Create a list of divs for the var options that will be shown.
 * Return list of Node.t
 */
let list_bindings_view = (ty: HTyp.t) => {
  let suggestions =
    switch (ty) {
    | Int => ["n"]
    | Float => ["f"]
    | Bool => ["b"]
    | List(_) => ["xs"]
    | _ => ["x", "y", "z"]
    };
  List.map(
    binding => {
      Vdom.(Node.div([Attr.classes(["option"])], [code_node(binding)]))
    },
    suggestions,
  );
};

let list_wild_view = () => {
  let suggestions = ["_"];
  List.map(
    binding => {
      Vdom.(Node.div([Attr.classes(["option"])], [code_node(binding)]))
    },
    suggestions,
  );
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

let view_exp =
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
        [Node.div([Attr.classes(["options"])], lit_msg_exp(typ))],
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
  let fun_ctx = fun_vars(ctx, typ);
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
      list_vars_view(fun_vars(ctx, typ));
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

let view_pat =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
    ) => {
  let lit_open = cursor_inspector.type_assist_lit;
  let binding_open = cursor_inspector.type_assist_binding;
  let wild_open = cursor_inspector.type_assist_wild;
  let pat_open = cursor_inspector.type_assist_pat;

  let ty = get_type(cursor_info);

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
        [Node.div([Attr.classes(["options"])], lit_msg_pat(typ))],
      )
    );

  let pat =
    subsection_header(
      Toggle_type_assist_pat,
      "Fill with " ++ type_to_str(ty) ++ " pattern",
      pat_open,
    );
  let pat_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [Node.div([Attr.classes(["options"])], pat_msg(typ))],
      )
    );

  let binding_view =
    Vdom.(Node.div([Attr.classes(["options"])], list_bindings_view(typ)));
  let binding =
    subsection_header(
      Toggle_type_assist_binding,
      "Fill with a binding",
      binding_open,
    );
  let binding_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [binding_view],
      )
    );

  let wild_view =
    Vdom.(Node.div([Attr.classes(["options"])], list_wild_view()));
  let wild =
    subsection_header(
      Toggle_type_assist_wild,
      "Fill with a wildcard",
      wild_open,
    );
  let wild_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [wild_view],
      )
    );

  let body = [];
  let body =
    if (lit_open) {
      body @ [fill_hole_msg, lit, lit_body];
    } else {
      body @ [fill_hole_msg, lit];
    };
  let body =
    if (pat_open) {
      body @ [pat, pat_body];
    } else {
      body @ [pat];
    };
  let body =
    if (binding_open) {
      body @ [binding, binding_body];
    } else {
      body @ [binding];
    };
  let body =
    if (wild_open) {
      body @ [wild, wild_body];
    } else {
      body @ [wild];
    };

  Vdom.(Node.div([Attr.classes(["type-driven"])], body));
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
    ) => {
  switch (cursor_info.cursor_term) {
  | Pat(_, EmptyHole(_)) => view_pat(~inject, cursor_inspector, cursor_info)
  | Exp(_, EmptyHole(_))
  | _ => view_exp(~inject, cursor_inspector, cursor_info)
  };
};
