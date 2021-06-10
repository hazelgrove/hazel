module Vdom = Virtual_dom.Vdom;
open Vdom;
open StrategyGuide_common;

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

/**
 * Create a div containing divs for all operator options that will be shown.
 * Return a Node.t
 */
let operator_options = cursor_info => {
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

let keyword_node = text =>
  Node.div([Attr.classes(["code-font", "keyword"])], [Node.text(text)]);

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

let exp_hole_view =
    (
      ~inject: ModelAction.t => Event.t,
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
    ) => {
  let lit_open = cursor_inspector.type_assist_lit;
  let var_open = cursor_inspector.type_assist_var;
  let fun_open = cursor_inspector.type_assist_fun;
  let branch_open = cursor_inspector.type_assist_branch;
  let new_var_open = cursor_inspector.type_assist_new_var;
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
    Node.div(
      [
        Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
        Attr.on_click(_ => {
          Event.Many([
            Event.Prevent_default,
            Event.Stop_propagation,
            inject(ModelAction.UpdateSettings(CursorInspector(setting))),
          ])
        }),
      ],
      [Node.text(text), subsection_arrow],
    );
  };

  let var_ctx = extract_vars(ctx, typ);

  let lit_msg_exp = (ty: HTyp.t) => {
    let fun_lit =
      option([
        Node.text("Enter a Function Literal"),
        fill_space,
        shortcut_node("\\"),
      ]);
    let sum_lit =
      option([
        Node.text("Enter an Injection Literal"),
        fill_space,
        shortcut_node("Alt+l"),
        Node.text("or"),
        shortcut_node("Alt+r"),
      ]);
    let prod_lit =
      option([
        Node.text("Enter a Tuple Literal"),
        fill_space,
        shortcut_node("("),
      ]);
    let list_lit =
      option([
        Node.text("Enter an Empty List Literal"),
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
      Toggle_type_assist_lit,
      /* TODO: a vs an*/
      "Will a "
      ++ type_to_str(~empty_hole=true, ty)
      ++ " literal give what you need?",
      lit_open,
    );
  let lit_body =
    Node.div(
      [Attr.classes(["panel-title-bar", "body-bar"])],
      [Node.div([Attr.classes(["options"])], lit_msg_exp(typ))],
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
      Toggle_type_assist_var,
      "Is there a variable that represents what you need?",
      var_open,
    );
  let var_body =
    Node.div([Attr.classes(["panel-title-bar", "body-bar"])], [vars_view]);

  let fun_h =
    subsection_header(
      Toggle_type_assist_fun,
      "Is there a function that will calculate what you need?",
      fun_open,
    );
  let fun_ctx = fun_vars(ctx, typ);
  let fun_ap_opt =
    option([
      Node.text("Apply a Function"),
      fill_space,
      shortcut_node("Space"),
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
      [fun_ap_opt, ...list_vars_view(fun_vars(ctx, typ))];
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
      Toggle_type_assist_branch,
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
            shortcut_node("Alt+c"),
          ],
        ),
      ],
    );

  let new_var =
    subsection_header(
      Toggle_type_assist_new_var,
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
    subsection_header(Toggle_type_assist_other, "Other Actions", other_open);
  let other_body =
    Node.div(
      [Attr.classes(["panel-title-bar", "body-bar"])],
      [
        Node.div(
          [Attr.classes(["option"])],
          [Node.text("Parenthesize"), fill_space, shortcut_node("(")],
        ),
        Node.div(
          [Attr.classes(["option"])],
          [
            Node.text("Move to next/previous hole"),
            fill_space,
            shortcut_node("Tab"),
            shortcut_node("Shift+Tab"),
          ],
        ),
        Node.div(
          [Attr.classes(["option"])],
          [
            Node.text("Swap line up/down"),
            fill_space,
            shortcut_node("Ctrl+Alt+i"),
            shortcut_node("Ctrl+Alt+k"),
          ],
        ),
        Node.div(
          [Attr.classes(["option"])],
          [
            Node.text("Swap operand left/right"),
            fill_space,
            shortcut_node("Ctrl+Alt+j"),
            shortcut_node("Ctrl+Alt+l"),
          ],
        ),
      ],
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

let is_complex_type = (cursor_info: CursorInfo.t) => {
  switch (cursor_info.typed) {
  | OnType(b) => b
  | _ => false
  };
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

let shortcut_node = text =>
  Vdom.Node.div(
    [Vdom.Attr.classes(["code-font", "shortcut"])],
    [Vdom.Node.text(text)],
  );

let fill_space = Vdom.(Node.span([Attr.classes(["filler"])], []));

let type_driven = body =>
  Vdom.(Node.div([Attr.classes(["type-driven"])], body));

let rules_view = (cursor_info: CursorInfo.t) => {
  /* TOOD: Make work for if on outermost part of rule exp? */
  switch (cursor_info.cursor_term) {
  | Rule(OnDelim(0, After), _)
  | Exp(OnDelim(1, Before), Case(_)) =>
    Some(
      type_driven([
        Vdom.(
          Node.div(
            [Attr.classes(["panel-title-bar", "body-bar"])],
            [
              Node.div(
                [Attr.classes(["option"])],
                [
                  Node.text("Add rule before"),
                  fill_space,
                  shortcut_node("Enter"),
                ],
              ),
            ],
          )
        ),
      ]),
    )
  | Rule(OnDelim(1, Before), _) =>
    Some(
      type_driven([
        Vdom.(
          Node.div(
            [Attr.classes(["panel-title-bar", "body-bar"])],
            [
              Node.div(
                [Attr.classes(["option"])],
                [
                  Node.text("Add rule after"),
                  fill_space,
                  shortcut_node("Enter"),
                ],
              ),
            ],
          )
        ),
      ]),
    )
  | _ => None
  };
};

let lines_view = () => {
  type_driven([
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Node.div(
            [Attr.classes(["option"])],
            [
              Node.text("Create new line"),
              fill_space,
              shortcut_node("Enter"),
            ],
          ),
          Node.div(
            [Attr.classes(["option"])],
            [
              Node.text("Create new comment line"),
              fill_space,
              shortcut_node("#"),
              shortcut_node("Shift+Enter"),
            ],
          ),
        ],
      )
    ),
  ]);
};

/**************************   TYPES    ****************************/

let get_shortcut = (typ: HTyp.t) => {
  switch (typ) {
  | HTyp.Int => "I"
  | HTyp.Float => "F"
  | HTyp.Bool => "B"
  | HTyp.List(_) => "["
  | HTyp.Sum(_, _) => "|"
  | HTyp.Prod(_) => "("
  | HTyp.Arrow(_, _) => ">"
  | _ => raise(Invalid_argument("Invalid HTyp"))
  };
};

let list_primitives_view = () => {
  open Vdom;
  let primitive_options =
    [HTyp.Int, HTyp.Float, HTyp.Bool]
    |> List.map(s => {
         let enter_msg = s == HTyp.Int ? "Enter an " : "Enter a ";
         Node.div(
           [Attr.classes(["option"])],
           [
             Node.text(enter_msg),
             Node.text(type_to_str(Some(s))),
             fill_space,
             shortcut_node(get_shortcut(s)),
           ],
         );
       });
  primitive_options;
};

let list_compounds_view = () => {
  open Vdom;
  let compound_options =
    [
      HTyp.Sum(HTyp.Hole, HTyp.Hole),
      HTyp.Prod([HTyp.Hole, HTyp.Hole]),
      HTyp.List(HTyp.Hole),
    ]
    |> List.map(s => {
         Node.div(
           [Attr.classes(["option"])],
           [
             Node.text("Enter a "),
             Node.text(type_to_str(Some(s))),
             Node.text(": "),
             HTypCode.view(~strategy_guide=true, s),
             fill_space,
             shortcut_node(get_shortcut(s)),
           ],
         )
       });
  compound_options;
};

let list_function_view = () => {
  open Vdom;
  let function_options =
    [HTyp.Arrow(HTyp.Hole, HTyp.Hole)]
    |> List.map(s => {
         Node.div(
           [Attr.classes(["option"])],
           [
             Node.text("Enter a "),
             Node.text(type_to_str(Some(s))),
             Node.text(": "),
             HTypCode.view(s),
             fill_space,
             shortcut_node(get_shortcut(s)),
           ],
         )
       });
  function_options;
};

let list_compounds_filled_view = () => {
  open Vdom;
  let compound_filled_options =
    [HTyp.Prod([HTyp.Hole, HTyp.Hole]), HTyp.List(HTyp.Hole)]
    |> List.map(s => {
         Node.div(
           [Attr.classes(["option"])],
           [
             Node.text("Create a "),
             Node.text(type_to_str(Some(s))),
             Node.text(": "),
             HTypCode.view(~strategy_guide=true, s),
             fill_space,
             shortcut_node(get_shortcut(s)),
           ],
         )
       });
  compound_filled_options;
};

let list_pat_filled_view = () => {
  open Vdom;
  let function_options =
    Node.div(
      [Attr.classes(["option"])],
      [Node.text("Create a PAT tuple "), fill_space, shortcut_node(",")],
    );
  [function_options];
};

let list_function_filled_view = () => {
  open Vdom;
  let function_options =
    [HTyp.Arrow(HTyp.Hole, HTyp.Hole)]
    |> List.map(s => {
         Node.div(
           [Attr.classes(["option"])],
           [
             Node.text("Create a "),
             Node.text(type_to_str(Some(s))),
             Node.text(": "),
             HTypCode.view(s),
             fill_space,
             shortcut_node(get_shortcut(s)),
           ],
         )
       });
  function_options;
};

let filled_type_view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
    ) => {
  let ty = get_type(cursor_info);
  let is_complex = is_complex_type(cursor_info);
  let ctx = cursor_info.ctx;

  let _ = ty;
  let _ = ctx;

  switch (ty) {
  | Some(Hole) => print_endline("Hole")
  | Some(Int) => print_endline("Int")
  | Some(Float) => print_endline("Float")
  | Some(Bool) => print_endline("Bool")
  | Some(List(_)) => print_endline("List")
  | _ => print_endline("else")
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

  let compound_filled_t = cursor_inspector.type_assist_comp_filled_ty;
  let function_filled_t = cursor_inspector.type_assist_func_filled_ty;
  let pat_filled_t = cursor_inspector.type_assist_pat_filled_ty;
  let new_type_filled_t = cursor_inspector.type_assist_new_type_filled_ty;

  let compound_filled_arrow =
    if (compound_filled_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
    };
  let compound_filled =
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(
                ModelAction.UpdateSettings(
                  CursorInspector(Toggle_type_assist_comp_filled_ty),
                ),
              ),
            ])
          ),
        ],
        [
          Node.text("Is converting to a compound type what you need?"),
          compound_filled_arrow,
        ],
      )
    );

  let compound_filled_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Vdom.(
            Node.div(
              [Attr.classes(["options"])],
              list_compounds_filled_view(),
            )
          ),
        ],
      )
    );
  let _ = compound_filled_body;

  let func_filled_arrow =
    if (function_filled_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
    };
  let func_filled =
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(
                ModelAction.UpdateSettings(
                  CursorInspector(Toggle_type_assist_func_filled_ty),
                ),
              ),
            ])
          ),
        ],
        [
          Node.text("Is converting to a function type what you need?"),
          func_filled_arrow,
        ],
      )
    );
  let func_filled_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Vdom.(
            Node.div(
              [Attr.classes(["options"])],
              list_function_filled_view(),
            )
          ),
        ],
      )
    );
  let _ = func_filled_body;

  let pat_filled_arrow =
    if (pat_filled_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
    };
  let pat_filled =
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(
                ModelAction.UpdateSettings(
                  CursorInspector(Toggle_type_assist_pat_filled_ty),
                ),
              ),
            ])
          ),
        ],
        [Node.text("Do you want to create a pattern?"), pat_filled_arrow],
      )
    );
  let pat_filled_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Vdom.(
            Node.div([Attr.classes(["options"])], list_pat_filled_view())
          ),
        ],
      )
    );
  let _ = pat_filled_body;

  let new_type_filled_arrow =
    if (new_type_filled_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
    };

  let new_type_filled =
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(
                ModelAction.UpdateSettings(
                  CursorInspector(Toggle_type_assist_new_type_filled_ty),
                ),
              ),
            ])
          ),
        ],
        [
          Node.text("Do you want to add another type?"),
          new_type_filled_arrow,
        ],
      )
    );

  let new_type_filled_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Vdom.(
            Node.div(
              [Attr.classes(["options"])],
              [
                Node.div(
                  [Attr.classes(["option"])],
                  [
                    Node.text("Add another type"),
                    fill_space,
                    shortcut_node(","),
                  ],
                ),
              ],
            )
          ),
        ],
      )
    );
  let _ = new_type_filled_body;

  let body =
    if (compound_filled_t) {
      List.append([fill_hole_msg], [compound_filled, compound_filled_body]);
    } else {
      List.append([fill_hole_msg], [compound_filled]);
    };
  let body =
    if (function_filled_t) {
      List.append(body, [func_filled, func_filled_body]);
    } else {
      List.append(body, [func_filled]);
    };
  let body =
    is_complex
      ? if (new_type_filled_t) {
          List.append(body, [new_type_filled, new_type_filled_body]);
        } else {
          List.append(body, [new_type_filled]);
        }
      : (
        if (pat_filled_t) {
          List.append(body, [pat_filled, pat_filled_body]);
        } else {
          List.append(body, [pat_filled]);
        }
      );

  Vdom.(Node.div([Attr.classes(["type-driven"])], body));
};

let type_view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
    ) => {
  let ty = get_type(cursor_info);
  let ctx = cursor_info.ctx;

  let _ = ty;
  let _ = ctx;

  let primitive_t = cursor_inspector.type_assist_prim_ty;
  let compound_t = cursor_inspector.type_assist_comp_ty;
  let function_t = cursor_inspector.type_assist_func_ty;

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
  let primitive_arrow =
    if (primitive_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
    };
  let primitive =
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(
                ModelAction.UpdateSettings(
                  CursorInspector(Toggle_type_assist_prim_ty),
                ),
              ),
            ])
          ),
        ],
        [Node.text("Is a Primitive type what you need?"), primitive_arrow],
      )
    );
  let primitive_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Vdom.(
            Node.div([Attr.classes(["options"])], list_primitives_view())
          ),
        ],
      )
    );
  let _ = primitive_body;

  let compound_arrow =
    if (compound_t) {
      Icons.down_arrow(["fill-arrow"]);
    } else {
      Icons.left_arrow(["fill-arrow"]);
    };
  let compound =
    Vdom.(
      Node.div(
        [
          Attr.classes(["title-bar", "panel-title-bar", "fill-bar"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(
                ModelAction.UpdateSettings(
                  CursorInspector(Toggle_type_assist_comp_ty),
                ),
              ),
            ])
          ),
        ],
        [Node.text("Is a Compound type what you need?"), compound_arrow],
      )
    );
  let compound_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Vdom.(
            Node.div([Attr.classes(["options"])], list_compounds_view())
          ),
        ],
      )
    );
  let _ = compound_body;

  let func_arrow =
    if (function_t) {
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
              inject(
                ModelAction.UpdateSettings(
                  CursorInspector(Toggle_type_assist_func_ty),
                ),
              ),
            ])
          ),
        ],
        [Node.text("Is a Function type what you need?"), func_arrow],
      )
    );
  let func_body =
    Vdom.(
      Node.div(
        [Attr.classes(["panel-title-bar", "body-bar"])],
        [
          Vdom.(Node.div([Attr.classes(["options"])], list_function_view())),
        ],
      )
    );
  let _ = func_body;

  let body =
    if (primitive_t) {
      List.append([fill_hole_msg], [primitive, primitive_body]);
    } else {
      List.append([fill_hole_msg], [primitive]);
    };
  let body =
    if (compound_t) {
      List.append(body, [compound, compound_body]);
    } else {
      List.append(body, [compound]);
    };
  let body =
    if (function_t) {
      List.append(body, [func, func_body]);
    } else {
      List.append(body, [func]);
    };

  Vdom.(Node.div([Attr.classes(["type-driven"])], body));
};
