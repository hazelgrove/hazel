module Vdom = Virtual_dom.Vdom;
open Vdom;
open StrategyGuide_common;

let val_msg_pat = (ty: HTyp.t) => {
  switch (ty) {
  | Hole => [int_lit, float_lit, bool_lit]
  | Int => [int_lit]
  | Float => [float_lit]
  | Bool => [bool_lit]
  | Arrow(_, _)
  | Sum(_, _)
  | Prod(_)
  | List(_) => [option([Node.text("No suggestions.")])]
  };
};

/**
 * Create a list of divs for the var options that will be shown.
 * Return list of Node.t
 */
let list_any_view = (ty: HTyp.t) => {
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
      Node.div([Attr.classes(["option"])], [code_node(binding)])
    },
    suggestions,
  );
};

/**
 * Create a list of divs for the var options that will be shown.
 * Return list of Node.t
 */
let list_ignore_view = () => {
  let suggestions = ["_"];
  List.map(
    binding => {
      Node.div([Attr.classes(["option"])], [code_node(binding)])
    },
    suggestions,
  );
};

let structure_msg = (ty: HTyp.t) => {
  let prod_pat =
    option([
      Node.text("Enter a Tuple pattern"),
      fill_space,
      shortcut_node("("),
    ]);
  let sum_pat =
    option([
      Node.text("Enter an Injection"),
      fill_space,
      shortcut_node("Alt + l"),
      Node.text("or"),
      shortcut_node("Alt + r"),
    ]);
  let list_pat =
    option([
      Node.text("Enter a Non-empty List pattern"),
      fill_space,
      shortcut_node(";"),
    ]);
  let list_lit_pat =
    option([Node.text("Enter a List"), fill_space, shortcut_node("[")]);
  switch (ty) {
  | Hole => [sum_pat, prod_pat, list_pat, list_lit_pat]
  | Prod(_) => [prod_pat]
  | List(_) => [list_pat, list_lit_pat]
  | Sum(_) => [sum_pat]
  | _ => [option([Node.text("No suggestions.")])]
  };
};

let pat_hole_view =
    (
      ~inject: ModelAction.t => Event.t,
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
    ) => {
  let val_open = cursor_inspector.type_assist_val;
  let structure_open = cursor_inspector.type_assist_structure;
  let any_open = cursor_inspector.type_assist_any;
  let ignore_open = cursor_inspector.type_assist_ignore;

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

  let fill_hole_msg =
    Node.div(
      [Attr.classes(["title-bar", "panel-title-bar", "main-fill"])],
      [
        Node.div(
          [Attr.classes(["words"])],
          [Node.text("Which strategy do you want to try?")],
        ),
      ],
    );

  let value =
    subsection_header(
      Toggle_type_assist_val,
      "Match with a primitive value?",
      val_open,
    );
  let val_body =
    Node.div(
      [Attr.classes(["panel-title-bar", "body-bar"])],
      [Node.div([Attr.classes(["options"])], val_msg_pat(typ))],
    );

  let structure =
    subsection_header(
      Toggle_type_assist_structure,
      "Match with a structure?",
      structure_open,
    );
  let structure_body =
    Node.div(
      [Attr.classes(["panel-title-bar", "body-bar"])],
      [Node.div([Attr.classes(["options"])], structure_msg(typ))],
    );

  let any_view = Node.div([Attr.classes(["options"])], list_any_view(typ));
  let any =
    subsection_header(
      Toggle_type_assist_any,
      "Bind to a variable?",
      any_open,
    );
  let any_body =
    Node.div([Attr.classes(["panel-title-bar", "body-bar"])], [any_view]);

  let ignore_view =
    Node.div([Attr.classes(["options"])], list_ignore_view());
  let ignore =
    subsection_header(Toggle_type_assist_ignore, "Ignore?", ignore_open);
  let ignore_body =
    Node.div(
      [Attr.classes(["panel-title-bar", "body-bar"])],
      [ignore_view],
    );

  let body = [];
  let body =
    if (val_open) {
      body @ [fill_hole_msg, value, val_body];
    } else {
      body @ [fill_hole_msg, value];
    };
  let body =
    if (structure_open) {
      body @ [structure, structure_body];
    } else {
      body @ [structure];
    };
  let body =
    if (any_open) {
      body @ [any, any_body];
    } else {
      body @ [any];
    };
  let body =
    if (ignore_open) {
      body @ [ignore, ignore_body];
    } else {
      body @ [ignore];
    };

  type_driven(body);
};

let var_hole_view =
    (
      ~inject: ModelAction.t => Event.t,
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
    ) => {
  let any_open = cursor_inspector.type_assist_any;
  let ignore_open = cursor_inspector.type_assist_ignore;

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

  let fill_hole_msg =
    Node.div(
      [Attr.classes(["title-bar", "panel-title-bar", "main-fill"])],
      [
        Node.div(
          [Attr.classes(["words"])],
          [Node.text("Which strategy do you want to try?")],
        ),
      ],
    );

  let ignore_view =
    Node.div([Attr.classes(["options"])], list_ignore_view());
  let ignore =
    subsection_header(Toggle_type_assist_ignore, "Ignore?", ignore_open);
  let ignore_body =
    Node.div(
      [Attr.classes(["panel-title-bar", "body-bar"])],
      [ignore_view],
    );

  let any_view = Node.div([Attr.classes(["options"])], list_any_view(typ));
  let any =
    subsection_header(
      Toggle_type_assist_any,
      "Bind to a variable?",
      any_open,
    );
  let any_body =
    Node.div([Attr.classes(["panel-title-bar", "body-bar"])], [any_view]);

  let body = [];
  let body =
    if (any_open) {
      body @ [fill_hole_msg, any, any_body];
    } else {
      body @ [fill_hole_msg, any];
    };
  let body =
    if (ignore_open) {
      body @ [ignore, ignore_body];
    } else {
      body @ [ignore];
    };

  StrategyGuide_common.type_driven(body);
};
