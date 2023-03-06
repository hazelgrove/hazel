open Virtual_dom.Vdom;
open Haz3lcore;
// open Util.Web;
// open Util;

let prov_string: Haz3lcore.Typ.type_provenance => string =
  fun
  | Internal => ""
  | TypeHole => "𝜏"
  | SynSwitch => "⇒";

let rec type_string = (ty: Haz3lcore.Typ.t): string =>
  //TODO: parens on ops when ambiguous
  switch (ty) {
  | Unknown(prov) => "?" ++ prov_string(prov)
  | Int => "Int"
  | Float => "Float"
  | String => "String"
  | Bool => "Bool"
  | Var(name) => name
  | List(t) => "[" ++ type_string(t) ++ "]"
  | Arrow(t1, t2) => type_string(t1) ++ "->" ++ type_string(t2)
  | Prod([]) => "Unit"
  | Prod([_]) => "BadProduct"
  | Prod([t0, ...ts]) =>
    "("
    ++ String.concat(
         type_string(t0),
         List.map(t => [",", type_string(t)], ts) |> List.flatten,
       )
    ++ ")"
  | Sum(t1, t2) => type_string(t1) ++ "+" ++ type_string(t2)
  };

let zipper_type_string = (zipper: Zipper.t, info_map: Statics.map) => {
  let backpack = zipper.backpack;
  let alert_content =
    if (List.length(backpack) > 0) {
      "";
    } else {
      let index = Haz3lcore.Indicated.index(zipper);

      switch (index) {
      | Some(index) =>
        switch (Haz3lcore.Id.Map.find_opt(index, info_map)) {
        | Some(ci) =>
          switch (ci) {
          | Invalid(_)
          | InfoPat(_)
          | InfoTyp(_)
          | InfoRul(_) => ""
          | InfoExp({mode, self, _}) =>
            let error_status = Haz3lcore.Statics.error_status(mode, self);
            switch (error_status) {
            | InHole(_) => ""
            | NotInHole(happy) =>
              switch (happy) {
              | SynConsistent(typ) => type_string(typ)
              | _ => ""
              }
            };
          }
        | None => ""
        }
      | None => ""
      };
    };
  alert_content;
};

let action_move_string = (editor: Editor.t, action: Action.t) => {
  // when the last action is moving up or down, we will read the full line, otherwise the character the cursor at
  let is_line_needed =
    switch (action) {
    | Move(Extreme(Up))
    | Move(Extreme(Down))
    | Move(Local(Up))
    | Move(Local(Down)) => true
    | _ => false
    };

  let program = Printer.to_string_editor(editor);
  let rows = String.split_on_char('\n', program);
  switch (Editor.caret_point(editor)) {
  | {row, col} =>
    switch (List.nth_opt(rows, row)) {
    | Some(str) =>
      is_line_needed
        ? str
        : (
          switch (String.sub(str, min(col, String.length(str) - 1), 1)) {
          | s => s
          | exception (Invalid_argument(_)) => ""
          }
        )
    | None => ""
    }
  };
};

let action_select_string = (editor: Editor.t) => {
  let content = editor.state.zipper.selection.content;
  "select " ++ Printer.of_segment(content);
};

let action_pickup_string = (editor: Editor.t) => {
  let backpack = editor.state.zipper.backpack;
  switch (backpack) {
  | [] => ""
  | [first, ..._] => "pick up " ++ Printer.of_segment(first.content)
  };
};

let action_rotate_backpack_string = (editor: Editor.t) => {
  let backpack = editor.state.zipper.backpack;
  switch (backpack) {
  | [] => ""
  | [first, ..._] => "rotate to " ++ Printer.of_segment(first.content)
  };
};

let action_string = (action: Action.t, editor: Editor.t) => {
  switch (action) {
  | Move(_) => action_move_string(editor, action)
  | Select(_) => action_select_string(editor)
  | Unselect => "unselected"
  | Pick_up => action_pickup_string(editor)
  | MoveToBackpackTarget(_) => "" // TODO
  | Put_down => "put down"
  | RotateBackpack => action_rotate_backpack_string(editor)
  | Destruct(_)
  | Insert(_)
  | Jump(_) => ""
  };
};

let view =
    //~inject,
    //~settings,
    //~show_lang_doc: bool,
    //zipper: Haz3lcore.Zipper.t,
    //info_map: Haz3lcore.Statics.map,
    (model: Model.t) => {
  let zipper = Editors.get_zipper(model.editors);
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let info_map = Statics.mk_map(term);

  let zipper_type = zipper_type_string(zipper, info_map);
  let editor = Editors.get_editor(model.editors);
  let action_str =
    switch (editor.history) {
    | (_, []) => ""
    | (_, [(action, _), ..._]) => action_string(action, editor)
    };

  let type_string: string = "The expression has type " ++ zipper_type;
  let alert = Node.span([Node.text(action_str), Node.text(type_string)]);
  let alert_div =
    Node.div(
      ~attr=
        Attr.many([
          Attr.id("accessibility"),
          Attr.classes(["visually-hidden"]),
          Attr.create("area-hidden", "true"),
          //Attr.create("role", "alert"),
        ]),
      [alert],
    );
  Node.div([JsUtil.clipboard_shim(~text="", ()), alert_div]);
};
