open Virtual_dom.Vdom;
open Haz3lcore;
// open Util.Web;
// open Util;

let prov_view: Haz3lcore.Typ.type_provenance => string =
  fun
  | Internal => ""
  | TypeHole => "𝜏"
  | SynSwitch => "⇒";

let rec type_view = (ty: Haz3lcore.Typ.t): string =>
  //TODO: parens on ops when ambiguous
  switch (ty) {
  | Unknown(prov) => "?" ++ prov_view(prov)
  | Int => "Int"
  | Float => "Float"
  | String => "String"
  | Bool => "Bool"
  | Var(name) => name
  | List(t) => "[" ++ type_view(t) ++ "]"
  | Arrow(t1, t2) => type_view(t1) ++ "->" ++ type_view(t2)
  | Prod([]) => "Unit"
  | Prod([_]) => "BadProduct"
  | Prod([t0, ...ts]) =>
    "("
    ++ String.concat(
         type_view(t0),
         List.map(t => [",", type_view(t)], ts) |> List.flatten,
       )
    ++ ")"
  | Sum(t1, t2) => type_view(t1) ++ "+" ++ type_view(t2)
  };

let zipper_type_view = (zipper: Zipper.t, info_map: Statics.map) => {
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
              | SynConsistent(typ) => type_view(typ)
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

type position_mode =
  | Line
  | Character;

let position_view = (editor: Editor.t) => {
  // when the last action is moving up or down, we will read the full line, otherwise the character the cursor at
  let pos_mode =
    switch (editor.history) {
    | ([], _) => Line
    | ([(a, _), ..._], _) =>
      switch (a) {
      | Move(Extreme(Up))
      | Move(Extreme(Down))
      | Move(Local(Up))
      | Move(Local(Down)) => Line
      | _ => Character
      }
    };

  let program = Printer.to_string_editor(editor);
  let rows = String.split_on_char('\n', program);
  switch (Editor.caret_point(editor)) {
  | {row, col} =>
    switch (List.nth_opt(rows, row)) {
    | Some(str) =>
      switch (pos_mode) {
      | Line => str
      | Character => String.sub(str, max(0, col - 1), 1)
      }
    | None => ""
    }
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

  let zipper_type = zipper_type_view(zipper, info_map);
  let line_str = position_view(model.editors |> Editors.get_editor);

  let alert_content: string =
    line_str ++ " . The expression has type " ++ zipper_type;
  let alert = Node.span([Node.text(alert_content)]);
  Node.div(
    ~attr=
      Attr.many([
        Attr.id("accessibility"),
        Attr.classes(["visually-hidden"]),
        Attr.create("role", "alert"),
      ]),
    [alert],
  );
};
