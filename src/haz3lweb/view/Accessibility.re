open Virtual_dom.Vdom;
open Haz3lcore;
// open Util.Web;
// open Util;

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
              | SynConsistent(typ) =>
                switch (typ) {
                | Int => "int"
                | _ => "unknown type"
                }
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

let line_view = (editor: Editor.t) => {
  let program = Printer.to_string_editor(editor);
  let rows = String.split_on_char('\n', program);
  switch (Editor.caret_point(editor)) {
  | {row, _} =>
    switch (List.nth_opt(rows, row)) {
    | Some(str) => str
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

  let alert_type = zipper_type_view(zipper, info_map);
  let line_str = line_view(model.editors |> Editors.get_editor);

  let alert_content: string =
    line_str ++ " and testing hazel is great!" ++ alert_type;
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
