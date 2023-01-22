open Virtual_dom.Vdom;
open Haz3lcore;
// open Util.Web;
// open Util;

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
  let alert_content: string = alert_content ++ " and testing hazel is great!";
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
