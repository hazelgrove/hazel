open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let remove = id => Update.PerformAction(Project(Remove(id)));

let put = (bool: bool) =>
  Example.mk_monotile(Form.mk_atomic(Exp, string_of_bool(bool)));

let get = piece =>
  switch (CheckboxProjectorCore.state_of(piece)) {
  | None => failwith("toggle: not a checkbox")
  | Some(s) => s
  };

let toggle = (piece: Piece.t) => {
  let cur =
    switch (CheckboxProjectorCore.state_of(piece)) {
    | None => failwith("toggle: not a checkbox")
    | Some(s) => s
    };
  put(!cur);
};

let toggle = id =>
  UpdateAction.PerformAction(Project(UpdateSyntax(id, toggle)));

let base = (clss, syntax, id, ~font_metrics, ~inject, ~measurement) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector", "checkbox"] @ clss),
        JsUtil.stop_mousedown_propagation,
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [
      Node.input(
        ~attr=
          Attr.many(
            [
              Attr.create("type", "checkbox"),
              Attr.on_input((_evt, _str) => {inject(toggle(id))}),
              JsUtil.stop_mousedown_propagation,
            ]
            @ (get(syntax) ? [Attr.checked] : []),
          ),
        [],
      ),
      PieceDec.convex_shard(~font_metrics, ~measurement),
    ],
  );

let key_handler = (id: Id.t, key: Key.t): option(UpdateAction.t) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(remove(id))
  | _ => None
  };

let mk =
    (id: Id.t, syntax: Piece.t, model: Projector.fold): ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = Projector.checkbox;
     let model = model;
     let id = id;
     let syntax = syntax;
     let normal = base([], syntax, id);
     let indicated = base(["indicated"], syntax, id);
     let key_handler = key_handler(id);
     let ci_string = () => "F";
   });
