open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let remove = id => Update.PerformAction(Project(Remove(id)));

let base = (clss, id, ~font_metrics, ~inject, ~measurement) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector", "fold"] @ clss),
        JsUtil.stop_mousedown_propagation,
        Attr.on_double_click(_ => inject(remove(id))),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [text("⋱"), PieceDec.convex_shard(~font_metrics, ~measurement)],
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
     type model = Projector.fold;
     let model = model;
     let id = id;
     let syntax = syntax;
     let normal = base([], id);
     let indicated = base(["indicated"], id);
     let key_handler = key_handler(id);
     let ci_string = () => "F";
   });
