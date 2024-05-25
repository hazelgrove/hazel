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

let mk = (data: Projector.fold): ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type t = Projector.fold;
     let data = data;
     let normal = base([]);
     let indicated = base(["indicated"]);
     let key_handler = (id, key: Key.t): option(UpdateAction.t) =>
       switch (key) {
       | {key: D("Escape"), _} => Some(remove(id))
       | _ => None
       };
     let ci_string = () => "F";
   });
