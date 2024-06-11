open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let base = (clss, ~font_metrics, ~inject, ~measurement) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector", "fold"] @ clss),
        JsUtil.stop_mousedown_propagation,
        Attr.on_double_click(_ => inject(Projector.Remove)),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [text("⋱"), PieceDec.convex_shard(~font_metrics, ~measurement)],
  );

let key_handler = (key: Key.t): option(Projector.action(unit)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Projector.Remove)
  | _ => None
  };

let mk =
    (syntax: Piece.t, model: Projector.fold, ~inject): ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = Projector.fold;
     type action = unit;
     let model = model;
     let syntax = syntax;
     let inject = inject;
     let normal = base([], ~inject);
     let indicated = base(["indicated"], ~inject);
     let key_handler = key_handler;
     let ci_string = () => "F";
   });
