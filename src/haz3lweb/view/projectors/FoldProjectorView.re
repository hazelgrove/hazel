open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let wrap =
    (
      ~font_metrics,
      ~measurement: Measured.measurement,
      clss, // include projector name
      guy,
    ) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector"] @ clss),
        JsUtil.stop_mousedown_propagation,
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [guy, PieceDec.convex_shard(~font_metrics, ~measurement)],
  );

let base = (~font_metrics, ~measurement: Measured.measurement, ~inject, clss) =>
  wrap(
    ~font_metrics,
    ~measurement,
    ["fold", ...clss],
    div(
      ~attr=Attr.on_double_click(_ => inject(Projector.Remove)),
      [text("⋱")],
    ),
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
