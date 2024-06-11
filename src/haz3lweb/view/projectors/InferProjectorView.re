open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let base =
    (
      clss,
      expected_ty: option(Typ.t),
      ~font_metrics,
      ~inject,
      ~measurement: Measured.measurement,
    ) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector", "infer"] @ clss),
        JsUtil.stop_mousedown_propagation,
        Attr.on_double_click(_ => inject(Projector.Remove)),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [
      text(expected_ty |> InferProjectorCore.display_ty |> Typ.pretty_print),
      PieceDec.convex_shard(~font_metrics, ~measurement),
    ],
  );

let key_handler = (key: Key.t): option(Projector.action(unit)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk =
    (syntax: Piece.t, model: Projector.infer, ~inject): ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = Projector.infer;
     type action = unit;
     let model = model;
     let syntax = syntax;
     let inject = inject;
     let normal = base([], model.expected_ty, ~inject);
     let indicated = base(["indicated"], model.expected_ty, ~inject);
     let key_handler = key_handler;
     let ci_string: unit => string = _ => "I";
   });
