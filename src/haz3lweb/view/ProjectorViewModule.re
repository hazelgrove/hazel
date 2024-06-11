open Haz3lcore;
open Virtual_dom.Vdom;

module type ProjectorView = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  let model: model;
  let id: Id.t;
  let syntax: Piece.t;

  let normal:
    (
      ~font_metrics: FontMetrics.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~measurement: Measured.measurement
    ) =>
    Node.t;

  let indicated:
    (
      ~font_metrics: FontMetrics.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~measurement: Measured.measurement
    ) =>
    Node.t;

  let key_handler: Key.t => option(UpdateAction.t);
  let ci_string: unit => string;
};

type t = (module ProjectorView);
