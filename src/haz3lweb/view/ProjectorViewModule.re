open Haz3lcore;
open Virtual_dom.Vdom;

module type ProjectorView = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  type action;

  let model: model;
  let syntax: Piece.t;
  let inject: Projector.action(action) => Ui_effect.t(unit);

  let normal:
    (~font_metrics: FontMetrics.t, ~measurement: Measured.measurement) =>
    Node.t;

  let indicated:
    (~font_metrics: FontMetrics.t, ~measurement: Measured.measurement) =>
    Node.t;

  let key_handler: Key.t => option(Projector.action(action));
  let ci_string: unit => string;
};

type t = (module ProjectorView);
