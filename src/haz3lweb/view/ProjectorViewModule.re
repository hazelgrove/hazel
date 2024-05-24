open Haz3lcore;
open Virtual_dom.Vdom;

module type PV = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;
  let data: t;

  let normal:
    (
      Id.t,
      ~font_metrics: FontMetrics.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~measurement: Measured.measurement
    ) =>
    Node.t;

  let indicated:
    (
      Id.t,
      ~font_metrics: FontMetrics.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~measurement: Measured.measurement
    ) =>
    Node.t;

  let key_handler: (Id.t, Key.t) => option(UpdateAction.t);
  let ci_string: unit => string; //Projector //TODO: rename to ci_string or something
};

type t = (module PV);
