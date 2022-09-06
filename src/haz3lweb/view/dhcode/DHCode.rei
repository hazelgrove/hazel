open Virtual_dom;
open Haz3lcore;

let view_of_hole_instance:
  (
    ~inject: Update.t => Vdom.Effect.t(unit),
    ~width: int,
    ~pos: int=?,
    ~selected_hole_instance: option(HoleInstance.t),
    ~settings: Settings.Evaluation.t,
    ~font_metrics: FontMetrics.t,
    HoleInstance.t
  ) =>
  Vdom.Node.t;

let view_of_var: string => Vdom.Node.t;

let view:
  (
    ~inject: Update.t => Vdom.Effect.t(unit),
    ~settings: Settings.Evaluation.t,
    ~selected_hole_instance: option(HoleInstance.t),
    ~font_metrics: FontMetrics.t,
    ~width: int,
    ~pos: int=?,
    DHExp.t
  ) =>
  Vdom.Node.t;

let view_tylr:
  (
    ~settings: Settings.Evaluation.t,
    ~selected_hole_instance: option(HoleInstance.t),
    ~font_metrics: FontMetrics.t,
    ~width: int,
    ~pos: int=?,
    DHExp.t
  ) =>
  Vdom.Node.t;

type font_metrics = FontMetrics.t;
