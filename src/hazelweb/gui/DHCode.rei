open Virtual_dom;

let view_of_hole_closure:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~width: int,
    ~pos: int=?,
    ~selected_hole_closure: option(HoleClosure.t),
    ~settings: Settings.Evaluation.t,
    ~font_metrics: FontMetrics.t,
    HoleClosure.t
  ) =>
  Vdom.Node.t;

let view_of_var: string => Vdom.Node.t;

let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~settings: Settings.Evaluation.t,
    ~selected_hole_closure: option(HoleClosure.t),
    ~font_metrics: FontMetrics.t,
    ~width: int,
    ~pos: int=?,
    DHExp.t
  ) =>
  Vdom.Node.t;
