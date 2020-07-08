open Virtual_dom;

/**
 * Code representation of UHExp.
 */
let view:
  (
    ~measure: bool,
    ~inject: ModelAction.t => Vdom.Event.t,
    ~font_metrics: FontMetrics.t,
    ~caret_pos: option((int, int)),
    UHLayout.t
  ) =>
  Vdom.Node.t;
