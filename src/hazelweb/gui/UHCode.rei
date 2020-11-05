open Virtual_dom;

let focus: unit => unit;

/**
 * Code representation of UHExp.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~font_metrics: FontMetrics.t,
    ~measure: bool,
    ~is_mac: bool,
    ~cursor_inspector: Model.cursor_inspector,
    Program.t
  ) =>
  Vdom.Node.t;
