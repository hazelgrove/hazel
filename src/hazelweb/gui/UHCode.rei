open Virtual_dom;

let root_id: string;
let focus: unit => unit;

/**
 * Code representation of UHExp.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~font_metrics: FontMetrics.t,
    ~is_mac: bool,
    ~settings: Settings.t,
    ~is_focused: bool,
    Program.t
  ) =>
  Vdom.Node.t;
