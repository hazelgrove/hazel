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
    ~settings: Settings.t,
    ~cursor_inspector: CursorInspectorModel.t,
    Program.t
  ) =>
  Vdom.Node.t;
