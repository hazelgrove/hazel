open Virtual_dom;

/**
 * Code representation of UHExp.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~font_metrics: FontMetrics.t,
    ~settings: Settings.t,
    ~cursor_inspector: CursorInspectorModel.t,
    ~program: Program.t
  ) =>
  Vdom.Node.t;
