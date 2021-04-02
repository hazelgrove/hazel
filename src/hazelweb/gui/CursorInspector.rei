open Virtual_dom;

/**
 * Typing information at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~font_metrics: FontMetrics.t,
    (float, float),
    Settings.CursorInspector.t,
    CursorInfo.t
  ) =>
  Vdom.Node.t;
