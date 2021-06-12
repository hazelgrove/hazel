open Virtual_dom;

/**
 * Typing information at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~font_metrics: FontMetrics.t,
    ~settings: Settings.t,
    array(Program.typ),
    (float, float),
    Settings.CursorInspector.t,
    CursorInfo.t,
    MetaVarGen.t
  ) =>
  Vdom.Node.t;
