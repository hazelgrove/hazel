open Virtual_dom;

/**
 * Typing information at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~font_metrics: FontMetrics.t,
    ~is_mac: bool,
    ~settings: Settings.t,
    bool,
    AssistantModel.t,
    (float, float),
    Settings.CursorInspector.t,
    CursorInfo.t,
    MetaVarGen.t
  ) =>
  Vdom.Node.t;
