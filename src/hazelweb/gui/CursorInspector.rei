open Virtual_dom.Vdom;

/**
 * Typing information at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Event.t,
    ~selected_tag_hole: option(MetaVar.t),
    ~font_metrics: FontMetrics.t,
    ~loc: (float, float),
    CursorInspectorModel.t,
    CursorInfo.t
  ) =>
  Node.t;
