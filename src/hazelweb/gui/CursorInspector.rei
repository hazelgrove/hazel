open Virtual_dom.Vdom;

/**
 * Typing information at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Event.t,
    ~font_metrics: FontMetrics.t,
    ~selected_tag_hole: option(MetaVar.t),
    Model.t
  ) =>
  Node.t;
