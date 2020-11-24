open Virtual_dom;

/**
 * Typing information at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    // HACK to get around cyclical dependency between UHCode, CursorInspector, SynthPanel for now
    ~view_of_text: UHLayout.t => list(Vdom.Node.t),
    (float, float),
    Model.cursor_inspector,
    CursorInfo.t
  ) =>
  Vdom.Node.t;
