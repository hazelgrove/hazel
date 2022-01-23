open Virtual_dom;

/**
 * Typing information at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~loc: (float, float),
    Contexts.t,
    CursorInspectorModel.t,
    CursorInfo.t
  ) =>
  Vdom.Node.t;
