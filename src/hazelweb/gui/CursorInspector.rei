open Virtual_dom;

/**
 * Typing information at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~loc: (float, float),
    ~assert_inspector: KeywordID.t => Vdom.Node.t,
    CursorInspectorModel.t,
    CursorInfo.t
  ) =>
  Vdom.Node.t;
