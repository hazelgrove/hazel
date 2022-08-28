open Virtual_dom;

/**
 * Typing information at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Effect.t(unit),
    ~loc: (float, float),
    ~test_inspector: KeywordID.t => option(Vdom.Node.t),
    CursorInspectorModel.t,
    CursorInfo.t
  ) =>
  Vdom.Node.t;
