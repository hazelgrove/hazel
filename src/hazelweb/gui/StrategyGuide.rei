open Virtual_dom;

/**
 * Strategy Guide at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    Model.cursor_inspector,
    CursorInfo_common.t
  ) =>
  Vdom.Node.t;
