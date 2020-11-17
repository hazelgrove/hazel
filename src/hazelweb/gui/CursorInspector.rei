open Virtual_dom;

/**
 * Typing information at the cursor.
 */
let view:
  (~inject: ModelAction.t => Vdom.Event.t, option(CursorInfo.t)) =>
  Vdom.Node.t;
