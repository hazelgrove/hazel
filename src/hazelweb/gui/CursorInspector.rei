open Virtual_dom;

/**
 * Typing information at the cursor.
 */
let view:
  (~inject: ModelAction.t => Vdom.Event.t, ~loc: (float, float), Model.t) =>
  Vdom.Node.t;
