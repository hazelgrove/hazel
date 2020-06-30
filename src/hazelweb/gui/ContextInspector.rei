open Virtual_dom;

exception InvalidInstance;

/**
 * Shows the typing context and environment at the cursor.
 */
let view: (~inject: ModelAction.t => Vdom.Event.t, Model.t) => Vdom.Node.t;
