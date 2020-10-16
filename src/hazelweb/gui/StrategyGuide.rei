open Virtual_dom;

/**
 * Strategy Guide at the cursor.
 */
let view: (~inject: ModelAction.t => Vdom.Event.t, Model.t) => Vdom.Node.t;
