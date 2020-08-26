open Virtual_dom;

/**
 * The top-level page.
 */
let view: (~inject: ModelAction.t => Vdom.Event.t, Model.t) => Vdom.Node.t;
