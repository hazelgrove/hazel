open Virtual_dom;

let view:
  (~inject: ModelAction.t => Vdom.Event.t, ~model: Model.t) => Vdom.Node.t;
