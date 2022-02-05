open Virtual_dom;

let import:
  (~inject: ModelAction.t => Vdom.Event.t, ~is_open: bool, ~model: Model.t) =>
  Vdom.Node.t;

let export:
  (~inject: ModelAction.t => Vdom.Event.t, ~is_open: bool, ~model: Model.t) =>
  Vdom.Node.t;
