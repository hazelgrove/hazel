open Virtual_dom;

let text_editor:
  (~inject: ModelAction.t => Vdom.Event.t, ~is_open: bool, ~model: Model.t) =>
  Vdom.Node.t;
