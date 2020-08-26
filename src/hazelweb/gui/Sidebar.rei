open Virtual_dom;

let left:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~is_open: bool,
    unit => list(Vdom.Node.t)
  ) =>
  Vdom.Node.t;

let right:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~is_open: bool,
    unit => list(Vdom.Node.t)
  ) =>
  Vdom.Node.t;
