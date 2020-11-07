let view:
  (
    ~inject: ModelAction.t => Virtual_dom.Vdom.Event.t,
    ~sync_livelit: ModelAction.t => unit,
    Model.t
  ) =>
  Virtual_dom.Vdom.Node.t;
