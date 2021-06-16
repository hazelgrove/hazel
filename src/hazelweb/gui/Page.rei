open Virtual_dom;

/**
 * The top-level page.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~sync_livelit: ModelAction.t => unit,
    Model.t
  ) =>
  Vdom.Node.t;
