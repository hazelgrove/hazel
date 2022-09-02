open Virtual_dom;

/**
 * The top-level page.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Effect.t(unit),
    ~model: Model.t,
    ~result: Result.t
  ) =>
  Vdom.Node.t;
