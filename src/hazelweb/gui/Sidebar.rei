open Virtual_dom;

let left:
  (~inject: ModelAction.t => Vdom.Effect.t(unit), ~model: Model.t) =>
  Vdom.Node.t;

let right:
  (
    ~inject: ModelAction.t => Vdom.Effect.t(unit),
    ~model: Model.t,
    ~result: Result.t
  ) =>
  Vdom.Node.t;
