open Virtual_dom;

let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ExplanationInfo.explanation_info,
    bool
  ) =>
  Vdom.Node.t;
