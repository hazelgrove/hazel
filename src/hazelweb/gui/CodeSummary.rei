open Virtual_dom;

let get_mapping: (ExplanationInfo.explanation_info, bool) => ColorSteps.t;

let build_msg: (string, bool) => (list(Vdom.Node.t), ColorSteps.t);

let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ExplanationInfo.explanation_info,
    bool
  ) =>
  Vdom.Node.t;
