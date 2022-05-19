open Virtual_dom;

let view:
  (~settings: Settings.t, ExplanationInfo.explanation_info, int) => Vdom.Node.t;
