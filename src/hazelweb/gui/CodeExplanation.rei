open Virtual_dom;
open Virtual_dom.Vdom;

let get_mapping: (~settings: DocumentationStudySettings.t) => ColorSteps.t;

let view:
  (
    ~settings: DocumentationStudySettings.t,
    ~inject: ModelAction.t => Event.t,
    list(Prompt.explain),
    string
  ) =>
  Vdom.Node.t;
