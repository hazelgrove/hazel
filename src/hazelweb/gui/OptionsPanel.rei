open Virtual_dom;

/**
 * Options for computing results, benchmarking, etc.
 */
let view: (~inject: ModelAction.t => Vdom.Event.t, Model.t) => Vdom.Node.t;

let labeled_checkbox:
  (
    ~id: string,
    ~classes: List.t(string)=?,
    ~label: string,
    ~on_change: unit => Vdom.Event.t,
    ~disabled: bool=?,
    bool
  ) =>
  Vdom.Node.t;
