open Virtual_dom;

/**
 * Options for computing results, benchmarking, etc.
 */
let view:
  (~inject: ModelAction.t => Vdom.Effect.t(unit), Settings.t) => Vdom.Node.t;

let labeled_checkbox:
  (
    ~id: string,
    ~classes: List.t(string)=?,
    ~label: string,
    ~on_change: unit => Vdom.Effect.t(unit),
    ~disabled: bool=?,
    bool
  ) =>
  Vdom.Node.t;
