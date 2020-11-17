open Virtual_dom;

let view_of_hole_instance:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~width: int,
    ~pos: int=?,
    ~settings: Settings.Evaluation.t,
    ~selected_hole_instance: option(NodeInstance.t),
    NodeInstance.t
  ) =>
  Vdom.Node.t;

let view_of_var: string => Vdom.Node.t;

let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~settings: Settings.Evaluation.t,
    ~selected_hole_instance: option(NodeInstance.t)=?,
    ~width: int,
    ~pos: int=?,
    DHExp.t
  ) =>
  Vdom.Node.t;
