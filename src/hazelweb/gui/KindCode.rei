open Virtual_dom;

let view:
  (
    ~width: int=?,
    ~pos: int=?,
    ~diff_steps: list(CursorPath.steps)=?,
    Kind.t
  ) =>
  Vdom.Node.t;
