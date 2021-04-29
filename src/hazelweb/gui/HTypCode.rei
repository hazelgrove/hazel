open Virtual_dom;

let view:
  (
    ~width: int=?,
    ~pos: int=?,
    ~diff_steps: list(CursorPath.steps)=?,
    HTyp.t
  ) =>
  Vdom.Node.t;
