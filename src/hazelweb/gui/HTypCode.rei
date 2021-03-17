open Virtual_dom;

let view:
  (
    ~width: int=?,
    ~pos: int=?,
    ~strategy_guide: bool=?,
    ~diff_steps: list(CursorPath.steps)=?,
    HTyp.t
  ) =>
  Vdom.Node.t;
