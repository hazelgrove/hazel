open Virtual_dom;

let view:
  (
    ~inject: ModelAction.t => Ui_event.t,
    ~selected_tag_hole: option(MetaVar.t),
    ~width: int=?,
    ~pos: int=?,
    ~diff_steps: list(CursorPath.steps)=?,
    HTyp.t
  ) =>
  Vdom.Node.t;
