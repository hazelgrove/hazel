let view:
  (
    ~inject: ModelAction.t => Virtual_dom.Vdom.Event.t,
    ~selected_tag_hole: option(MetaVar.t),
    Model.t
  ) =>
  Virtual_dom.Vdom.Node.t;
