let kc_actions: Hashtbl.t(HazelKeyCombos.t, CursorInfo.t => Action.t);

let focus: unit => unit;

let view:
  (~inject: ModelAction.t => Virtual_dom.Vdom.Event.t, Model.t) =>
  Virtual_dom.Vdom.Node.t;
