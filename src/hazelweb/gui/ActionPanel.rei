/** A sidebar panel listing available actions and associated hotkeys. */

let view:
  (~inject: ModelAction.t => Virtual_dom.Vdom.Event.t, Model.t) =>
  Virtual_dom.Vdom.Node.t;
