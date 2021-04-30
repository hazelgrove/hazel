open Virtual_dom;
module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

let left:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~is_open: bool,
    unit => list(Vdom.Node.t)
  ) =>
  Vdom.Node.t;

let right:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~is_open: bool,
    list((bool, Vdom.Node.t, Vdom.Event.t, unit => Vdom.Node.t))
  ) =>
  Vdom.Node.t;
