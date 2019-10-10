module Vdom = Virtual_dom.Vdom;

type t = {
  header: Vdom.Node.t,
  caption: Vdom.Node.t,
  init_zblock: ZExp.zblock,
};
