module Vdom = Virtual_dom.Vdom;

type t = {
  caption: Vdom.Node.t,
  init_block: UHExp.block,
};
