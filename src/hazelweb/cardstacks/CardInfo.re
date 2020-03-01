module Vdom = Virtual_dom.Vdom;

type t = {
  caption: Vdom.Node.t,
  init_zexp: ZExp.t,
};

let put_zexp = (ze: ZExp.t, info: t) => {...info, init_zexp: ze};
