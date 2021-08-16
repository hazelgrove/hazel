module Vdom = Virtual_dom.Vdom;
open Sexplib.Std;

[@deriving (sexp, show)]
type t = {
  name: string,
  caption: [@sexp.opaque] [@show.opaque] Vdom.Node.t,
  init_zexp: ZExp.t,
};
