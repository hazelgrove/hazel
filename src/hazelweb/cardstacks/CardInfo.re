module Vdom = Virtual_dom.Vdom;
open Sexplib.Std;

[@deriving sexp]
type t = {
  name: string,
  caption: [@sexp.opaque] Vdom.Node.t,
  init_edit_state: Statics.edit_state,
};
