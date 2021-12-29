module Vdom = Virtual_dom.Vdom;
open Sexplib.Std;

[@deriving sexp]
type t = {
  name: string,
  caption: [@sexp.opaque] Vdom.Node.t,
  init_prelude: Statics.edit_state,
  init_template: Statics.edit_state,
  init_tester: Statics.edit_state,
};
