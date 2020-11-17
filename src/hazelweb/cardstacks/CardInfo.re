module Vdom = Virtual_dom.Vdom;

[@deriving sexp]
type t = {
  caption: [@sexp.opaque] Vdom.Node.t,
  init_term: Program.EditState.term,
};
