type rev_path = list(int);
type rev_paths = Hashtbl.t(string, list(int));
let mk_rev_paths = () => Hashtbl.create(256);
type t = {
  e: UHExp.t,
  pp_view: Tyxml_js.Html5.elt([ Html_types.div]),
  pp_view_dom: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.divElement),
  rev_paths,
};
type mk_editor_box = (rev_path, rev_paths, UHExp.t) => t;
module NatMap = GeneralUtil.NatMap;
