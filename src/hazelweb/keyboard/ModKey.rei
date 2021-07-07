module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

/**
 * An individual modifier key identifier
 */
type t =
  | Ctrl
  | Shift
  | Alt;

let matches: (t, Js.t(Dom_html.keyboardEvent)) => bool;
