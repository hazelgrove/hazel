module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;


let of_evt: Js.t(Dom_html.keyboardEvent) => option(t);
