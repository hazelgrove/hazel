module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html /* need to know whether a Mac is being used to determine certain key   combinations, such as Ctrl+Z vs Cmd+Z for undo */;

let is_mac: bool =
  Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
    Js.string("MAC"),
  )
  >= 0;
