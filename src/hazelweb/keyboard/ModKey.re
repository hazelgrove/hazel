module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

type t =
  | Ctrl
  | Shift
  | Alt;

let matches = (mk, evt: Js.t(Dom_html.keyboardEvent)) =>
  switch (mk) {
  | Ctrl =>
    IsMac.is_mac ? Js.to_bool(evt##.metaKey) : Js.to_bool(evt##.ctrlKey)
  | Shift => Js.to_bool(evt##.shiftKey)
  | Alt => Js.to_bool(evt##.altKey)
  };
