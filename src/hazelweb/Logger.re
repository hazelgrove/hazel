module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

let array_empty: Js.constr(Js.t(Js.js_array(Js.t(Js.js_string)))) = Js.array_empty;
let action_log = {
  %js
  new array_empty;
};
let action_log_global = "action_log";
Js.Unsafe.set(Dom_html.window, action_log_global, action_log);

let init_log = () => ();

let append = (s: string) => {
  let _ = action_log##push(Js.string(s));
  ();
};
