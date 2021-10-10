module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

type t = {
  mod_keys: ModKeys.t,
  key: Key.t,
};

let matches: (t, Js.t(Dom_html.keyboardEvent)) => bool;
let name: t => string;

let enter: t;
let escape: t;
let backspace: t;
let delete: t;
let tab: t;
let shift_tab: t;
let shift_enter: t;
let space: t;
let lt: t;
let gt: t;
let colon: t;
let backslash: t;
let left_parens: t;
let left_bracket: t;
let equals: t;
let pound: t;
let plus: t;
let minus: t;
let asterisk: t;
let slash: t;
let semicolon: t;
let quote: t;
let comma: t;
let vbar: t;
let ampersand: t;
let alt_L: t;
let alt_R: t;
let alt_C: t;
let alt_PageUp: t;
let alt_PageDown: t;
let ctrl_s: t;
let ctrlOrCmd_z: t;
let ctrlOrCmd_shift_z: t;
let up: t;
let down: t;
let left: t;
let right: t;
let home_key: t;
let end_key: t;
let alt_up: t;
let alt_down: t;
let alt_left: t;
let alt_right: t;
