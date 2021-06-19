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
let comma: t;
let vbar: t;
let ampersand: t;
let alt_L: t;
let alt_R: t;
let alt_C: t;
let alt_PageUp: t;
let alt_PageDown: t;
let ctrl_z: t;
let ctrl_a: t;
let ctrl_space: t;
let ctrl_shift_z: t;
let ctrl_alt_i: t;
let ctrl_alt_k: t;
let ctrl_alt_j: t;
let ctrl_alt_l: t;
let meta_z: t;
let meta_shift_z: t;
