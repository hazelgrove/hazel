module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

type t = {
  mod_keys: ModKeys.t,
  key: Key.t,
};

let mk = (mod_keys, key) => {mod_keys, key};
let plain = key => {mod_keys: ModKeys.not_held, key};
let no_ctrl_alt_meta = key => {mod_keys: ModKeys.no_ctrl_alt_meta, key};
let shift = key => {mod_keys: ModKeys.shift, key};
let ctrl = key => {mod_keys: ModKeys.ctrl, key};
let alt = key => {mod_keys: ModKeys.alt, key};
let meta = key => {mod_keys: ModKeys.meta, key};
let ctrl_shift = key => {mod_keys: ModKeys.ctrl_shift, key};
let ctrl_alt = key => {mod_keys: ModKeys.ctrl_alt, key};
let meta_shift = key => {mod_keys: ModKeys.meta_shift, key};

let matches = (kc, evt: Js.t(Dom_html.keyboardEvent)) =>
  ModKeys.matches(kc.mod_keys, evt) && Key.matches(kc.key, evt);

let name = kc => {
  let mod_prefix = ModKeys.mod_prefix(kc.mod_keys);
  mod_prefix ++ kc.key.plain_name;
};
