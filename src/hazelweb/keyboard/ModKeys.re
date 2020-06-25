module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

type req =
  | Held
  | NotHeld
  | Any;

let is_held =
  fun
  | Held => true
  | NotHeld
  | Any => false;

type t = {
  c: req,
  s: req,
  a: req,
  m: req,
};

let not_held = {c: NotHeld, s: NotHeld, a: NotHeld, m: NotHeld};
let ctrl = {c: Held, s: NotHeld, a: NotHeld, m: NotHeld};
let shift = {c: NotHeld, s: Held, a: NotHeld, m: NotHeld};
let alt = {c: NotHeld, s: Any, a: Held, m: NotHeld};
let meta = {c: NotHeld, s: NotHeld, a: NotHeld, m: Held};
let no_ctrl_alt_meta = {c: NotHeld, s: Any, a: NotHeld, m: NotHeld};
let ctrl_shift = {c: Held, s: Held, a: NotHeld, m: NotHeld};
let ctrl_alt = {c: Held, s: NotHeld, a: Held, m: NotHeld};
let meta_shift = {c: NotHeld, s: Held, a: NotHeld, m: Held};

let req_matches = (req, mk, evt) =>
  switch (req) {
  | Any => true
  | Held => ModKey.matches(mk, evt)
  | NotHeld => !ModKey.matches(mk, evt)
  };

let matches = (mks, evt: Js.t(Dom_html.keyboardEvent)) =>
  req_matches(mks.c, ModKey.Ctrl, evt)
  && req_matches(mks.s, ModKey.Shift, evt)
  && req_matches(mks.a, ModKey.Alt, evt)
  && req_matches(mks.m, ModKey.Meta, evt);

let mod_prefix = mk => {
  let ctrl_text = is_held(mk.c) ? "Ctrl + " : "";
  let shift_text = is_held(mk.s) ? "Shift + " : "";
  let alt_text = is_held(mk.a) ? "Alt + " : "";
  let meta_text = is_held(mk.m) ? "Meta + " : "";
  meta_text ++ ctrl_text ++ alt_text ++ shift_text;
};
