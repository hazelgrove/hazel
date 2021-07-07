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
};

let not_held = {c: NotHeld, s: NotHeld, a: NotHeld};
let ctrl = {c: Held, s: NotHeld, a: NotHeld};
let shift = {c: NotHeld, s: Held, a: NotHeld};
let alt = {c: NotHeld, s: Any, a: Held};
let no_ctrl_alt = {c: NotHeld, s: Any, a: NotHeld};
let ctrl_shift = {c: Held, s: Held, a: NotHeld};
let ctrl_alt = {c: Held, s: NotHeld, a: Held};

let matches = (mks, evt: Js.t(Dom_html.keyboardEvent)) => {
  let req_matches = (req, mk) => {
    let mod_matches = ModKey.matches(mk, evt);
    switch (req) {
    | Any => true
    | Held => mod_matches
    | NotHeld => !mod_matches
    };
  };

  req_matches(mks.c, ModKey.Ctrl)
  && req_matches(mks.s, ModKey.Shift)
  && req_matches(mks.a, ModKey.Alt);
};

let mod_prefix = mk => {
  let conditional_text = (req, name) => is_held(req) ? name ++ " " : "";

  if (IsMac.is_mac) {
    let option_text = conditional_text(mk.a, Unicode.option);
    let shift_text = conditional_text(mk.s, Unicode.shift);
    let command_text = conditional_text(mk.c, Unicode.command);
    option_text ++ shift_text ++ command_text;
  } else {
    let ctrl_text = conditional_text(mk.c, "Ctrl +");
    let shift_text = conditional_text(mk.s, "Shift +");
    let alt_text = conditional_text(mk.a, "Alt +");
    ctrl_text ++ alt_text ++ shift_text;
  };
};
