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

let matches = (mks, evt: Js.t(Dom_html.keyboardEvent), is_mac) => {
  let req_matches = (req, mk) => {
    let mod_matches = ModKey.matches(mk, evt, is_mac);
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

let mod_prefix = (mk, is_mac) => {
  let ctrl_text =
    switch (is_held(mk.c), is_mac) {
    | (true, true) => "Cmd + "
    | (true, false) => "Ctrl + "
    | (false, _) => ""
    };
  let shift_text = is_held(mk.s) ? "Shift + " : "";
  let alt_text = is_held(mk.a) ? "Alt + " : "";
  ctrl_text ++ alt_text ++ shift_text;
};
