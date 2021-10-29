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
  ctrl: req,
  shift: req,
  alt: req,
  meta: req,
};

// Uses Ctrl if on a non-Mac and Cmd if on a Mac
let ignore_cmd_vs_ctrl = (ctrlOrCmd, shift, alt) =>
  if (IsMac.is_mac) {
    {ctrl: NotHeld, meta: ctrlOrCmd, shift, alt};
  } else {
    {ctrl: ctrlOrCmd, meta: NotHeld, shift, alt};
  };

let not_held = ignore_cmd_vs_ctrl(NotHeld, NotHeld, NotHeld);
let ctrlOrCmd = ignore_cmd_vs_ctrl(Held, NotHeld, NotHeld);
let ctrl = {ctrl: Held, meta: NotHeld, shift: NotHeld, alt: NotHeld};
let shift = ignore_cmd_vs_ctrl(NotHeld, Held, NotHeld);
let alt = ignore_cmd_vs_ctrl(NotHeld, Any, Held);
let no_ctrlOrCmd_alt = ignore_cmd_vs_ctrl(NotHeld, Any, NotHeld);
let ctrlOrCmd_shift = ignore_cmd_vs_ctrl(Held, Held, NotHeld);
let ctrlOrCmd_alt = ignore_cmd_vs_ctrl(Held, NotHeld, Held);

let matches = (mks, evt: Js.t(Dom_html.keyboardEvent)) => {
  let req_matches = (req, mk) => {
    let mod_matches = ModKey.matches(mk, evt);
    switch (req) {
    | Any => true
    | Held => mod_matches
    | NotHeld => !mod_matches
    };
  };
  //TODO(andrew): merge fixes?
  req_matches(mks.ctrl, ModKey.Ctrl)
  && req_matches(mks.shift, ModKey.Shift)
  && req_matches(mks.alt, ModKey.Alt)
  && req_matches(mks.meta, ModKey.Meta);
};

let mod_prefix = mk => {
  let conditional_text = (req, name) => is_held(req) ? name ++ " " : "";

  if (IsMac.is_mac) {
    let ctrl_text = conditional_text(mk.ctrl, Unicode.ctrl);
    let option_text = conditional_text(mk.alt, Unicode.option);
    let shift_text = conditional_text(mk.shift, Unicode.shift);
    let command_text = conditional_text(mk.meta, Unicode.command);
    ctrl_text ++ option_text ++ shift_text ++ command_text;
  } else {
    let ctrl_text = conditional_text(mk.ctrl, "Ctrl +");
    let shift_text = conditional_text(mk.shift, "Shift +");
    let alt_text = conditional_text(mk.alt, "Alt +");
    ctrl_text ++ alt_text ++ shift_text;
  };
};
