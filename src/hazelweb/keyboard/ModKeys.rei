module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

type req =
  | Held
  | NotHeld
  | Any;

/**
 * Modifier key requirements for a key combo
 */
type t = {
  ctrl: req,
  shift: req,
  alt: req,
  meta: req,
};

let not_held: t;
let ctrlOrCmd: t;
let ctrl: t;
let shift: t;
let alt: t;
let no_ctrlOrCmd_alt: t;
let ctrlOrCmd_shift: t;
let ctrlOrCmd_alt: t;

let matches: (t, Js.t(Dom_html.keyboardEvent)) => bool;

/** generates a string representation of a ModKeys for display */
let mod_prefix: t => string;
