module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

type req =
  | Held
  | NotHeld
  | Any;

let is_held: req => bool;

type t = {
  c: req,
  s: req,
  a: req,
  m: req,
};

let not_held: t;
let ctrl: t;
let shift: t;
let alt: t;
let meta: t;
let no_ctrl_alt_meta: t;
let ctrl_shift: t;
let ctrl_alt: t;
let meta_shift: t;

let req_matches: (req, ModKey.t, Js.t(Dom_html.keyboardEvent)) => bool;

let matches: (t, Js.t(Dom_html.keyboardEvent)) => bool;

let mod_prefix: t => string;
