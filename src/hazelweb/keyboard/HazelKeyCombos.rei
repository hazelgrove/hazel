module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

[@deriving sexp]
type t =
  | Escape
  | Backspace
  | Delete
  | ShiftTab
  | Tab
  | GT
  | Ampersand
  | VBar
  | LeftParen
  | Colon
  | Equals
  | Enter
  | Shift_Enter
  | Backslash
  | Plus
  | Minus
  | Asterisk
  | Slash
  | LT
  | Space
  | Comma
  | LeftBracket
  | Semicolon
  | Alt_I
  | Alt_C
  | Pound
  | Ctrl_Z
  | Ctrl_Shift_Z
  | Ctrl_Alt_I
  | Ctrl_Alt_K
  | Ctrl_Alt_J
  | Ctrl_Alt_L
  | Meta_Z
  | Meta_Shift_Z;

let get_details: t => KeyCombo.t;

let of_evt: Js.t(Dom_html.keyboardEvent) => option(t);
