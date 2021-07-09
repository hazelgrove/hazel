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
  | Alt_L
  | Alt_R
  | Alt_C
  | Pound
  | Ctrl_S
  | CtrlOrCmd_Z
  | CtrlOrCmd_Shift_Z
  | CtrlOrCmd_Alt_I
  | CtrlOrCmd_Alt_K
  | CtrlOrCmd_Alt_J
  | CtrlOrCmd_Alt_L;

let get_details: t => KeyCombo.t;

let of_evt: Js.t(Dom_html.keyboardEvent) => option(t);

let name: t => string;
