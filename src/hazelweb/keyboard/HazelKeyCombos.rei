module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

type single_key =
  | Number(int)
  | Letter(string)
  | Underscore;

[@deriving sexp]
type t =
  | Single(single_key)
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
  | Ctrl_Space
  | Ctrl_S
  | CtrlOrCmd_Z
  | CtrlOrCmd_Shift_Z
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Alt_Up
  | Alt_Down
  | Alt_Left
  | Alt_Right;

let get_details: t => KeyCombo.t;

let of_evt: Js.t(Dom_html.keyboardEvent) => option(t);

let name: t => string;
