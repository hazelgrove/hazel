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
  | Ctrl_Enter
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
  | Ctrl_Z
  | Ctrl_Space
  | Ctrl_Shift_Z
  | Ctrl_Alt_I
  | Ctrl_Alt_K
  | Ctrl_Alt_J
  | Ctrl_Alt_L
  | Meta_Z
  | Meta_Shift_Z;

let get_details =
  fun
  | Pound => KeyCombo.pound
  | Escape => KeyCombo.escape
  | Backspace => KeyCombo.backspace
  | Delete => KeyCombo.delete
  | ShiftTab => KeyCombo.shift_tab
  | Tab => KeyCombo.tab
  | GT => KeyCombo.gt
  | Ampersand => KeyCombo.ampersand
  | VBar => KeyCombo.vbar
  | LeftParen => KeyCombo.left_parens
  | Colon => KeyCombo.colon
  | Equals => KeyCombo.equals
  | Enter => KeyCombo.enter
  | Shift_Enter => KeyCombo.shift_enter
  | Ctrl_Enter => KeyCombo.ctrl_enter
  | Backslash => KeyCombo.backslash
  | Plus => KeyCombo.plus
  | Minus => KeyCombo.minus
  | Asterisk => KeyCombo.asterisk
  | Slash => KeyCombo.slash
  | LT => KeyCombo.lt
  | Space => KeyCombo.space
  | Comma => KeyCombo.comma
  | LeftBracket => KeyCombo.left_bracket
  | Semicolon => KeyCombo.semicolon
  | Alt_L => KeyCombo.alt_L
  | Alt_R => KeyCombo.alt_R
  | Alt_C => KeyCombo.alt_C
  | Ctrl_Z => KeyCombo.ctrl_z
  | Ctrl_Space => KeyCombo.ctrl_space
  | Ctrl_Shift_Z => KeyCombo.ctrl_shift_z
  | Ctrl_Alt_I => KeyCombo.ctrl_alt_i
  | Ctrl_Alt_K => KeyCombo.ctrl_alt_k
  | Ctrl_Alt_J => KeyCombo.ctrl_alt_j
  | Ctrl_Alt_L => KeyCombo.ctrl_alt_l
  | Meta_Z => KeyCombo.meta_z
  | Meta_Shift_Z => KeyCombo.meta_shift_z;

let of_evt = (evt: Js.t(Dom_html.keyboardEvent)): option(t) => {
  let evt_matches = details => KeyCombo.matches(details, evt);
  if (evt_matches(KeyCombo.pound)) {
    Some(Pound);
  } else if (evt_matches(KeyCombo.ctrl_z)) {
    Some(Ctrl_Z);
  } else if (evt_matches(KeyCombo.ctrl_shift_z)) {
    Some(Ctrl_Shift_Z);
  } else if (evt_matches(KeyCombo.meta_z)) {
    Some(Meta_Z);
  } else if (evt_matches(KeyCombo.meta_shift_z)) {
    Some(Meta_Shift_Z);
  } else if (evt_matches(KeyCombo.escape)) {
    Some(Escape);
  } else if (evt_matches(KeyCombo.backspace)) {
    Some(Backspace);
  } else if (evt_matches(KeyCombo.delete)) {
    Some(Delete);
  } else if (evt_matches(KeyCombo.shift_tab)) {
    Some(ShiftTab);
  } else if (evt_matches(KeyCombo.tab)) {
    Some(Tab);
  } else if (evt_matches(KeyCombo.gt)) {
    Some(GT);
  } else if (evt_matches(KeyCombo.ampersand)) {
    Some(Ampersand);
  } else if (evt_matches(KeyCombo.vbar)) {
    Some(VBar);
  } else if (evt_matches(KeyCombo.left_parens)) {
    Some(LeftParen);
  } else if (evt_matches(KeyCombo.colon)) {
    Some(Colon);
  } else if (evt_matches(KeyCombo.equals)) {
    Some(Equals);
  } else if (evt_matches(KeyCombo.enter)) {
    Some(Enter);
  } else if (evt_matches(KeyCombo.shift_enter)) {
    Some(Shift_Enter);
  } else if (evt_matches(KeyCombo.ctrl_enter)) {
    Some(Ctrl_Enter);
  } else if (evt_matches(KeyCombo.backslash)) {
    Some(Backslash);
  } else if (evt_matches(KeyCombo.plus)) {
    Some(Plus);
  } else if (evt_matches(KeyCombo.minus)) {
    Some(Minus);
  } else if (evt_matches(KeyCombo.asterisk)) {
    Some(Asterisk);
  } else if (evt_matches(KeyCombo.slash)) {
    Some(Slash);
  } else if (evt_matches(KeyCombo.lt)) {
    Some(LT);
  } else if (evt_matches(KeyCombo.space)) {
    Some(Space);
  } else if (evt_matches(KeyCombo.comma)) {
    Some(Comma);
  } else if (evt_matches(KeyCombo.left_bracket)) {
    Some(LeftBracket);
  } else if (evt_matches(KeyCombo.semicolon)) {
    Some(Semicolon);
  } else if (evt_matches(KeyCombo.alt_L)) {
    Some(Alt_L);
  } else if (evt_matches(KeyCombo.alt_R)) {
    Some(Alt_R);
  } else if (evt_matches(KeyCombo.alt_C)) {
    Some(Alt_C);
  } else if (evt_matches(KeyCombo.ctrl_alt_i)) {
    Some(Ctrl_Alt_I);
  } else if (evt_matches(KeyCombo.ctrl_alt_k)) {
    Some(Ctrl_Alt_K);
  } else if (evt_matches(KeyCombo.ctrl_alt_j)) {
    Some(Ctrl_Alt_J);
  } else if (evt_matches(KeyCombo.ctrl_alt_l)) {
    Some(Ctrl_Alt_L);
  } else if (evt_matches(KeyCombo.ctrl_space)) {
    Some(Ctrl_Space);
  } else {
    None;
  };
};
