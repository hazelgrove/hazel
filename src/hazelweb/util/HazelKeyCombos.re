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
  | Ctrl_Shift_Z
  | Ctrl_Alt_I
  | Ctrl_Alt_K
  | Ctrl_Alt_J
  | Ctrl_Alt_L
  | Meta_Z
  | Meta_Shift_Z;

let get_details =
  fun
  | Pound => KeyCombo.Details.pound
  | Escape => KeyCombo.Details.escape
  | Backspace => KeyCombo.Details.backspace
  | Delete => KeyCombo.Details.delete
  | ShiftTab => KeyCombo.Details.shift_tab
  | Tab => KeyCombo.Details.tab
  | GT => KeyCombo.Details.gt
  | Ampersand => KeyCombo.Details.ampersand
  | VBar => KeyCombo.Details.vbar
  | LeftParen => KeyCombo.Details.left_parens
  | Colon => KeyCombo.Details.colon
  | Equals => KeyCombo.Details.equals
  | Enter => KeyCombo.Details.enter
  | Backslash => KeyCombo.Details.backslash
  | Plus => KeyCombo.Details.plus
  | Minus => KeyCombo.Details.minus
  | Asterisk => KeyCombo.Details.asterisk
  | Slash => KeyCombo.Details.slash
  | LT => KeyCombo.Details.lt
  | Space => KeyCombo.Details.space
  | Comma => KeyCombo.Details.comma
  | LeftBracket => KeyCombo.Details.left_bracket
  | Semicolon => KeyCombo.Details.semicolon
  | Alt_L => KeyCombo.Details.alt_L
  | Alt_R => KeyCombo.Details.alt_R
  | Alt_C => KeyCombo.Details.alt_C
  | Ctrl_Z => KeyCombo.Details.ctrl_z
  | Ctrl_Shift_Z => KeyCombo.Details.ctrl_shift_z
  | Ctrl_Alt_I => KeyCombo.Details.ctrl_alt_i
  | Ctrl_Alt_K => KeyCombo.Details.ctrl_alt_k
  | Ctrl_Alt_J => KeyCombo.Details.ctrl_alt_j
  | Ctrl_Alt_L => KeyCombo.Details.ctrl_alt_l
  | Meta_Z => KeyCombo.Details.meta_z
  | Meta_Shift_Z => KeyCombo.Details.meta_shift_z;

let of_evt = (evt: Js.t(Dom_html.keyboardEvent)): option(t) => {
  let evt_matches = details => KeyCombo.Details.matches(details, evt);
  if (evt_matches(KeyCombo.Details.pound)) {
    Some(Pound);
  } else if (evt_matches(KeyCombo.Details.ctrl_z)) {
    Some(Ctrl_Z);
  } else if (evt_matches(KeyCombo.Details.ctrl_shift_z)) {
    Some(Ctrl_Shift_Z);
  } else if (evt_matches(KeyCombo.Details.meta_z)) {
    Some(Meta_Z);
  } else if (evt_matches(KeyCombo.Details.meta_shift_z)) {
    Some(Meta_Shift_Z);
  } else if (evt_matches(KeyCombo.Details.escape)) {
    Some(Escape);
  } else if (evt_matches(KeyCombo.Details.backspace)) {
    Some(Backspace);
  } else if (evt_matches(KeyCombo.Details.delete)) {
    Some(Delete);
  } else if (evt_matches(KeyCombo.Details.shift_tab)) {
    Some(ShiftTab);
  } else if (evt_matches(KeyCombo.Details.tab)) {
    Some(Tab);
  } else if (evt_matches(KeyCombo.Details.gt)) {
    Some(GT);
  } else if (evt_matches(KeyCombo.Details.ampersand)) {
    Some(Ampersand);
  } else if (evt_matches(KeyCombo.Details.vbar)) {
    Some(VBar);
  } else if (evt_matches(KeyCombo.Details.left_parens)) {
    Some(LeftParen);
  } else if (evt_matches(KeyCombo.Details.colon)) {
    Some(Colon);
  } else if (evt_matches(KeyCombo.Details.equals)) {
    Some(Equals);
  } else if (evt_matches(KeyCombo.Details.enter)) {
    Some(Enter);
  } else if (evt_matches(KeyCombo.Details.backslash)) {
    Some(Backslash);
  } else if (evt_matches(KeyCombo.Details.plus)) {
    Some(Plus);
  } else if (evt_matches(KeyCombo.Details.minus)) {
    Some(Minus);
  } else if (evt_matches(KeyCombo.Details.asterisk)) {
    Some(Asterisk);
  } else if (evt_matches(KeyCombo.Details.slash)) {
    Some(Slash);
  } else if (evt_matches(KeyCombo.Details.lt)) {
    Some(LT);
  } else if (evt_matches(KeyCombo.Details.space)) {
    Some(Space);
  } else if (evt_matches(KeyCombo.Details.comma)) {
    Some(Comma);
  } else if (evt_matches(KeyCombo.Details.left_bracket)) {
    Some(LeftBracket);
  } else if (evt_matches(KeyCombo.Details.semicolon)) {
    Some(Semicolon);
  } else if (evt_matches(KeyCombo.Details.alt_L)) {
    Some(Alt_L);
  } else if (evt_matches(KeyCombo.Details.alt_R)) {
    Some(Alt_R);
  } else if (evt_matches(KeyCombo.Details.alt_C)) {
    Some(Alt_C);
  } else if (evt_matches(KeyCombo.Details.ctrl_alt_i)) {
    Some(Ctrl_Alt_I);
  } else if (evt_matches(KeyCombo.Details.ctrl_alt_k)) {
    Some(Ctrl_Alt_K);
  } else if (evt_matches(KeyCombo.Details.ctrl_alt_j)) {
    Some(Ctrl_Alt_J);
  } else if (evt_matches(KeyCombo.Details.ctrl_alt_l)) {
    Some(Ctrl_Alt_L);
  } else {
    None;
  };
};
