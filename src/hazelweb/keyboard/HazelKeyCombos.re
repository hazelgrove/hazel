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
  | Tilde
  | Semicolon
  | Alt_L
  | Alt_R
  | Alt_C
  | Pound
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
  | Backslash => KeyCombo.backslash
  | Plus => KeyCombo.plus
  | Minus => KeyCombo.minus
  | Asterisk => KeyCombo.asterisk
  | Slash => KeyCombo.slash
  | LT => KeyCombo.lt
  | Space => KeyCombo.space
  | Comma => KeyCombo.comma
  | LeftBracket => KeyCombo.left_bracket
  | Tilde => KeyCombo.tilde
  | Semicolon => KeyCombo.semicolon
  | Alt_L => KeyCombo.alt_L
  | Alt_R => KeyCombo.alt_R
  | Alt_C => KeyCombo.alt_C
  | Ctrl_S => KeyCombo.ctrl_s
  | CtrlOrCmd_Z => KeyCombo.ctrlOrCmd_z
  | CtrlOrCmd_Shift_Z => KeyCombo.ctrlOrCmd_shift_z
  | Up => KeyCombo.up
  | Down => KeyCombo.down
  | Left => KeyCombo.left
  | Right => KeyCombo.right
  | Home => KeyCombo.home_key
  | End => KeyCombo.end_key
  | Alt_Up => KeyCombo.alt_up
  | Alt_Down => KeyCombo.alt_down
  | Alt_Left => KeyCombo.alt_left
  | Alt_Right => KeyCombo.alt_right;

let of_evt = (evt: Js.t(Dom_html.keyboardEvent)): option(t) => {
  let evt_matches = details => KeyCombo.matches(details, evt);
  if (evt_matches(KeyCombo.pound)) {
    Some(Pound);
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
  } else if (evt_matches(KeyCombo.tilde)) {
    Some(Tilde);
  } else if (evt_matches(KeyCombo.semicolon)) {
    Some(Semicolon);
  } else if (evt_matches(KeyCombo.alt_L)) {
    Some(Alt_L);
  } else if (evt_matches(KeyCombo.alt_R)) {
    Some(Alt_R);
  } else if (evt_matches(KeyCombo.alt_C)) {
    Some(Alt_C);
  } else if (evt_matches(KeyCombo.ctrl_s)) {
    Some(Ctrl_S);
  } else if (evt_matches(KeyCombo.ctrlOrCmd_z)) {
    Some(CtrlOrCmd_Z);
  } else if (evt_matches(KeyCombo.ctrlOrCmd_shift_z)) {
    Some(CtrlOrCmd_Shift_Z);
  } else if (evt_matches(KeyCombo.up)) {
    Some(Up);
  } else if (evt_matches(KeyCombo.down)) {
    Some(Down);
  } else if (evt_matches(KeyCombo.left)) {
    Some(Left);
  } else if (evt_matches(KeyCombo.right)) {
    Some(Right);
  } else if (evt_matches(KeyCombo.home_key)) {
    Some(Home);
  } else if (evt_matches(KeyCombo.end_key)) {
    Some(End);
  } else if (evt_matches(KeyCombo.alt_up)) {
    Some(Alt_Up);
  } else if (evt_matches(KeyCombo.alt_down)) {
    Some(Alt_Down);
  } else if (evt_matches(KeyCombo.alt_left)) {
    Some(Alt_Left);
  } else if (evt_matches(KeyCombo.alt_right)) {
    Some(Alt_Right);
  } else {
    None;
  };
};

let name = (combo: t): string => KeyCombo.name(get_details(combo));
