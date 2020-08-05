module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

let enter = plain(Key.the_key("Enter"));
let escape = plain(Key.the_key("Escape"));
let backspace = plain(Key.the_key("Backspace"));
let delete = plain(Key.the_key("Delete"));
let tab = plain(Key.the_key("Tab"));
let shift_tab = shift(Key.the_key("Tab"));
let shift_enter = shift(Key.the_key("Enter")); // For creating another "CommentLine"
let space = plain(Key.key1("Space", " "));
let lt = no_ctrl_alt_meta(Key.the_key("<"));
let gt = no_ctrl_alt_meta(Key.the_key(">"));
let colon = no_ctrl_alt_meta(Key.the_key(":"));
let backslash = no_ctrl_alt_meta(Key.the_key("\\"));
let left_parens = no_ctrl_alt_meta(Key.the_key("("));
let right_parens = no_ctrl_alt_meta(Key.the_key(")"));
let left_bracket = no_ctrl_alt_meta(Key.the_key("["));
let right_bracket = no_ctrl_alt_meta(Key.the_key("]"));
let qmark = no_ctrl_alt_meta(Key.the_key("?"));
let equals = no_ctrl_alt_meta(Key.the_key("="));
let pound = no_ctrl_alt_meta(Key.the_key("#"));
let plus = no_ctrl_alt_meta(Key.the_key("+"));
let minus = no_ctrl_alt_meta(Key.the_key("-"));
let asterisk = no_ctrl_alt_meta(Key.the_key("*"));
let slash = no_ctrl_alt_meta(Key.the_key("/"));
let semicolon = no_ctrl_alt_meta(Key.the_key(";"));
let comma = no_ctrl_alt_meta(Key.the_key(","));
let vbar = no_ctrl_alt_meta(Key.the_key("|"));
let ampersand = no_ctrl_alt_meta(Key.the_key("&"));
let dollar = no_ctrl_alt_meta(Key.the_key("$"));
let amp = no_ctrl_alt_meta(Key.the_key("&"));
let alt_L = alt(Key.the_key("l"));
let alt_R = alt(Key.the_key("r"));
let alt_C = alt(Key.the_key("c"));
let alt_PageUp = alt(Key.the_key("PageUp"));
let alt_PageDown = alt(Key.the_key("PageDown"));
let alt_T = alt(Key.the_key("T"));
let alt_F = alt(Key.the_key("F"));
let ctrl_z = ctrl(Key.the_key("z"));
let ctrl_shift_z = ctrl_shift(Key.the_key("Z"));
let ctrl_alt_i = ctrl_alt(Key.the_key("i"));
let ctrl_alt_k = ctrl_alt(Key.the_key("k"));
let ctrl_alt_j = ctrl_alt(Key.the_key("j"));
let ctrl_alt_l = ctrl_alt(Key.the_key("l"));
let meta_z = ctrl(Key.the_key("z"));
let meta_shift_z = ctrl_shift(Key.the_key("Z"));

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
  | Semicolon => KeyCombo.semicolon
  | Alt_L => KeyCombo.alt_L
  | Alt_R => KeyCombo.alt_R
  | Alt_C => KeyCombo.alt_C
  | Ctrl_Z => KeyCombo.ctrl_z
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
    Some(pound);
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
  } else {
    None;
  };
};
