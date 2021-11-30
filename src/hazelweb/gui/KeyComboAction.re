module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;

let get_model_action_from_kc =
    (cursor_info: CursorInfo.t, key_combo: HazelKeyCombos.t)
    : option(ModelAction.t) => {
  let construct = (shape: Action.shape): option(ModelAction.t) =>
    Some(EditAction(Construct(shape)));

  let (cursor_on_type, cursor_on_comment, cursor_on_exp) =
    switch (cursor_info) {
    | {typed: OnType, _} => (true, false, false)
    | {cursor_term: Line(_, CommentLine(_)), _} => (false, true, false)
    | {cursor_term: ExpOperand(_, operand), _} =>
      switch (operand) {
      | EmptyHole(_) => (false, false, false)
      | _ => (false, false, true)
      }

    | _ => (false, false, false)
    };

  /* When adding or updating key combo actions, make sure to appropriately update
     messages in the strategy guide. */
  switch (key_combo) {
  | Escape => None
  | Backspace => Some(EditAction(Backspace))
  | Delete => Some(EditAction(Delete))
  | ShiftTab => Some(EditAction(MoveToPrevHole))
  | Tab => Some(EditAction(MoveToNextHole))
  | GT when cursor_on_type => construct(SOp(SArrow))
  | GT => construct(SOp(SGreaterThan))
  | Ampersand => construct(SOp(SAnd))
  | VBar when cursor_on_type => construct(SOp(SVBar))
  | VBar => construct(SOp(SOr))
  | LeftParen => construct(SParenthesized)
  | RightParen => construct(SCloseParens)
  | RightBrace => construct(SCloseBraces)
  | RightSquareBracket => construct(SCloseSquareBracket)
  | Colon => construct(SAnn)
  | Equals => construct(SOp(SEquals))
  | Enter => construct(SLine)
  | Shift_Enter => construct(SCommentLine)
  | Backslash => construct(SLam)
  | Plus => construct(SOp(SPlus))
  | Minus => construct(SOp(SMinus))
  | Asterisk => construct(SOp(STimes))
  | Slash => construct(SOp(SDivide))
  | LT => construct(SOp(SLessThan))
  | Space when cursor_on_comment => construct(SChar(" "))
  | Space => construct(SOp(SSpace))
  | Comma => construct(SOp(SComma))
  | LeftBracket when cursor_on_type => construct(SList)
  | LeftBracket when cursor_on_exp => construct(SSubscript)
  | LeftBracket => construct(SListNil)
  | Semicolon => construct(SOp(SCons))
  | Quote => construct(SQuote)
  | Caret => construct(SOp(SCaret))
  | Alt_L => construct(SInj(L))
  | Alt_R => construct(SInj(R))
  | Alt_C => construct(SCase)
  | Pound => construct(SCommentLine)
  | Ctrl_Space => Some(UpdateCursorInspector(Toggle_visible))
  | Ctrl_S => Some(SerializeToConsole(UHExp))
  | Ctrl_Shift_S => Some(SerializeToConsole(ZExp))
  | CtrlOrCmd_Z => Some(Undo)
  | CtrlOrCmd_Shift_Z => Some(Redo)
  | Up => Some(MoveAction(Key(ArrowUp)))
  | Down => Some(MoveAction(Key(ArrowDown)))
  | Left => Some(MoveAction(Key(ArrowLeft)))
  | Right => Some(MoveAction(Key(ArrowRight)))
  | Home => Some(MoveAction(Key(Home)))
  | End => Some(MoveAction(Key(End)))
  | Alt_Up => Some(EditAction(SwapUp))
  | Alt_Down => Some(EditAction(SwapDown))
  | Alt_Left => Some(EditAction(SwapLeft))
  | Alt_Right => Some(EditAction(SwapRight))
  };
};

let get_model_action =
    (cursor_info: CursorInfo.t, evt: Js.t(Dom_html.keyboardEvent))
    : option(ModelAction.t) => {
  let construct = (shape: Action.shape): option(ModelAction.t) =>
    Some(EditAction(Construct(shape)));

  let (_cursor_on_type, cursor_on_comment, cursor_on_stringlit) =
    switch (cursor_info) {
    | {typed: OnType, _} => (true, false, false)
    | {cursor_term: Line(_, CommentLine(_)), _} => (false, true, false)
    | {cursor_term: ExpOperand(OnText(_), StringLit(_, _)), _} => (
        false,
        false,
        true,
      )
    | _ => (false, false, false)
    };

  let key_combo = HazelKeyCombos.of_evt(evt);

  let alpha_regexp = Js_of_ocaml.Regexp.regexp("^[a-zA-Z']$");
  let comment_char_regexp = Js_of_ocaml.Regexp.regexp("^[^#]$");
  let any_regexp = Js_of_ocaml.Regexp.regexp("^.$");
  let single_key = JSUtil.is_single_key(evt, alpha_regexp);
  let single_key_in_comment = JSUtil.is_single_key(evt, comment_char_regexp);
  let single_key_in_stringlit = JSUtil.is_single_key(evt, any_regexp);

  switch (
    key_combo,
    single_key,
    single_key_in_comment,
    single_key_in_stringlit,
  ) {
  | (_, _, Some(single_key_in_comment), _) when cursor_on_comment =>
    construct(SChar(JSUtil.single_key_string(single_key_in_comment)))
  | (_, _, _, Some(single_key_in_stringlit)) when cursor_on_stringlit =>
    construct(SChar(JSUtil.single_key_string(single_key_in_stringlit)))
  | (Some(key_combo), _, _, _) =>
    get_model_action_from_kc(cursor_info, key_combo)
  | (_, Some(single_key), _, _) =>
    construct(SChar(JSUtil.single_key_string(single_key)))
  | (None, None, _, _) => None
  };
};
