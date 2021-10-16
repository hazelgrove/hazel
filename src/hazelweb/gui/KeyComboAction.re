let construct = (shape: Action.shape): ModelAction.t =>
  EditAction(Construct(shape));

let get_main_action =
    (kc: HazelKeyCombos.t, ~cursor_on_type: bool, ~cursor_on_comment: bool)
    : ModelAction.t =>
  /* When adding or updating key combo actions, make sure to appropriately update
     messages in the strategy guide. */
  switch (kc) {
  | Escape => SetCursorInspectorMode(None)
  | Ctrl_Space => ToggleCursorInspectorMode
  | Ctrl_Delete =>
    EditAction(
      ReplaceOperand(Exp(UHExp.EmptyHole(0), Some(ZExp.place_before))),
    )
  | Ctrl_S => SerializeToConsole(UHExp)
  | CtrlOrCmd_Z => Undo
  | CtrlOrCmd_Shift_Z => Redo
  | Alt_Up => EditAction(SwapUp)
  | Alt_Down => EditAction(SwapDown)
  | Alt_Left => EditAction(SwapLeft)
  | Alt_Right => EditAction(SwapRight)
  | Backspace => EditAction(Backspace)
  | Delete => EditAction(Delete)
  | ShiftTab => EditAction(MoveToPrevHole)
  | Tab => EditAction(MoveToNextHole)
  | Up => MoveAction(Key(ArrowUp))
  | Down => MoveAction(Key(ArrowDown))
  | Left => MoveAction(Key(ArrowLeft))
  | Right => MoveAction(Key(ArrowRight))
  | Home => MoveAction(Key(Home))
  | End => MoveAction(Key(End))
  | Space when cursor_on_comment => construct(SChar(" "))
  | Space => construct(SOp(SSpace))
  | LeftBracket when cursor_on_type => construct(SList)
  | LeftBracket => construct(SListNil)
  | GT when cursor_on_type => construct(SOp(SArrow))
  | GT => construct(SOp(SGreaterThan))
  | VBar when cursor_on_type => construct(SOp(SVBar))
  | VBar => construct(SOp(SOr))
  | Ampersand => construct(SOp(SAnd))
  | Equals => construct(SOp(SEquals))
  | Plus => construct(SOp(SPlus))
  | Minus => construct(SOp(SMinus))
  | Asterisk => construct(SOp(STimes))
  | Slash => construct(SOp(SDivide))
  | LT => construct(SOp(SLessThan))
  | Comma => construct(SOp(SComma))
  | Semicolon => construct(SOp(SCons))
  | LeftParen => construct(SParenthesized)
  | Colon => construct(SAnn)
  | Enter => construct(SLine)
  | Shift_Enter => construct(SCommentLine)
  | Pound => construct(SCommentLine)
  | Backslash => construct(SLam)
  | Alt_L => construct(SInj(L))
  | Alt_R => construct(SInj(R))
  | Alt_C => construct(SCase)
  | Single(_) => construct(SChar(HazelKeyCombos.name(kc)))
  | _ => Chain([])
  };

let get_main_action_default =
  get_main_action(~cursor_on_type=false, ~cursor_on_comment=false);

let get_assistant_action =
    (kc: HazelKeyCombos.t, ~assistant_action: option(Action.t))
    : option(ModelAction.t) =>
  switch (kc, assistant_action) {
  | (Shift_Down, _) => Some(get_main_action_default(Down))
  | (Shift_Up, _) => Some(get_main_action_default(Up))
  | (Down, Some(_)) => Some(UpdateAssistant(Increment_selection_index))
  | (Up, Some(_)) => Some(UpdateAssistant(Decrement_selection_index))
  | (Enter, Some(ReplaceOperand(Exp(operand, _)))) =>
    Some(
      AcceptSuggestion(
        ReplaceOperand(Exp(operand, Some(ZExp.place_after))),
      ),
    )
  | (Enter, Some(ReplaceOperand(Pat(operand, _)))) =>
    Some(
      AcceptSuggestion(
        ReplaceOperand(Pat(operand, Some(ZPat.place_after))),
      ),
    )
  | (Enter, Some(action)) => Some(AcceptSuggestion(action))
  | (Tab, Some(action)) =>
    Some(Chain([AcceptSuggestion(action), EditAction(MoveToNextHole)]))
  | _ => None
  };

let get_model_action = (model: Model.t, kc: HazelKeyCombos.t): ModelAction.t => {
  let cursor_info = Model.get_cursor_info(model);
  let assistant_focussed =
    model.focal_editor == Model.MainProgram && model.assistant.active;
  let assistant_action =
    AssistantModel.get_action(model.assistant, cursor_info);
  let (cursor_on_type, cursor_on_comment) =
    switch (cursor_info) {
    | {typed: OnType, _} => (true, false)
    | {cursor_term: Line(_, CommentLine(_)), _} => (false, true)
    | _ => (false, false)
    };
  switch (get_assistant_action(kc, ~assistant_action)) {
  | Some(action) when assistant_focussed => action
  | _ =>
    Chain([
      UpdateAssistant(Reset), // reset assistant selection
      get_main_action(kc, ~cursor_on_type, ~cursor_on_comment),
    ])
  };
};
