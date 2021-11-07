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

let definitely_stays_on_operand =
    (action: ModelAction.t, ci: CursorInfo.t): bool => {
  //an altenate approach: reset retained whenever operand
  //isnt invalidtext or freevar
  let ct = ci.cursor_term;
  let on_text = CursorInfo_common.cursor_is_on_text(ct);
  let at_end = CursorInfo_common.cursor_at_end(ct);
  let at_start = CursorInfo_common.cursor_at_start(ct);
  let single_char = CursorInfo_common.cursor_is_on_single_char_text(ct);
  print_endline(string_of_bool(single_char));
  switch (action) {
  | MoveAction(Key(ArrowLeft)) => on_text && !at_end
  | MoveAction(Key(ArrowRight)) => on_text && !at_start
  | EditAction(ea) =>
    switch (ea) {
    | MoveLeft => on_text && !at_end
    | MoveRight => on_text && !at_start
    | Backspace => on_text && !at_end && !(single_char && at_start)
    | Delete => on_text && !at_start && !(single_char && at_end)
    | Construct(shape) =>
      switch (shape) {
      | SChar(_string) => true
      | SListNil //TODO (maybe true?)
      | _ => false
      }
    | _ => false
    }
  | _ => false
  };
};

let get_assistant_action =
    (
      kc: HazelKeyCombos.t,
      ~ci: CursorInfo.t,
      ~assistant_action: option(Action.t),
      ~main_action: ModelAction.t,
    )
    : ModelAction.t => {
  let base_action: ModelAction.t =
    switch (kc, assistant_action) {
    | (Down, Some(_)) => UpdateAssistant(Increment_selection_index)
    | (Up, Some(_)) => UpdateAssistant(Decrement_selection_index)
    | (Enter, Some(ReplaceOperand(of_sort))) =>
      let place_after: Action.replace_operand_of_sort =
        switch (of_sort) {
        | Exp(operand, _) => Exp(operand, Some(ZExp.place_after))
        | Pat(operand, _) => Pat(operand, Some(ZPat.place_after))
        | Typ(operand, _) => Typ(operand, Some(ZTyp.place_after))
        };
      AcceptSuggestion(ReplaceOperand(place_after));
    | (Enter, Some(action)) => AcceptSuggestion(action)
    | (Tab, Some(action)) =>
      Chain([AcceptSuggestion(action), EditAction(MoveToNextHole)])
    | (Shift_Down, _) => get_main_action_default(Down)
    | (Shift_Up, _) => get_main_action_default(Up)
    | _ => main_action /*
      Chain([
        UpdateAssistant(Reset), // reset assistant selection
        main_action,
      ])*/
    };
  let cond =
    definitely_stays_on_operand(main_action, ci) || kc == Down || kc == Up;
  cond
    ? base_action
    : Chain([
        base_action,
        UpdateAssistant(Reset) /*UpdateAssistant(Set_retained_ci(Some(ci)))*/,
      ]);
};

let get_model_action = (model: Model.t, kc: HazelKeyCombos.t): ModelAction.t => {
  let ci = Model.get_cursor_info(model);
  let assistant_focussed =
    model.focal_editor == Model.MainProgram && model.assistant.active;
  let assistant_action = AssistantModel.action(model.assistant, ci);
  let (cursor_on_type, cursor_on_comment) =
    switch (ci) {
    | {typed: OnType(_), _} => (true, false)
    | {cursor_term: Line(_, CommentLine(_)), _} => (false, true)
    | _ => (false, false)
    };
  let main_action = get_main_action(kc, ~cursor_on_type, ~cursor_on_comment);
  let assistant_action =
    get_assistant_action(kc, ~ci, ~assistant_action, ~main_action);
  assistant_focussed ? assistant_action : main_action;
};
