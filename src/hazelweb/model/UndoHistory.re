type cursor_term = CursorInfo.cursor_term;
type delete_edit =
  | Term(cursor_term)
  | Space
  | EmptyLine
  | TypeAnn;
type construct_edit =
  | Space
  | EmptyLine
  | LetBinding
  | CaseMatch
  | TypeAnn
  | ShapeEdit(Action.shape);
type edit_action =
  | EditVar
  | DeleteEdit(delete_edit)
  | ConstructEdit(construct_edit)
  | CursorMove;
type cursor_term_info = {
  previous_cursor_term: cursor_term,
  current_cursor_term: cursor_term,
  prev_is_empty_line: bool,
  next_is_empty_line: bool,
};
type info = {
  cursor_term_info,
  previous_action: Action.t,
  edit_action,
};
type undo_history_entry = {
  cardstacks: Cardstacks.t,
  info: option(info),
  not_movement_agnostic: bool,
};

type undo_history_group = {
  group_entries: ZList.t(undo_history_entry, undo_history_entry),
  is_expanded: bool,
  /* [is_complete: bool] if any cursor-moving action interupts the current edit,
     the current group becomes complete.
     Next action will start a new group */
  is_complete: bool,
  prev_is_complete: bool,
};

type t = ZList.t(undo_history_group, undo_history_group);

type entry_base = (cursor_term_info, Action.t, Cardstacks.t);

let edit_action_is_DeleteEmptyLine = (edit_action: edit_action): bool => {
  switch (edit_action) {
  | DeleteEdit(edit_detail) =>
    switch (edit_detail) {
    | EmptyLine => true
    | Term(_)
    | Space
    | TypeAnn => false
    }
  | EditVar
  | ConstructEdit(_)
  | CursorMove => false
  };
};

let edit_action_is_DeleteSpace = (edit_action: edit_action): bool => {
  switch (edit_action) {
  | DeleteEdit(edit_detail) =>
    switch (edit_detail) {
    | Space => true
    | Term(_)
    | EmptyLine
    | TypeAnn => false
    }
  | EditVar
  | ConstructEdit(_)
  | CursorMove => false
  };
};

let edit_action_is_ConstructSpace = (edit_action: edit_action): bool => {
  switch (edit_action) {
  | ConstructEdit(edit_detail) =>
    switch (edit_detail) {
    | Space => true
    | EmptyLine
    | LetBinding
    | CaseMatch
    | TypeAnn
    | ShapeEdit(_) => false
    }
  | EditVar
  | DeleteEdit(_)
  | CursorMove => false
  };
};
let action_is_Sline = (action: Action.t): bool => {
  switch (action) {
  | MoveTo(_)
  | MoveToBefore(_)
  | MoveLeft
  | MoveRight
  | MoveToNextHole
  | MoveToPrevHole
  | UpdateApPalette(_)
  | Delete
  | Backspace => false
  | Construct(shape) =>
    switch (shape) {
    | SList
    | SParenthesized
    | SAsc
    | SLam
    | SListNil
    | SInj(_)
    | SLet
    | SCase
    | SChar(_)
    | SOp(_) => false
    | SApPalette(_) =>
      failwith("ApPalette is not implemented in undo_history")
    | SLine => true
    }
  };
};
let get_cursor_info = (cardstacks: Cardstacks.t): (cursor_term, bool, bool) => {
  let zexp =
    ZList.prj_z(ZList.prj_z(cardstacks).zcards).program |> Program.get_zexp;
  CursorInfo.extract_cursor_term(zexp);
};

let undoable_action = (action: Action.t): bool => {
  switch (action) {
  | UpdateApPalette(_) =>
    failwith("ApPalette is not implemented in undo_history")
  | Delete
  | Backspace
  | Construct(_) => true
  | MoveTo(_)
  | MoveToBefore(_)
  | MoveLeft
  | MoveRight
  | MoveToNextHole
  | MoveToPrevHole => false
  };
};

let get_cursor_pos = (cursor_term: cursor_term): CursorPosition.t => {
  switch (cursor_term) {
  | Exp(cursor_pos, _)
  | Pat(cursor_pos, _)
  | Typ(cursor_pos, _)
  | ExpOp(cursor_pos, _)
  | PatOp(cursor_pos, _)
  | TypOp(cursor_pos, _)
  | Line(cursor_pos, _)
  | Rule(cursor_pos, _) => cursor_pos
  };
};
let has_typ_ann = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | Exp(_, exp) =>
    switch (exp) {
    | EmptyHole(_)
    | Var(_, _, _)
    | NumLit(_, _)
    | BoolLit(_, _)
    | ListNil(_)
    | Inj(_, _, _)
    | Case(_, _, _, _)
    | Parenthesized(_) => false
    | Lam(_, _, _, _) => true
    | ApPalette(_, _, _, _) => failwith("ApPalette is not implemented")
    }
  | Pat(_, _)
  | Typ(_, _)
  | ExpOp(_, _)
  | PatOp(_, _)
  | TypOp(_, _) => false
  | Line(_, line_content) =>
    switch (line_content) {
    | EmptyLine
    | ExpLine(_) => false
    | LetLine(_, _, _) => true
    }
  | Rule(_, _) => false
  };
};

let push_history_entry =
    (
      prev_group: undo_history_group,
      new_entry: undo_history_entry,
      is_complete,
    )
    : undo_history_group => {
  {
    group_entries: (
      [],
      new_entry,
      [
        ZList.prj_z(prev_group.group_entries),
        ...ZList.prj_suffix(prev_group.group_entries),
      ],
    ),
    is_expanded: false,
    is_complete,
    prev_is_complete: is_complete,
  };
};

/* let cursor_jump =
       (prev_group: undo_history_group, cur_cardstacks: Cardstacks.t): bool => {
     let prev_step =
       ZList.prj_z(prev_group.group_entries).cardstacks
       |> Cardstacks.get_program
       |> Program.get_steps;
     let new_step = cur_cardstacks |> Cardstacks.get_program |> Program.get_steps;
     prev_step != new_step;
   }; */
let rec first_not_cursor_move =
        (ls: list(undo_history_entry)): option(undo_history_entry) => {
  switch (ls) {
  | [] => None
  | [head, ...tail] =>
    if (head.not_movement_agnostic) {
      first_not_cursor_move(tail);
    } else {
      Some(head);
    }
  };
};

let cursor_jump =
    (prev_group: undo_history_group, cur_cardstacks: Cardstacks.t): bool => {
  let prev_none_move_entry =
    first_not_cursor_move([
      ZList.prj_z(prev_group.group_entries),
      ...ZList.prj_suffix(prev_group.group_entries),
    ]);
  switch (prev_none_move_entry) {
  | None => true
  | Some(entry') =>
    let prev_step =
      entry'.cardstacks |> Cardstacks.get_program |> Program.get_steps;
    let new_step =
      cur_cardstacks |> Cardstacks.get_program |> Program.get_steps;
    prev_step != new_step;
  };
};
type group_result =
  | Success(undo_history_group)
  | Fail(undo_history_group, undo_history_entry, bool);

let set_fail_join =
    (
      prev_group: undo_history_group,
      entry_base,
      new_edit_action: option(edit_action),
      is_complete_entry: bool,
    )
    : group_result => {
  let (cursor_term_info, action, cardstacks) = entry_base;

  let prev_group' = {...prev_group, is_complete: true};
  let new_entry =
    switch (new_edit_action) {
    | None => {cardstacks, info: None, not_movement_agnostic: false}
    | Some(edit_action) => {
        cardstacks,
        info: Some({cursor_term_info, previous_action: action, edit_action}),
        not_movement_agnostic: false,
      }
    };

  Fail(prev_group', new_entry, is_complete_entry);
};

let set_success_join =
    (
      prev_group: undo_history_group,
      entry_base,
      new_edit_action: option(edit_action),
      is_complete: bool,
    )
    : group_result => {
  let (cursor_term_info, action, cardstacks) = entry_base;

  let new_entry =
    switch (new_edit_action) {
    | None => {cardstacks, info: None, not_movement_agnostic: false}
    | Some(edit_action) => {
        cardstacks,
        info: Some({cursor_term_info, previous_action: action, edit_action}),
        not_movement_agnostic: false,
      }
    };

  Success(push_history_entry(prev_group, new_entry, is_complete));
};

let construct_space =
    (prev_group: undo_history_group, new_entry_base): group_result => {
  //let prev_entry = ZList.prj_z(prev_group.group_entries);
  switch (
    first_not_cursor_move([
      ZList.prj_z(prev_group.group_entries),
      ...ZList.prj_suffix(prev_group.group_entries),
    ])
  ) {
  | None =>
    JSUtil.log("294");
    set_fail_join(
      prev_group,
      new_entry_base,
      Some(ConstructEdit(Space)),
      false,
    );
  | Some(prev_entry) =>
    switch (prev_entry.info) {
    | None =>
      JSUtil.log("304");
      set_fail_join(
        prev_group,
        new_entry_base,
        Some(ConstructEdit(Space)),
        false,
      );
    | Some(info) =>
      if (edit_action_is_ConstructSpace(info.edit_action)) {
        set_success_join(
          prev_group,
          new_entry_base,
          Some(ConstructEdit(Space)),
          false,
        );
      } else {
        JSUtil.log("320");
        set_fail_join(
          prev_group,
          new_entry_base,
          Some(ConstructEdit(Space)),
          false,
        );
      }
    }
  // set_fail_join(prev_group, entry_base, Some(ConstructEdit(Space)), false);
  };
};
type comp_len_typ =
  | MaxLen
  | Ignore
  | Len(int);
/* < */
let comp_len_lt =
    (cursor_len_1: comp_len_typ, cursor_len_2: comp_len_typ): bool => {
  switch (cursor_len_1, cursor_len_2) {
  | (_, MaxLen) => true
  | (_, Ignore) => false
  | (MaxLen, _) => false
  | (Ignore, _) => true
  | (Len(len1), Len(len2)) => len1 < len2
  };
};
let cursor_term_len = (cursor_term: cursor_term): comp_len_typ => {
  switch (cursor_term) {
  | Exp(_, operand) =>
    switch (operand) {
    | EmptyHole(_) => Ignore
    | Var(_, _, var) => Len(Var.length(var))
    | NumLit(_, num) => Len(String.length(string_of_int(num)))
    | BoolLit(_, _)
    | ListNil(_)
    | Lam(_, _, _, _)
    | Inj(_, _, _)
    | Case(_, _, _, _)
    | Parenthesized(_) => MaxLen
    | ApPalette(_, _, _, _) => failwith("ApPalette not implemented")
    }
  | Pat(_, operand) =>
    switch (operand) {
    | EmptyHole(_) => Ignore
    | Wild(_) => Len(1)
    | Var(_, _, var) => Len(Var.length(var))
    | NumLit(_, num) => Len(String.length(string_of_int(num)))
    | BoolLit(_, _)
    | ListNil(_)
    | Parenthesized(_)
    | Inj(_, _, _) => MaxLen
    }
  | Typ(_, _)
  | ExpOp(_, _)
  | PatOp(_, _)
  | TypOp(_, _)
  | Line(_, _)
  | Rule(_, _) => MaxLen
  };
};
let get_initial_term_before_delete =
    (group: undo_history_group, new_cursor_term_info: cursor_term_info)
    : cursor_term => {
  let suffix = ZList.prj_suffix(group.group_entries);
  JSUtil.log("suffix:" ++ string_of_int(List.length(suffix)));
  let rec max_len_term =
          (
            ls: list(undo_history_entry),
            max_len: comp_len_typ,
            cursor_term: cursor_term,
          )
          : cursor_term => {
    switch (ls) {
    | [] => cursor_term
    | [elt] =>
      switch (elt.info) {
      | None => cursor_term
      | Some(info) =>
        let len_cur =
          cursor_term_len(info.cursor_term_info.current_cursor_term);
        let len_prev =
          cursor_term_len(info.cursor_term_info.previous_cursor_term);
        let len_temp =
          if (comp_len_lt(len_cur, len_prev)) {
            len_prev;
          } else {
            len_cur;
          };
        let cursor_temp =
          if (comp_len_lt(len_cur, len_prev)) {
            info.cursor_term_info.previous_cursor_term;
          } else {
            info.cursor_term_info.current_cursor_term;
          };
        if (comp_len_lt(max_len, len_temp)) {
          cursor_temp;
        } else {
          cursor_term;
        };
      }
    | [head, ...tail] =>
      switch (head.info) {
      | None => max_len_term(tail, max_len, cursor_term)
      | Some(info) =>
        let len_temp =
          cursor_term_len(info.cursor_term_info.current_cursor_term);
        if (comp_len_lt(max_len, len_temp)) {
          max_len_term(
            tail,
            len_temp,
            info.cursor_term_info.current_cursor_term,
          );
        } else {
          max_len_term(tail, max_len, cursor_term);
        };
      }
    };
  };
  max_len_term(
    suffix,
    cursor_term_len(new_cursor_term_info.previous_cursor_term),
    new_cursor_term_info.previous_cursor_term,
  );
};

let cursor_jump_after_delete =
    (cursor_pos1: CursorPosition.t, cursor_pos2: CursorPosition.t): bool => {
  switch (cursor_pos1) {
  | OnText(pos1) =>
    if (pos1 != 0) {
      switch (cursor_pos2) {
      | OnText(pos2) => pos2 == 0
      | OnDelim(_, side) =>
        switch (side) {
        | Before => true
        | After => failwith("impossible jump")
        }
      | OnOp(side) =>
        switch (side) {
        | Before => true
        | After => failwith("impossible jump")
        }
      };
    } else {
      false;
    }
  | OnDelim(_, side) =>
    switch (side) {
    | Before => false
    | After => true
    }
  | OnOp(side) =>
    switch (side) {
    | Before => false
    | After => true
    }
  };
};
let cursor_jump_after_backspace =
    (cursor_pos1: CursorPosition.t, cursor_pos2: CursorPosition.t): bool => {
  switch (cursor_pos1) {
  | OnText(pos1) =>
    if (pos1 == 0) {
      switch (cursor_pos2) {
      | OnText(_) => true
      | OnDelim(_, side) =>
        switch (side) {
        | Before => failwith("impossible jump")
        | After => true
        }
      | OnOp(side) =>
        switch (side) {
        | Before => failwith("impossible jump")
        | After => true
        }
      };
    } else {
      false;
    }
  | OnDelim(_, side) =>
    switch (side) {
    | Before => true
    | After => false
    }
  | OnOp(side) =>
    switch (side) {
    | Before => true
    | After => false
    }
  };
};
let group_edit_action =
    (
      prev_group: undo_history_group,
      prev_cardstacks: Cardstacks.t,
      prev_action: Action.t,
    )
    : bool => {
  let is_edit_action =
    switch (prev_action) {
    | Delete
    | Backspace => true
    | Construct(shape) =>
      switch (shape) {
      | SChar(_) => true
      | SList
      | SParenthesized
      | SAsc
      | SLam
      | SListNil
      | SInj(_)
      | SLet
      | SLine
      | SCase
      | SOp(_)
      | SApPalette(_) => false
      }
    | MoveTo(_)
    | MoveToBefore(_)
    | MoveLeft
    | MoveRight
    | MoveToNextHole
    | MoveToPrevHole => true
    | UpdateApPalette(_) => failwith("ApPalette is not implemented")
    };
  is_edit_action && !cursor_jump(prev_group, prev_cardstacks);
};

let ontext_del =
    (
      ~prev_group: undo_history_group,
      /*       ~new_entry: undo_history_entry,
               ~new_entry_info: info, */
      ~new_entry_base: entry_base,
      ~adjacent_is_empty_line: bool,
      ~prev_entry_info: option(info)=?,
      (),
    )
    : group_result => {
  let (cursor_term_info, new_action, _) = new_entry_base;

  let prev_cursor_pos = get_cursor_pos(cursor_term_info.previous_cursor_term);
  let new_cursor_pos = get_cursor_pos(cursor_term_info.current_cursor_term);

  if (CursorInfo.is_empty_line(cursor_term_info.previous_cursor_term)
      || adjacent_is_empty_line
      && cursor_term_info.previous_cursor_term
      == cursor_term_info.current_cursor_term) {
    /* whether delete the previous empty line */
    switch (prev_entry_info) {
    | None =>
      set_fail_join(
        prev_group,
        new_entry_base,
        Some(DeleteEdit(EmptyLine)),
        false,
      )
    | Some(info) =>
      if (edit_action_is_DeleteEmptyLine(info.edit_action)) {
        set_success_join(
          prev_group,
          new_entry_base,
          Some(DeleteEdit(EmptyLine)),
          false,
        );
      } else {
        set_fail_join(
          prev_group,
          new_entry_base,
          Some(DeleteEdit(EmptyLine)),
          false,
        );
      }
    };
  } else if (new_action == Backspace
             && cursor_jump_after_backspace(prev_cursor_pos, new_cursor_pos)) {
    /* jump to next term */
    set_fail_join(
      prev_group,
      new_entry_base,
      None,
      true,
    );
  } else if (new_action == Delete
             && cursor_jump_after_delete(prev_cursor_pos, new_cursor_pos)) {
    /* jump to next term */
    set_fail_join(
      prev_group,
      new_entry_base,
      None,
      true,
    );
  } else if
    /* normal edit */
    (CursorInfo.is_empty_line(cursor_term_info.current_cursor_term)
     || CursorInfo.is_hole(cursor_term_info.current_cursor_term)) {
    switch (prev_entry_info) {
    | None =>
      JSUtil.log("fail combine 499");
      set_fail_join(
        prev_group,
        new_entry_base,
        Some(DeleteEdit(Term(cursor_term_info.previous_cursor_term))),
        true,
      );
    | Some(_) =>
      let initial_term =
        get_initial_term_before_delete(prev_group, cursor_term_info);
      set_success_join(
        prev_group,
        new_entry_base,
        Some(DeleteEdit(Term(initial_term))),
        true,
      );
    };
  } else {
    switch (prev_entry_info) {
    | None =>
      JSUtil.log("520");
      set_fail_join(prev_group, new_entry_base, Some(EditVar), false);
    | Some(_) =>
      JSUtil.log("523");
      set_success_join(prev_group, new_entry_base, Some(EditVar), false);
    };
  };
};
let ondelim_undel =
    (
      ~prev_group: undo_history_group,
      /*       ~new_entry: undo_history_entry,
               ~new_entry_info: info, */
      ~new_entry_base: entry_base,
      ~adjacent_is_empty_line: bool,
      ~prev_entry_info: option(info)=?,
      (),
    )
    : group_result =>
  if (adjacent_is_empty_line) {
    /* whether delete the previous empty line */
    switch (prev_entry_info) {
    | None =>
      set_fail_join(
        prev_group,
        new_entry_base,
        Some(DeleteEdit(EmptyLine)),
        false,
      )
    | Some(info) =>
      if (edit_action_is_DeleteEmptyLine(info.edit_action)) {
        set_success_join(
          prev_group,
          new_entry_base,
          Some(DeleteEdit(EmptyLine)),
          false,
        );
      } else {
        set_fail_join(
          prev_group,
          new_entry_base,
          Some(DeleteEdit(EmptyLine)),
          false,
        );
      }
    };
  } else {
    let (cursor_term_info, _, _) = new_entry_base;
    if (CursorInfo.is_hole(cursor_term_info.previous_cursor_term)) {
      if (CursorInfo.is_exp_inside(cursor_term_info.current_cursor_term)) {
        set_fail_join(prev_group, new_entry_base, None, true);
      } else {
        switch (prev_entry_info) {
        | None =>
          set_fail_join(
            prev_group,
            new_entry_base,
            Some(DeleteEdit(Space)),
            false,
          )
        | Some(info) =>
          if (edit_action_is_DeleteSpace(info.edit_action)) {
            set_success_join(
              prev_group,
              new_entry_base,
              Some(DeleteEdit(Space)),
              false,
            );
          } else {
            set_fail_join(
              prev_group,
              new_entry_base,
              Some(DeleteEdit(Space)),
              false,
            );
          }
        };
      };
    } else {
      /* move cursor to next term, just ignore this move */
      set_fail_join(
        prev_group,
        new_entry_base,
        None,
        true,
      );
    };
  };

let ondelim_del =
    (
      ~prev_group: undo_history_group,
      ~new_entry_base: entry_base,
      ~pos: DelimIndex.t,
      ~prev_entry_info: option(info)=?,
      (),
    )
    : group_result => {
  let (cursor_term_info, _, _) = new_entry_base;

  if (CursorInfo.is_hole(cursor_term_info.previous_cursor_term)) {
    /* move cursor in the hole */
    set_fail_join(
      prev_group,
      new_entry_base,
      None,
      true,
    );
  } else if (pos == 1 && has_typ_ann(cursor_term_info.previous_cursor_term)) {
    /* num==1 is the position of ':' in an expression */
    set_fail_join(
      prev_group,
      new_entry_base,
      Some(DeleteEdit(TypeAnn)),
      true,
    );
  } else {
    /* delete and reach a hole */
    switch (prev_entry_info) {
    | None =>
      JSUtil.log("fail combine 617");
      set_fail_join(
        prev_group,
        new_entry_base,
        Some(DeleteEdit(Term(cursor_term_info.previous_cursor_term))),
        true,
      );
    | Some(_) =>
      let initial_term =
        get_initial_term_before_delete(prev_group, cursor_term_info);
      set_success_join(
        prev_group,
        new_entry_base,
        Some(DeleteEdit(Term(initial_term))),
        true,
      );
    };
  };
};
let is_construct_space = (edit_action): bool => {
  switch (edit_action) {
  | ConstructEdit(construct_edit) =>
    switch (construct_edit) {
    | Space => true
    | EmptyLine
    | LetBinding
    | CaseMatch
    | TypeAnn
    | ShapeEdit(_) => false
    }
  | DeleteEdit(_)
  | EditVar
  | CursorMove => false
  };
};
let construct_shape =
    (
      ~shape: Action.shape,
      ~prev_group: undo_history_group,
      ~new_entry_base: entry_base,
      ~append_info: option((info, Cardstacks.t))=?,
      (),
    )
    : group_result => {
  let insert_shape_helper =
      (
        ~shape: Action.shape,
        ~prev_group: undo_history_group,
        ~new_entry_base: entry_base,
        ~append_info: option((info, Cardstacks.t)),
      )
      : group_result => {
    switch (append_info) {
    | None =>
      set_fail_join(
        prev_group,
        new_entry_base,
        Some(ConstructEdit(ShapeEdit(shape))),
        true,
      )

    | Some(append_info') =>
      let (prev_info, _) = append_info';
      if (is_construct_space(prev_info.edit_action)) {
        set_fail_join(
          prev_group,
          new_entry_base,
          Some(ConstructEdit(ShapeEdit(shape))),
          true,
        );
      } else {
        set_success_join(
          prev_group,
          new_entry_base,
          Some(ConstructEdit(ShapeEdit(shape))),
          true,
        );
      };
    };
  };
  let (new_cursor_term_info, _, _) = new_entry_base;
  switch (shape) {
  | SLine =>
    switch (append_info) {
    | None =>
      set_fail_join(
        prev_group,
        new_entry_base,
        Some(ConstructEdit(EmptyLine)),
        false,
      )
    | Some(append_info') =>
      let (prev_info, _) = append_info';
      if (action_is_Sline(prev_info.previous_action)) {
        set_success_join(
          prev_group,
          new_entry_base,
          Some(ConstructEdit(EmptyLine)),
          false,
        );
      } else {
        set_fail_join(
          prev_group,
          new_entry_base,
          Some(ConstructEdit(EmptyLine)),
          false,
        );
      };
    }

  | SParenthesized
  | SList
  | SAsc
  | SLam
  | SListNil
  | SInj(_)
  | SLet
  | SCase =>
    /* if previous is hole then combine else start a new group */
    insert_shape_helper(~shape, ~prev_group, ~new_entry_base, ~append_info)
  | SChar(_) =>
    /* if previous is hole then combine else if previous is char then combine else start a new group */

    switch (append_info) {
    | None => set_fail_join(prev_group, new_entry_base, Some(EditVar), false)

    | Some(append_info') =>
      let (prev_info, prev_cardstacks) = append_info';
      if (is_construct_space(prev_info.edit_action)) {
        if (group_edit_action(
              prev_group,
              prev_cardstacks,
              prev_info.previous_action,
            )) {
          set_success_join(prev_group, new_entry_base, Some(EditVar), false);
        } else {
          set_fail_join(prev_group, new_entry_base, Some(EditVar), false);
        };
      } else {
        set_success_join(prev_group, new_entry_base, Some(EditVar), false);
      };
    }

  | SOp(shape') =>
    switch (shape') {
    | SMinus
    | SPlus
    | STimes
    | SLessThan
    | SGreaterThan
    | SEquals
    | SComma
    | SArrow
    | SVBar
    | SCons
    | SAnd
    | SOr =>
      /* if previous is hole then combine else start a new group */
      insert_shape_helper(~shape, ~prev_group, ~new_entry_base, ~append_info)
    | SSpace =>
      let prev_last_entry = ZList.prj_z(prev_group.group_entries);
      switch (new_cursor_term_info.previous_cursor_term) {
      | Exp(_, uexp_operand) =>
        switch (uexp_operand) {
        | Var(_, InVarHole(Keyword(k), _), _) =>
          switch (k) {
          | Let =>
            switch (get_cursor_pos(new_cursor_term_info.previous_cursor_term)) {
            | OnText(pos) =>
              if (pos == 3) {
                switch (append_info) {
                | None =>
                  set_fail_join(
                    prev_group,
                    new_entry_base,
                    Some(ConstructEdit(LetBinding)),
                    true,
                  )
                | Some(_) =>
                  let prev_group' = {
                    ...prev_group,
                    group_entries:
                      ZList.replace_z(
                        {
                          ...prev_last_entry,
                          info: None,
                          not_movement_agnostic: false,
                        },
                        prev_group.group_entries,
                      ),
                  };
                  set_success_join(
                    prev_group',
                    new_entry_base,
                    Some(ConstructEdit(LetBinding)),
                    true,
                  );
                };
              } else {
                construct_space(prev_group, new_entry_base);
              }
            | OnDelim(_, _)
            | OnOp(_) => construct_space(prev_group, new_entry_base)
            }

          | Case =>
            switch (get_cursor_pos(new_cursor_term_info.previous_cursor_term)) {
            | OnText(pos) =>
              if (pos == 4) {
                switch (append_info) {
                | None =>
                  set_fail_join(
                    prev_group,
                    new_entry_base,
                    Some(ConstructEdit(CaseMatch)),
                    true,
                  )
                | Some(_) =>
                  let prev_group' = {
                    ...prev_group,
                    group_entries:
                      ZList.replace_z(
                        {
                          ...prev_last_entry,
                          info: None,
                          not_movement_agnostic: false,
                        },
                        prev_group.group_entries,
                      ),
                  };
                  set_success_join(
                    prev_group',
                    new_entry_base,
                    Some(ConstructEdit(CaseMatch)),
                    true,
                  );
                };
              } else {
                construct_space(prev_group, new_entry_base);
              }
            | OnDelim(_, _)
            | OnOp(_) => construct_space(prev_group, new_entry_base)
            }
          }
        | EmptyHole(_)
        | Var(_, _, _)
        | NumLit(_, _)
        | BoolLit(_, _)
        | ListNil(_)
        | Lam(_, _, _, _)
        | Inj(_, _, _)
        | Case(_, _, _, _)
        | Parenthesized(_) => construct_space(prev_group, new_entry_base)
        | ApPalette(_, _, _, _) => failwith("ApPalette is not implemented")
        }
      | Pat(_, _)
      | Typ(_, _)
      | ExpOp(_, _)
      | PatOp(_, _)
      | TypOp(_, _)
      | Line(_, _)
      | Rule(_, _) => construct_space(prev_group, new_entry_base)
      };
    }

  | SApPalette(_) => failwith("ApPalette is not implemented")
  };
};
let entry_to_start_a_group =
    (prev_group: undo_history_group, new_entry_base: entry_base): group_result => {
  let (new_cursor_term_info, action, _) = new_entry_base;

  let prev_cursor_pos =
    get_cursor_pos(new_cursor_term_info.previous_cursor_term);

  let op_del =
      (~prev_group: undo_history_group, ~new_entry_base: entry_base)
      : group_result => {
    let (new_cursor_term_info, _, _) = new_entry_base;
    /* delete and reach a hole */
    JSUtil.log("fail combine 897");
    set_fail_join(
      prev_group,
      new_entry_base,
      Some(DeleteEdit(Term(new_cursor_term_info.previous_cursor_term))),
      true,
    );
  };
  switch (action) {
  | Delete =>
    switch (prev_cursor_pos) {
    | OnText(_) =>
      ontext_del(
        ~prev_group,
        ~new_entry_base,
        ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
        (),
      )
    | OnDelim(pos, side) =>
      switch (side) {
      | Before => ondelim_del(~prev_group, ~new_entry_base, ~pos, ())
      | After =>
        ondelim_undel(
          ~prev_group,
          ~new_entry_base,
          ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
          (),
        )
      }
    | OnOp(side) =>
      switch (side) {
      | Before => op_del(~prev_group, ~new_entry_base)
      | After =>
        /* move cursor to next term, just ignore this move */
        set_fail_join(prev_group, new_entry_base, None, true)
      }
    }
  | Backspace =>
    switch (prev_cursor_pos) {
    | OnText(_) =>
      ontext_del(
        ~prev_group,
        ~new_entry_base,
        ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
        (),
      )
    | OnDelim(pos, side) =>
      switch (side) {
      | Before =>
        ondelim_undel(
          ~prev_group,
          ~new_entry_base,
          ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
          (),
        )

      | After => ondelim_del(~prev_group, ~new_entry_base, ~pos, ())
      }
    | OnOp(side) =>
      switch (side) {
      | Before =>
        /* move cursor to next term, just ignore this move */
        set_fail_join(prev_group, new_entry_base, None, true)
      | After => op_del(~prev_group, ~new_entry_base)
      }
    }
  | Construct(shape) =>
    construct_shape(~shape, ~prev_group, ~new_entry_base, ())
  | MoveTo(_)
  | MoveToBefore(_)
  | MoveLeft
  | MoveRight
  | MoveToNextHole
  | MoveToPrevHole =>
    failwith("Impossible, those actions will not show in undo history")
  | UpdateApPalette(_) => failwith("ApPalette is not implemented")
  };
};

let join_group =
    (
      prev_group: undo_history_group,
      new_entry_base: entry_base,
      prev_cardstacks: Cardstacks.t,
    )
    : group_result => {
  let prev_last_entry = ZList.prj_z(prev_group.group_entries);
  let prev_complete = prev_group.is_complete;
  let (new_cursor_term_info, action, _) = new_entry_base;
  switch (prev_last_entry.info, action) {
  | (None, _) => entry_to_start_a_group(prev_group, new_entry_base)
  | (Some(prev_entry_info), action') =>
    switch (prev_entry_info.previous_action, action') {
    | (prev_ac, Delete) =>
      if (!group_edit_action(prev_group, prev_cardstacks, prev_ac)
          || prev_complete) {
        entry_to_start_a_group(prev_group, new_entry_base);
      } else {
        let prev_cursor_pos =
          get_cursor_pos(
            prev_entry_info.cursor_term_info.current_cursor_term,
          );
        switch (prev_cursor_pos) {
        | OnText(_) =>
          ontext_del(
            ~prev_group,
            ~new_entry_base,
            ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
            ~prev_entry_info,
            (),
          )
        | OnDelim(pos, side) =>
          switch (side) {
          | Before =>
            ondelim_del(
              ~prev_group,
              ~new_entry_base,
              ~pos,
              ~prev_entry_info,
              (),
            )
          | After =>
            ondelim_undel(
              ~prev_group,
              ~new_entry_base,
              ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
              ~prev_entry_info,
              (),
            )
          }
        | OnOp(side) =>
          switch (side) {
          | Before =>
            /* delete and reach a hole */
            let initial_term =
              get_initial_term_before_delete(
                prev_group,
                new_cursor_term_info,
              );
            set_success_join(
              prev_group,
              new_entry_base,
              Some(DeleteEdit(Term(initial_term))),
              true,
            );
          | After =>
            /* move cursor to next term, just ignore this move */
            set_fail_join(prev_group, new_entry_base, None, true)
          }
        };
      }
    | (prev_ac, Backspace) =>
      if (!group_edit_action(prev_group, prev_cardstacks, prev_ac)
          || prev_complete) {
        if (prev_complete) {
          JSUtil.log("comp 1045");
        } else {
          JSUtil.log(" not comp 1045");
        };
        entry_to_start_a_group(prev_group, new_entry_base);
      } else {
        JSUtil.log("group");
        let prev_cursor_pos =
          get_cursor_pos(
            prev_entry_info.cursor_term_info.current_cursor_term,
          );
        switch (prev_cursor_pos) {
        | OnText(_) =>
          ontext_del(
            ~prev_group,
            ~new_entry_base,
            ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
            ~prev_entry_info,
            (),
          )

        | OnDelim(pos, side) =>
          switch (side) {
          | Before =>
            ondelim_undel(
              ~prev_group,
              ~new_entry_base,
              ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
              ~prev_entry_info,
              (),
            )

          | After =>
            ondelim_del(
              ~prev_group,
              ~new_entry_base,
              ~pos,
              ~prev_entry_info,
              (),
            )
          }
        | OnOp(side) =>
          switch (side) {
          | Before =>
            /* move cursor to next term, just ignore this move */
            set_fail_join(prev_group, new_entry_base, None, true)
          | After =>
            let initial_term =
              get_initial_term_before_delete(
                prev_group,
                new_cursor_term_info,
              );
            set_success_join(
              prev_group,
              new_entry_base,
              Some(DeleteEdit(Term(initial_term))),
              true,
            );
          }
        };
      }
    | (_, Construct(shape)) =>
      if (prev_complete) {
        entry_to_start_a_group(prev_group, new_entry_base);
      } else {
        construct_shape(
          ~shape,
          ~prev_group,
          ~new_entry_base,
          ~append_info=(prev_entry_info, prev_cardstacks),
          (),
        );
      }
    | (_, UpdateApPalette(_)) =>
      failwith("ApPalette is not implemented in undo_history")
    | (_, MoveTo(_))
    | (_, MoveToBefore(_))
    | (_, MoveLeft)
    | (_, MoveRight)
    | (_, MoveToNextHole)
    | (_, MoveToPrevHole) =>
      failwith("Impossible match. Not undoable actions will not be pushed")
    }
  };
};

let update_move_action = (undo_history: t, new_entry_base: entry_base): t => {
  let prev_group = ZList.prj_z(undo_history);
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  let (cursor_term_info, action, cardstacks) = new_entry_base;
  let new_entry_info = {
    cursor_term_info,
    previous_action: action,
    edit_action: CursorMove,
  };
  if (cursor_jump(prev_group, cardstacks)) {
    JSUtil.log("jump~");
  } else {
    JSUtil.log("not jump~");
  };
  let new_group =
    if (prev_entry.not_movement_agnostic) {
      JSUtil.log("update new move~");
      {
        ...prev_group,
        group_entries:
          ZList.replace_z(
            {
              cardstacks,
              info: Some(new_entry_info),
              not_movement_agnostic: true,
            },
            prev_group.group_entries,
          ),
        is_complete:
          cursor_jump(prev_group, cardstacks) || prev_group.prev_is_complete,
      };
    } else {
      JSUtil.log("new move~");
      {
        ...prev_group,
        group_entries: (
          ZList.prj_prefix(prev_group.group_entries),
          {
            cardstacks,
            info: Some(new_entry_info),
            not_movement_agnostic: true,
          },
          [
            ZList.prj_z(prev_group.group_entries),
            ...ZList.prj_suffix(prev_group.group_entries),
          ],
        ),
        is_complete:
          cursor_jump(prev_group, cardstacks) || prev_group.is_complete,
        prev_is_complete: prev_group.is_complete,
      };
    };
  ZList.replace_z(new_group, undo_history);
};
let push_edit_state =
    (
      undo_history: t,
      prev_cardstacks: Cardstacks.t,
      cur_cardstacks: Cardstacks.t,
      action: Action.t,
    )
    : t => {
  JSUtil.log("push!");
  let prev_group = ZList.prj_z(undo_history);
  let (prev_cursor_term, prev_is_empty_line, next_is_empty_line) =
    get_cursor_info(prev_cardstacks);
  let (cur_cursor_term, _, _) = get_cursor_info(cur_cardstacks);
  let cursor_term_info = {
    previous_cursor_term: prev_cursor_term,
    current_cursor_term: cur_cursor_term,
    prev_is_empty_line,
    next_is_empty_line,
  };
  if (undoable_action(action)) {
    switch (
      join_group(
        prev_group,
        (cursor_term_info, action, cur_cardstacks),
        prev_cardstacks,
      )
    ) {
    | Success(new_group) => ([], new_group, ZList.prj_suffix(undo_history))
    | Fail(prev_group', new_entry', is_complete_entry) =>
      let new_group = {
        group_entries: ([], new_entry', []),
        is_expanded: false,
        is_complete: is_complete_entry,
        prev_is_complete: is_complete_entry,
      };
      ([], new_group, [prev_group', ...ZList.prj_suffix(undo_history)]);
    };
  } else {
    update_move_action(
      undo_history,
      (cursor_term_info, action, cur_cardstacks),
    );
  };
};

let set_all_hidden_history = (undo_history: t, expanded: bool): t => {
  let hidden_group = (group: undo_history_group) => {
    ...group,
    is_expanded: expanded,
  };
  (
    List.map(hidden_group, ZList.prj_prefix(undo_history)),
    hidden_group(ZList.prj_z(undo_history)),
    List.map(hidden_group, ZList.prj_suffix(undo_history)),
  );
};
