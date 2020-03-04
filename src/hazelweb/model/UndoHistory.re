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
  | Ignore;
type cursor_term_info = {
  previous_cursor_term: cursor_term,
  current_cursor_term: cursor_term,
  prev_is_empty_line: bool,
  next_is_empty_line: bool,
};

type undo_history_entry = {
  cardstacks: Cardstacks.t,
  cursor_term_info,
  previous_action: Action.t,
  edit_action,
};

type undo_history_group = {
  group_entries: ZList.t(undo_history_entry, undo_history_entry),
  is_expanded: bool,
};

type t = ZList.t(undo_history_group, undo_history_group);

type entry_base = (cursor_term_info, Action.t, Cardstacks.t);

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
    (prev_group: undo_history_group, new_entry: undo_history_entry)
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
  };
};

let rec first_not_cursor_move =
        (ls: list(undo_history_entry)): option(undo_history_entry) => {
  switch (ls) {
  | [] => None
  | [head, ...tail] =>
    if (head.edit_action == Ignore) {
      first_not_cursor_move(tail);
    } else {
      Some(head);
    }
  };
};

let cursor_jump =
    (prev_group: undo_history_group, cardstacks_before: Cardstacks.t): bool => {
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
      cardstacks_before |> Cardstacks.get_program |> Program.get_steps;
    prev_step != new_step;
  };
};

type group_result =
  | Success(undo_history_group)
  | Fail(undo_history_group, undo_history_entry);

let set_fail_join =
    (prev_group: undo_history_group, entry_base, new_edit_action: edit_action)
    : group_result => {
  let (cursor_term_info, action, cardstacks) = entry_base;

  let new_entry = {
    cardstacks,
    cursor_term_info,
    previous_action: action,
    edit_action: new_edit_action,
  };
  Fail(prev_group, new_entry);
};

let set_success_join =
    (prev_group: undo_history_group, entry_base, new_edit_action: edit_action)
    : group_result => {
  let (cursor_term_info, action, cardstacks) = entry_base;
  let new_entry = {
    cardstacks,
    cursor_term_info,
    previous_action: action,
    edit_action: new_edit_action,
  };
  Success(push_history_entry(prev_group, new_entry));
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
  | Rule(_, _) => MaxLen
  | Line(_, _) => Ignore
  };
};
let get_original_term =
    (group: undo_history_group, new_cursor_term_info: cursor_term_info)
    : cursor_term => {
  let suffix = ZList.prj_suffix(group.group_entries);
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
      if (elt.edit_action == Ignore) {
        cursor_term;
      } else {
        let len_cur =
          cursor_term_len(elt.cursor_term_info.current_cursor_term);
        let len_prev =
          cursor_term_len(elt.cursor_term_info.previous_cursor_term);
        let len_temp =
          if (comp_len_lt(len_cur, len_prev)) {
            len_prev;
          } else {
            len_cur;
          };
        let cursor_temp =
          if (comp_len_lt(len_cur, len_prev)) {
            elt.cursor_term_info.previous_cursor_term;
          } else {
            elt.cursor_term_info.current_cursor_term;
          };
        if (comp_len_lt(max_len, len_temp)) {
          cursor_temp;
        } else {
          cursor_term;
        };
      }

    | [head, ...tail] =>
      if (head.edit_action == Ignore) {
        max_len_term(tail, max_len, cursor_term);
      } else {
        let len_temp =
          cursor_term_len(head.cursor_term_info.current_cursor_term);
        if (comp_len_lt(max_len, len_temp)) {
          max_len_term(
            tail,
            len_temp,
            head.cursor_term_info.current_cursor_term,
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
    (edit_action_1: edit_action, edit_action_2: edit_action): bool =>
  switch (edit_action_1, edit_action_2) {
  | (Ignore, _)
  | (_, Ignore)
  | (EditVar, EditVar) => true
  | (EditVar, DeleteEdit(delete_edit)) =>
    switch (delete_edit) {
    | Term(_) => true
    | Space
    | EmptyLine
    | TypeAnn => false
    }
  | (EditVar, ConstructEdit(construct_edit)) =>
    switch (construct_edit) {
    | LetBinding
    | CaseMatch => true
    | Space
    | EmptyLine
    | TypeAnn
    | ShapeEdit(_) => false
    }
  | (DeleteEdit(delete_edit_1), DeleteEdit(delete_edit_2)) =>
    switch (delete_edit_1, delete_edit_2) {
    | (Space, Space)
    | (EmptyLine, EmptyLine) => true
    | (Space, _)
    | (EmptyLine, _)
    | (Term(_), _)
    | (TypeAnn, _) => false
    }
  | (DeleteEdit(_), _) => false
  | (ConstructEdit(construct_edit_1), ConstructEdit(construct_edit_2)) =>
    switch (construct_edit_1, construct_edit_2) {
    | (Space, Space)
    | (EmptyLine, EmptyLine) => true
    | (Space, _)
    | (EmptyLine, _)
    | (LetBinding, _)
    | (CaseMatch, _)
    | (TypeAnn, _)
    | (ShapeEdit(_), _) => false
    }
  | (ConstructEdit(_), _) => false
  };

let group_entry =
    (
      prev_group: undo_history_group,
      prev_cardstacks: Cardstacks.t,
      prev_action: Action.t,
      new_edit_action: edit_action,
    )
    : bool => {
  let can_group_edit_action =
    switch (
      ZList.prj_z(prev_group.group_entries).edit_action,
      new_edit_action,
    ) {
    | (Ignore, _) =>
      switch (
        first_not_cursor_move([
          ZList.prj_z(prev_group.group_entries),
          ...ZList.prj_suffix(prev_group.group_entries),
        ])
      ) {
      | None => true
      | Some(prev_entry) =>
        group_edit_action(prev_entry.edit_action, new_edit_action)
      }
    | (_, _) =>
      group_edit_action(
        ZList.prj_z(prev_group.group_entries).edit_action,
        new_edit_action,
      )
    };
  let can_group_action =
    switch (prev_action) {
    | Delete
    | Backspace => true
    | Construct(shape) =>
      switch (shape) {
      | SLine
      | SChar(_) => true
      | SOp(op) => op == SSpace
      | SList
      | SParenthesized
      | SAsc
      | SLam
      | SListNil
      | SInj(_)
      | SLet
      | SCase
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
  let ignore_cursor_jump =
    switch (
      first_not_cursor_move([
        ZList.prj_z(prev_group.group_entries),
        ...ZList.prj_suffix(prev_group.group_entries),
      ])
    ) {
    | None => true
    | Some(prev_entry) =>
      switch (prev_entry.edit_action, new_edit_action) {
      | (Ignore, _) =>
        failwith(
          "impossible match, Ignore edit action entries have been filtered",
        )
      | (_, Ignore) => true
      | (DeleteEdit(delete_edit_1), DeleteEdit(delete_edit_2)) =>
        switch (delete_edit_1, delete_edit_2) {
        | (Space, Space)
        | (EmptyLine, EmptyLine) => true
        | (Space, _)
        | (EmptyLine, _)
        | (Term(_), _)
        | (TypeAnn, _) => false
        }
      | (ConstructEdit(construct_edit_1), ConstructEdit(construct_edit_2)) =>
        switch (construct_edit_1, construct_edit_2) {
        | (Space, Space)
        | (EmptyLine, EmptyLine) => true
        | (Space, _)
        | (EmptyLine, _)
        | (LetBinding, _)
        | (CaseMatch, _)
        | (TypeAnn, _)
        | (ShapeEdit(_), _) => false
        }
      | (EditVar, _)
      | (DeleteEdit(_), _)
      | (ConstructEdit(_), _) => false
      }
    };
  can_group_edit_action
  && can_group_action
  && (!cursor_jump(prev_group, prev_cardstacks) || ignore_cursor_jump);
};
let construct_space =
    (prev_group: undo_history_group, prev_cardstacks, new_entry_base)
    : group_result =>
  if (group_entry(
        prev_group,
        prev_cardstacks,
        ZList.prj_z(prev_group.group_entries).previous_action,
        ConstructEdit(Space),
      )) {
    set_success_join(prev_group, new_entry_base, ConstructEdit(Space));
  } else {
    set_fail_join(prev_group, new_entry_base, ConstructEdit(Space));
  };
let ontext_del =
    (
      ~prev_group: undo_history_group,
      ~prev_cardstacks: Cardstacks.t,
      ~new_entry_base: entry_base,
      ~adjacent_is_empty_line: bool,
      ~set_new_group: bool,
    )
    : group_result => {
  let prev_last_entry = ZList.prj_z(prev_group.group_entries);
  let (cursor_term_info, new_action, _) = new_entry_base;

  let prev_cursor_pos = get_cursor_pos(cursor_term_info.previous_cursor_term);
  let new_cursor_pos = get_cursor_pos(cursor_term_info.current_cursor_term);

  if (CursorInfo.is_empty_line(cursor_term_info.previous_cursor_term)
      || adjacent_is_empty_line
      && cursor_term_info.previous_cursor_term
      == cursor_term_info.current_cursor_term) {
    /* whether delete the previous empty line */
    if (set_new_group
        || !
             group_entry(
               prev_group,
               prev_cardstacks,
               prev_last_entry.previous_action,
               DeleteEdit(EmptyLine),
             )) {
      if (set_new_group) {
        JSUtil.log("527");
      } else {
        JSUtil.log("528");
      };
      set_fail_join(prev_group, new_entry_base, DeleteEdit(EmptyLine));
    } else {
      JSUtil.log("535");
      set_success_join(prev_group, new_entry_base, DeleteEdit(EmptyLine));
    };
  } else if (new_action == Backspace
             && cursor_jump_after_backspace(prev_cursor_pos, new_cursor_pos)) {
    /* jump to next term */
    set_fail_join(prev_group, new_entry_base, Ignore);
  } else if (new_action == Delete
             && cursor_jump_after_delete(prev_cursor_pos, new_cursor_pos)) {
    /* jump to next term */
    set_fail_join(prev_group, new_entry_base, Ignore);
  } else if (CursorInfo.is_empty_line(cursor_term_info.current_cursor_term)
             || CursorInfo.is_hole(cursor_term_info.current_cursor_term)) {
    if (set_new_group
        || !
             group_entry(
               prev_group,
               prev_cardstacks,
               prev_last_entry.previous_action,
               DeleteEdit(Term(cursor_term_info.previous_cursor_term)),
             )) {
      set_fail_join(
        prev_group,
        new_entry_base,
        DeleteEdit(Term(cursor_term_info.previous_cursor_term)),
      );
    } else {
      let initial_term = get_original_term(prev_group, cursor_term_info);
      set_success_join(
        prev_group,
        new_entry_base,
        DeleteEdit(Term(initial_term)),
      );
    };
  } else if (set_new_group
             || !
                  group_entry(
                    prev_group,
                    prev_cardstacks,
                    prev_last_entry.previous_action,
                    EditVar,
                  )) {
    set_fail_join(prev_group, new_entry_base, EditVar);
  } else {
    set_success_join(prev_group, new_entry_base, EditVar);
  };
};
let ondelim_undel =
    (
      ~prev_group: undo_history_group,
      ~prev_cardstacks: Cardstacks.t,
      ~new_entry_base: entry_base,
      ~adjacent_is_empty_line: bool,
      ~set_new_group: bool,
    )
    : group_result => {
  let prev_last_entry = ZList.prj_z(prev_group.group_entries);
  let (cursor_term_info, _, _) = new_entry_base;

  if (adjacent_is_empty_line
      && cursor_term_info.previous_cursor_term
      == cursor_term_info.current_cursor_term) {
    /* whether delete the previous empty line */
    if (set_new_group
        || !
             group_entry(
               prev_group,
               prev_cardstacks,
               prev_last_entry.previous_action,
               DeleteEdit(EmptyLine),
             )) {
      JSUtil.log("619");
      set_fail_join(prev_group, new_entry_base, DeleteEdit(EmptyLine));
    } else {
      JSUtil.log("627");
      set_success_join(prev_group, new_entry_base, DeleteEdit(EmptyLine));
    };
  } else {
    let (cursor_term_info, _, _) = new_entry_base;
    if (CursorInfo.is_hole(cursor_term_info.previous_cursor_term)) {
      if (CursorInfo.is_exp_inside(cursor_term_info.current_cursor_term)) {
        set_success_join(prev_group, new_entry_base, Ignore);
      } else if (group_entry(
                   prev_group,
                   prev_cardstacks,
                   prev_last_entry.previous_action,
                   DeleteEdit(Space),
                 )) {
        JSUtil.log(
          "679 len" ++ string_of_int(ZList.length(prev_group.group_entries)),
        );
        set_success_join(prev_group, new_entry_base, DeleteEdit(Space));
      } else {
        JSUtil.log("687");
        set_fail_join(prev_group, new_entry_base, DeleteEdit(Space));
      };
    } else {
      /* move cursor to next term, just ignore this move */
      set_success_join(
        prev_group,
        new_entry_base,
        Ignore,
      );
    };
  };
};

let ondelim_del =
    (
      ~prev_group: undo_history_group,
      ~prev_cardstacks: Cardstacks.t,
      ~new_entry_base: entry_base,
      ~pos: DelimIndex.t,
      ~set_new_group: bool,
    )
    : group_result => {
  let (cursor_term_info, _, _) = new_entry_base;
  let prev_last_entry = ZList.prj_z(prev_group.group_entries);
  if (CursorInfo.is_hole(cursor_term_info.previous_cursor_term)) {
    /* move cursor in the hole */
    set_success_join(
      prev_group,
      new_entry_base,
      Ignore,
    );
  } else if (pos == 1 && has_typ_ann(cursor_term_info.previous_cursor_term)) {
    /* num==1 is the position of ':' in an expression */
    set_fail_join(
      prev_group,
      new_entry_base,
      DeleteEdit(TypeAnn),
    );
  } else if (set_new_group
             || !
                  group_entry(
                    prev_group,
                    prev_cardstacks,
                    prev_last_entry.previous_action,
                    DeleteEdit(Term(cursor_term_info.previous_cursor_term)),
                  )) {
    set_fail_join(
      prev_group,
      new_entry_base,
      DeleteEdit(Term(cursor_term_info.previous_cursor_term)),
    );
  } else {
    let initial_term = get_original_term(prev_group, cursor_term_info);
    set_success_join(
      prev_group,
      new_entry_base,
      DeleteEdit(Term(initial_term)),
    );
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
  | Ignore => false
  };
};
let construct_shape =
    (
      ~shape: Action.shape,
      ~prev_group: undo_history_group,
      ~prev_cardstacks: Cardstacks.t,
      ~new_entry_base: entry_base,
      ~set_new_group: bool,
    )
    : group_result => {
  let prev_last_entry = ZList.prj_z(prev_group.group_entries);
  let (new_cursor_term_info, _, _) = new_entry_base;
  switch (shape) {
  | SLine =>
    if (set_new_group
        || !
             group_entry(
               prev_group,
               prev_cardstacks,
               prev_last_entry.previous_action,
               ConstructEdit(EmptyLine),
             )) {
      set_fail_join(prev_group, new_entry_base, ConstructEdit(EmptyLine));
    } else {
      set_success_join(prev_group, new_entry_base, ConstructEdit(EmptyLine));
    }

  | SParenthesized
  | SList
  | SAsc
  | SLam
  | SListNil
  | SInj(_)
  | SLet
  | SCase =>
    if (set_new_group
        || !
             group_entry(
               prev_group,
               prev_cardstacks,
               prev_last_entry.previous_action,
               ConstructEdit(ShapeEdit(shape)),
             )) {
      set_fail_join(
        prev_group,
        new_entry_base,
        ConstructEdit(ShapeEdit(shape)),
      );
    } else {
      set_success_join(
        prev_group,
        new_entry_base,
        ConstructEdit(ShapeEdit(shape)),
      );
    }
  | SChar(_) =>
    /* if previous is hole then combine else if previous is char then combine else start a new group */
    if (set_new_group
        || !
             group_entry(
               prev_group,
               prev_cardstacks,
               prev_last_entry.previous_action,
               EditVar,
             )) {
      set_fail_join(prev_group, new_entry_base, EditVar);
    } else {
      set_success_join(prev_group, new_entry_base, EditVar);
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
      if (set_new_group
          || !
               group_entry(
                 prev_group,
                 prev_cardstacks,
                 prev_last_entry.previous_action,
                 ConstructEdit(ShapeEdit(shape)),
               )) {
        set_fail_join(
          prev_group,
          new_entry_base,
          ConstructEdit(ShapeEdit(shape)),
        );
      } else {
        set_success_join(
          prev_group,
          new_entry_base,
          ConstructEdit(ShapeEdit(shape)),
        );
      }
    | SSpace =>
      switch (new_cursor_term_info.previous_cursor_term) {
      | Exp(_, uexp_operand) =>
        switch (uexp_operand) {
        | Var(_, InVarHole(Keyword(k), _), _) =>
          switch (k) {
          | Let =>
            switch (get_cursor_pos(new_cursor_term_info.previous_cursor_term)) {
            | OnText(pos) =>
              if (pos == 3) {
                if (set_new_group
                    || !
                         group_entry(
                           prev_group,
                           prev_cardstacks,
                           prev_last_entry.previous_action,
                           ConstructEdit(LetBinding),
                         )) {
                  set_fail_join(
                    prev_group,
                    new_entry_base,
                    ConstructEdit(LetBinding),
                  );
                } else {
                  let prev_group' = {
                    ...prev_group,
                    group_entries:
                      ZList.replace_z(
                        {...prev_last_entry, edit_action: Ignore},
                        prev_group.group_entries,
                      ),
                  };
                  set_success_join(
                    prev_group',
                    new_entry_base,
                    ConstructEdit(LetBinding),
                  );
                };
              } else {
                construct_space(prev_group, prev_cardstacks, new_entry_base);
              }
            | OnDelim(_, _)
            | OnOp(_) =>
              construct_space(prev_group, prev_cardstacks, new_entry_base)
            }

          | Case =>
            switch (get_cursor_pos(new_cursor_term_info.previous_cursor_term)) {
            | OnText(pos) =>
              if (pos == 4) {
                if (set_new_group
                    || !
                         group_entry(
                           prev_group,
                           prev_cardstacks,
                           prev_last_entry.previous_action,
                           ConstructEdit(LetBinding),
                         )) {
                  set_fail_join(
                    prev_group,
                    new_entry_base,
                    ConstructEdit(CaseMatch),
                  );
                } else {
                  let prev_group' = {
                    ...prev_group,
                    group_entries:
                      ZList.replace_z(
                        {...prev_last_entry, edit_action: Ignore},
                        prev_group.group_entries,
                      ),
                  };
                  set_success_join(
                    prev_group',
                    new_entry_base,
                    ConstructEdit(CaseMatch),
                  );
                };
              } else {
                construct_space(prev_group, prev_cardstacks, new_entry_base);
              }
            | OnDelim(_, _)
            | OnOp(_) =>
              construct_space(prev_group, prev_cardstacks, new_entry_base)
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
        | Parenthesized(_) =>
          construct_space(prev_group, prev_cardstacks, new_entry_base)
        | ApPalette(_, _, _, _) => failwith("ApPalette is not implemented")
        }
      | Pat(_, _)
      | Typ(_, _)
      | ExpOp(_, _)
      | PatOp(_, _)
      | TypOp(_, _)
      | Line(_, _)
      | Rule(_, _) =>
        construct_space(prev_group, prev_cardstacks, new_entry_base)
      }
    }

  | SApPalette(_) => failwith("ApPalette is not implemented")
  };
};
let entry_to_start_a_group =
    (
      prev_group: undo_history_group,
      prev_cardstacks: Cardstacks.t,
      new_entry_base: entry_base,
    )
    : group_result => {
  let (new_cursor_term_info, action, _) = new_entry_base;

  let prev_cursor_pos =
    get_cursor_pos(new_cursor_term_info.previous_cursor_term);

  switch (action) {
  | Delete =>
    switch (prev_cursor_pos) {
    | OnText(_) =>
      ontext_del(
        ~prev_group,
        ~prev_cardstacks,
        ~new_entry_base,
        ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
        ~set_new_group=true,
      )
    | OnDelim(pos, side) =>
      switch (side) {
      | Before =>
        ondelim_del(
          ~prev_group,
          ~prev_cardstacks,
          ~new_entry_base,
          ~pos,
          ~set_new_group=true,
        )
      | After =>
        ondelim_undel(
          ~prev_group,
          ~prev_cardstacks,
          ~new_entry_base,
          ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
          ~set_new_group=true,
        )
      }
    | OnOp(side) =>
      switch (side) {
      | Before =>
        set_fail_join(
          prev_group,
          new_entry_base,
          DeleteEdit(Term(new_cursor_term_info.previous_cursor_term)),
        )
      | After =>
        /* move cursor to next term, just ignore this move */
        set_success_join(prev_group, new_entry_base, Ignore)
      }
    }
  | Backspace =>
    switch (prev_cursor_pos) {
    | OnText(_) =>
      ontext_del(
        ~prev_group,
        ~prev_cardstacks,
        ~new_entry_base,
        ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
        ~set_new_group=true,
      )
    | OnDelim(pos, side) =>
      switch (side) {
      | Before =>
        ondelim_undel(
          ~prev_group,
          ~prev_cardstacks,
          ~new_entry_base,
          ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
          ~set_new_group=true,
        )

      | After =>
        ondelim_del(
          ~prev_group,
          ~prev_cardstacks,
          ~new_entry_base,
          ~pos,
          ~set_new_group=true,
        )
      }
    | OnOp(side) =>
      switch (side) {
      | Before =>
        /* move cursor to next term, just ignore this move */
        set_success_join(prev_group, new_entry_base, Ignore)
      | After =>
        set_fail_join(
          prev_group,
          new_entry_base,
          DeleteEdit(Term(new_cursor_term_info.previous_cursor_term)),
        )
      }
    }
  | Construct(shape) =>
    construct_shape(
      ~shape,
      ~prev_group,
      ~prev_cardstacks,
      ~new_entry_base,
      ~set_new_group=true,
    )
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
  let (new_cursor_term_info, action, _) = new_entry_base;
  switch (prev_last_entry.previous_action, action) {
  | (prev_ac, Delete) =>
    let prev_cursor_pos =
      get_cursor_pos(prev_last_entry.cursor_term_info.current_cursor_term);
    switch (prev_cursor_pos) {
    | OnText(_) =>
      ontext_del(
        ~prev_group,
        ~prev_cardstacks,
        ~new_entry_base,
        ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
        ~set_new_group=false,
      )
    | OnDelim(pos, side) =>
      switch (side) {
      | Before =>
        ondelim_del(
          ~prev_group,
          ~prev_cardstacks,
          ~new_entry_base,
          ~pos,
          ~set_new_group=false,
        )
      | After =>
        ondelim_undel(
          ~prev_group,
          ~prev_cardstacks,
          ~new_entry_base,
          ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
          ~set_new_group=false,
        )
      }
    | OnOp(side) =>
      switch (side) {
      | Before =>
        /* delete and reach a hole */
        let initial_term =
          get_original_term(prev_group, new_cursor_term_info);
        if (group_entry(
              prev_group,
              prev_cardstacks,
              prev_ac,
              DeleteEdit(Term(initial_term)),
            )) {
          set_success_join(
            prev_group,
            new_entry_base,
            DeleteEdit(Term(initial_term)),
          );
        } else {
          set_fail_join(
            prev_group,
            new_entry_base,
            DeleteEdit(Term(initial_term)),
          );
        };
      | After =>
        /* move cursor to next term, just ignore this move */
        set_success_join(prev_group, new_entry_base, Ignore)
      }
    };

  | (prev_ac, Backspace) =>
    let prev_cursor_pos =
      get_cursor_pos(prev_last_entry.cursor_term_info.current_cursor_term);
    switch (prev_cursor_pos) {
    | OnText(_) =>
      ontext_del(
        ~prev_group,
        ~prev_cardstacks,
        ~new_entry_base,
        ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
        ~set_new_group=false,
      )

    | OnDelim(pos, side) =>
      switch (side) {
      | Before =>
        ondelim_undel(
          ~prev_group,
          ~prev_cardstacks,
          ~new_entry_base,
          ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
          ~set_new_group=false,
        )

      | After =>
        ondelim_del(
          ~prev_group,
          ~prev_cardstacks,
          ~new_entry_base,
          ~pos,
          ~set_new_group=false,
        )
      }
    | OnOp(side) =>
      switch (side) {
      | Before =>
        /* move cursor to next term, just ignore this move */
        set_success_join(prev_group, new_entry_base, Ignore)
      | After =>
        let initial_term =
          get_original_term(prev_group, new_cursor_term_info);
        if (group_entry(
              prev_group,
              prev_cardstacks,
              prev_ac,
              DeleteEdit(Term(initial_term)),
            )) {
          set_success_join(
            prev_group,
            new_entry_base,
            DeleteEdit(Term(initial_term)),
          );
        } else {
          set_fail_join(
            prev_group,
            new_entry_base,
            DeleteEdit(Term(initial_term)),
          );
        };
      }
    };

  | (_, Construct(shape)) =>
    construct_shape(
      ~shape,
      ~prev_group,
      ~new_entry_base,
      ~prev_cardstacks,
      ~set_new_group=false,
    )

  | (_, UpdateApPalette(_)) =>
    failwith("ApPalette is not implemented in undo_history")
  | (_, MoveTo(_))
  | (_, MoveToBefore(_))
  | (_, MoveLeft)
  | (_, MoveRight)
  | (_, MoveToNextHole)
  | (_, MoveToPrevHole) =>
    failwith("Impossible match. Not undoable actions will not be pushed")
  };
};

let update_move_action = (undo_history: t, new_entry_base: entry_base): t => {
  let prev_group = ZList.prj_z(undo_history);
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  let (cursor_term_info, action, cardstacks) = new_entry_base;
  let new_entry_info = {
    cardstacks,
    cursor_term_info,
    previous_action: action,
    edit_action: Ignore,
  };
  let new_group =
    if (prev_entry.edit_action == Ignore) {
      {
        ...prev_group,
        group_entries:
          ZList.replace_z(new_entry_info, prev_group.group_entries),
      };
    } else {
      {
        ...prev_group,
        group_entries: (
          ZList.prj_prefix(prev_group.group_entries),
          new_entry_info,
          [
            ZList.prj_z(prev_group.group_entries),
            ...ZList.prj_suffix(prev_group.group_entries),
          ],
        ),
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
  JSUtil.log("1135");
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
    | Fail(prev_group', new_entry') =>
      let new_group = {
        group_entries: ([], new_entry', []),
        is_expanded: false,
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
