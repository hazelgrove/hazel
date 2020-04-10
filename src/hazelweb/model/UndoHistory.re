type cursor_term = CursorInfo.cursor_term;
type outer_zexp = CursorInfo.outer_zexp;
type delete_edit =
  | Term(cursor_term)
  | Space
  | EmptyLine
  | TypeAnn;

type edit_action =
  | EditVar
  | DeleteEdit(delete_edit)
  | ConstructEdit(Action.shape)
  | Ignore /* cursor move and init state */;

type cursor_term_info = {
  cursor_term_before: cursor_term,
  cursor_term_after: cursor_term,
  prev_is_empty_line: bool,
  next_is_empty_line: bool,
};

type undo_history_entry = {
  cardstacks: Cardstacks.t,
  cursor_term_info,
  previous_action: Action.t,
  edit_action,
  outer_zexp,
};

type undo_history_group = {
  group_entries: ZList.t(undo_history_entry, undo_history_entry),
  is_expanded: bool,
  timestamp: float,
  display_timestamp: bool,
};

type t = {
  groups: ZList.t(undo_history_group, undo_history_group),
  latest_timestamp: float,
  all_hidden_history_expand: bool,
};

type entry_base = (cursor_term_info, Action.t, Cardstacks.t,outer_zexp) /* return value: cursor_term,prev_is_empty_line: bool, next_is_empty_line: bool, */;

let get_cursor_info = (cardstacks: Cardstacks.t): (cursor_term, bool, bool) => {
  let zexp =
    ZList.prj_z(ZList.prj_z(cardstacks).zcards).program |> Program.get_zexp;
  CursorInfo.extract_cursor_term(zexp);
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
    | Lam(_, _, _, _) => true
    | _ => false
    }
  | Line(_, line_content) =>
    switch (line_content) {
    | LetLine(_, _, _) => true
    | _ => false
    }
  | _ => false
  };
};

let push_history_entry =
    (prev_group: undo_history_group, new_entry: undo_history_entry)
    : undo_history_group => {
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  if (new_entry.edit_action == Ignore) {
    /* if edit_action is Ignore, the successory history should not be cleared */
    if (prev_entry.edit_action == Ignore) {
      {
        /* only store 1 cursor-move (Ignore) entry if there are consecutive cursor-move actions */

        ...prev_group,
        group_entries: ZList.replace_z(new_entry, prev_group.group_entries),
      };
    } else {
      {
        ...prev_group,
        group_entries: (
          ZList.prj_prefix(prev_group.group_entries),
          new_entry,
          [
            ZList.prj_z(prev_group.group_entries),
            ...ZList.prj_suffix(prev_group.group_entries),
          ],
        ),
      };
    };
  } else {
    {
      ...prev_group,
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
};

let rec drop_prefix_ignore_entries =
        (ls: list(undo_history_entry)): option(undo_history_entry) => {
  switch (ls) {
  | [] => None
  | [head, ...tail] =>
    if (head.edit_action == Ignore) {
      drop_prefix_ignore_entries(tail);
    } else {
      Some(head);
    }
  };
};

let cursor_jump =
    (prev_group: undo_history_group, cardstacks_before: Cardstacks.t): bool => {
  switch (
    drop_prefix_ignore_entries([
      ZList.prj_z(prev_group.group_entries),
      ...ZList.prj_suffix(prev_group.group_entries),
    ])
  ) {
  | None => true
  | Some(entry') =>
    let prev_step =
      entry'.cardstacks |> Cardstacks.get_program |> Program.get_steps;
    let new_step =
      cardstacks_before |> Cardstacks.get_program |> Program.get_steps;
    prev_step != new_step;
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
    | SLet
    | SCase => true
    | _ => false
    }
  | (DeleteEdit(delete_edit_1), DeleteEdit(delete_edit_2)) =>
    switch (delete_edit_1, delete_edit_2) {
    | (Space, Space)
    | (EmptyLine, EmptyLine) => true
    | (_, _) => false
    }
  | (DeleteEdit(_), _) => false
  | (ConstructEdit(construct_edit_1), ConstructEdit(construct_edit_2)) =>
    switch (construct_edit_1, construct_edit_2) {
    | (SOp(SSpace), SOp(SSpace))
    | (SLine, SLine) => true
    | (_, _) => false
    }
  | (ConstructEdit(_), _) => false
  };
let group_entry =
    (
      prev_group: undo_history_group,
      cardstacks_before: Cardstacks.t,
      new_edit_action: edit_action,
    )
    : bool => {
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  let can_group_edit_action =
    switch (prev_entry.edit_action, new_edit_action) {
    | (Ignore, _) =>
      switch (
        drop_prefix_ignore_entries([
          prev_entry,
          ...ZList.prj_suffix(prev_group.group_entries),
        ])
      ) {
      | None => true
      | Some(prev_entry) =>
        group_edit_action(prev_entry.edit_action, new_edit_action)
      }
    | (_, _) => group_edit_action(prev_entry.edit_action, new_edit_action)
    };

  let ignore_cursor_jump =
    switch (
      drop_prefix_ignore_entries([
        prev_entry,
        ...ZList.prj_suffix(prev_group.group_entries),
      ])
    ) {
    | None => true
    | Some(prev_entry) =>
      switch (prev_entry.edit_action, new_edit_action) {
      | (Ignore, _) =>
        failwith(
          "impossible match, prefix Ignore edit action entries have been filtered",
        )
      | (_, Ignore) => true
      | (DeleteEdit(delete_edit_1), DeleteEdit(delete_edit_2)) =>
        switch (delete_edit_1, delete_edit_2) {
        | (Space, Space)
        | (EmptyLine, EmptyLine) => true
        | _ => false
        }
      | (ConstructEdit(construct_edit_1), ConstructEdit(construct_edit_2)) =>
        switch (construct_edit_1, construct_edit_2) {
        | (SOp(SSpace), SOp(SSpace))
        | (SLine, SLine) => true
        | _ => false
        }
      | _ => false
      }
    };
  can_group_edit_action
  && (!cursor_jump(prev_group, cardstacks_before) || ignore_cursor_jump);
};

type group_result =
  | Success(undo_history_group)
  | Fail(undo_history_group, undo_history_entry);

let build_entry =
    (entry_base, new_edit_action: edit_action): undo_history_entry => {
  let (cursor_term_info, action, cardstacks,outer_zexp) = entry_base;
  {
    cardstacks,
    cursor_term_info,
    previous_action: action,
    edit_action: new_edit_action,
    outer_zexp,
  };
};
let set_fail_join =
    (prev_group: undo_history_group, entry_base, new_edit_action: edit_action)
    : group_result =>
  Fail(prev_group, build_entry(entry_base, new_edit_action));

let set_success_join =
    (prev_group: undo_history_group, entry_base, new_edit_action: edit_action)
    : group_result => {
  Success(
    push_history_entry(prev_group, build_entry(entry_base, new_edit_action)),
  );
};

let set_join_result =
    (
      prev_group: undo_history_group,
      cardstacks_before: Cardstacks.t,
      entry_base,
      edit_action: edit_action,
    )
    : group_result =>
  if (group_entry(prev_group, cardstacks_before, edit_action)) {
    set_success_join(prev_group, entry_base, edit_action);
  } else {
    set_fail_join(prev_group, entry_base, edit_action);
  };

type comp_len_typ =
  | MaxLen
  | Ignore
  | Len(int) /* < */;
let comp_len_lt =
    (cursor_len_1: comp_len_typ, cursor_len_2: comp_len_typ): bool => {
  switch (cursor_len_1, cursor_len_2) {
  | (MaxLen, MaxLen) => false
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
  | Typ(_, operand) =>
    switch (operand) {
    | Hole => Ignore
    | Unit
    | Num
    | Bool
    | Parenthesized(_)
    | List(_) => MaxLen
    }
  | ExpOp(_, _)
  | PatOp(_, _)
  | TypOp(_, _)
  | Rule(_, _) => MaxLen
  | Line(_, line) =>
    switch (line) {
    | EmptyLine => Ignore
    | LetLine(_, _, _)
    | ExpLine(_) => MaxLen
    }
  };
};
let comp_len_larger =
    (cursor_term_1: cursor_term, cursor_term_2: cursor_term): cursor_term =>
  if (comp_len_lt(
        cursor_term_len(cursor_term_1),
        cursor_term_len(cursor_term_2),
      )) {
    cursor_term_2;
  } else {
    cursor_term_1;
  };
let get_original_deleted_term =
    (group: undo_history_group, new_cursor_term_info: cursor_term_info)
    : cursor_term => {
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
        comp_len_larger(
          comp_len_larger(
            elt.cursor_term_info.cursor_term_after,
            elt.cursor_term_info.cursor_term_before,
          ),
          cursor_term,
        );
      }
    | [head, ...tail] =>
      if (head.edit_action == Ignore) {
        max_len_term(tail, max_len, cursor_term);
      } else {
        let larger_term =
          comp_len_larger(
            cursor_term,
            head.cursor_term_info.cursor_term_after,
          );
        max_len_term(tail, cursor_term_len(larger_term), larger_term);
      }
    };
  };
  max_len_term(
    [
      ZList.prj_z(group.group_entries),
      ...ZList.prj_suffix(group.group_entries),
    ],
    cursor_term_len(new_cursor_term_info.cursor_term_before),
    new_cursor_term_info.cursor_term_before,
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

let construct_space =
    (prev_group: undo_history_group, cardstacks_before, new_entry_base)
    : group_result =>
  set_join_result(
    prev_group,
    cardstacks_before,
    new_entry_base,
    ConstructEdit(SOp(SSpace)),
  );

let is_delete_emptylines =
    (adjacent_is_empty_line: bool, cursor_term_info): bool =>
  CursorInfo.is_empty_line(cursor_term_info.cursor_term_before)
  || adjacent_is_empty_line
  && cursor_term_info.cursor_term_before == cursor_term_info.cursor_term_after;

let ontext_delete =
    (
      ~prev_group: undo_history_group,
      ~cardstacks_before: Cardstacks.t,
      ~new_entry_base: entry_base,
      ~adjacent_is_empty_line: bool,
    )
    : group_result => {
  let (new_cursor_term_info, new_action, _) = new_entry_base;

  let prev_cursor_pos =
    get_cursor_pos(new_cursor_term_info.cursor_term_before);
  let new_cursor_pos = get_cursor_pos(new_cursor_term_info.cursor_term_after);

  if (is_delete_emptylines(adjacent_is_empty_line, new_cursor_term_info)) {
    /* delete adjacent empty line */
    set_join_result(
      prev_group,
      cardstacks_before,
      new_entry_base,
      DeleteEdit(EmptyLine),
    );
  } else if (new_action == Backspace
             && cursor_jump_after_backspace(prev_cursor_pos, new_cursor_pos)) {
    /* jump to next term */
    set_success_join(
      prev_group,
      new_entry_base,
      Ignore,
    );
  } else if (new_action == Delete
             && cursor_jump_after_delete(prev_cursor_pos, new_cursor_pos)) {
    /* jump to next term */
    set_success_join(
      prev_group,
      new_entry_base,
      Ignore,
    );
  } else if (CursorInfo.is_empty_line(new_cursor_term_info.cursor_term_after)
             || CursorInfo.is_hole(new_cursor_term_info.cursor_term_after)) {
    /* delete the whole term */
    let initial_term =
      get_original_deleted_term(prev_group, new_cursor_term_info);
    set_join_result(
      prev_group,
      cardstacks_before,
      new_entry_base,
      DeleteEdit(Term(initial_term)),
    );
  } else {
    /* edit the term */
    set_join_result(
      prev_group,
      cardstacks_before,
      new_entry_base,
      EditVar,
    );
  };
};
let ondelim_not_delete =
    (
      ~prev_group: undo_history_group,
      ~cardstacks_before: Cardstacks.t,
      ~new_entry_base: entry_base,
      ~adjacent_is_empty_line: bool,
    )
    : group_result => {
  let (new_cursor_term_info, _, _) = new_entry_base;
  if (is_delete_emptylines(adjacent_is_empty_line, new_cursor_term_info)) {
    /* delete the adjacent empty line */
    set_join_result(
      prev_group,
      cardstacks_before,
      new_entry_base,
      DeleteEdit(EmptyLine),
    );
  } else if (CursorInfo.is_hole(new_cursor_term_info.cursor_term_before)) {
    /*     if (CursorInfo.is_exp_inside(new_cursor_term_info.cursor_term_after)) {
             /* move the cursor to the outer structure */
             set_success_join(
               prev_group,
               new_entry_base,
               Ignore,
             );
           } else */
    set_join_result(
      prev_group,
      cardstacks_before,
      new_entry_base,
      DeleteEdit(Space),
    );
  } else {
    /* move cursor to next term */
    set_success_join(
      prev_group,
      new_entry_base,
      Ignore,
    );
  };
};

let ondelim_delete =
    (
      ~prev_group: undo_history_group,
      ~cardstacks_before: Cardstacks.t,
      ~new_entry_base: entry_base,
      ~pos: DelimIndex.t,
    )
    : group_result => {
  let (new_cursor_term_info, _, _) = new_entry_base;
  if (CursorInfo.is_hole(new_cursor_term_info.cursor_term_before)) {
    /* move cursor in the hole */
    set_success_join(
      prev_group,
      new_entry_base,
      Ignore,
    );
  } else if (pos == 1 && has_typ_ann(new_cursor_term_info.cursor_term_before)) {
    /* num==1 is the position of ':' in an expression */
    set_fail_join(
      prev_group,
      new_entry_base,
      DeleteEdit(TypeAnn),
    );
  } else {
    /* delete the whole term */
    let initial_term =
      get_original_deleted_term(prev_group, new_cursor_term_info);
    set_join_result(
      prev_group,
      cardstacks_before,
      new_entry_base,
      DeleteEdit(Term(initial_term)),
    );
  };
};

let join_group =
    (
      prev_group: undo_history_group,
      new_entry_base: entry_base,
      cardstacks_before: Cardstacks.t,
    )
    : group_result => {
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  let (new_cursor_term_info, action, _) = new_entry_base;
  switch (action) {
  | Delete =>
    let prev_cursor_pos =
      get_cursor_pos(prev_entry.cursor_term_info.cursor_term_after);
    switch (prev_cursor_pos) {
    | OnText(_) =>
      ontext_delete(
        ~prev_group,
        ~cardstacks_before,
        ~new_entry_base,
        ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
      )
    | OnDelim(pos, side) =>
      switch (side) {
      | Before =>
        ondelim_delete(~prev_group, ~cardstacks_before, ~new_entry_base, ~pos)
      | After =>
        ondelim_not_delete(
          ~prev_group,
          ~cardstacks_before,
          ~new_entry_base,
          ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
        )
      }
    | OnOp(side) =>
      switch (side) {
      | Before =>
        /* delete and reach a hole */
        let initial_term =
          get_original_deleted_term(prev_group, new_cursor_term_info);
        set_join_result(
          prev_group,
          cardstacks_before,
          new_entry_base,
          DeleteEdit(Term(initial_term)),
        );
      | After =>
        /* move cursor to next term, just ignore this move */
        set_success_join(prev_group, new_entry_base, Ignore)
      }
    };

  | Backspace =>
    let prev_cursor_pos =
      get_cursor_pos(prev_entry.cursor_term_info.cursor_term_after);
    switch (prev_cursor_pos) {
    | OnText(_) =>
      ontext_delete(
        ~prev_group,
        ~cardstacks_before,
        ~new_entry_base,
        ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
      )

    | OnDelim(pos, side) =>
      switch (side) {
      | Before =>
        ondelim_not_delete(
          ~prev_group,
          ~cardstacks_before,
          ~new_entry_base,
          ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
        )

      | After =>
        ondelim_delete(~prev_group, ~cardstacks_before, ~new_entry_base, ~pos)
      }
    | OnOp(side) =>
      switch (side) {
      | Before =>
        /* move cursor to next term, just ignore this move */
        set_success_join(prev_group, new_entry_base, Ignore)
      | After =>
        let initial_term =
          get_original_deleted_term(prev_group, new_cursor_term_info);
        set_join_result(
          prev_group,
          cardstacks_before,
          new_entry_base,
          DeleteEdit(Term(initial_term)),
        );
      }
    };

  | Construct(shape) =>
    switch (shape) {
    | SLine
    | SParenthesized
    | SList
    | SAsc
    | SLam
    | SListNil
    | SInj(_)
    | SLet
    | SCase =>
      set_join_result(
        prev_group,
        cardstacks_before,
        new_entry_base,
        ConstructEdit(shape),
      )
    | SChar(_) =>
      set_join_result(prev_group, cardstacks_before, new_entry_base, EditVar)
    | SOp(op) =>
      switch (op) {
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
        set_join_result(
          prev_group,
          cardstacks_before,
          new_entry_base,
          ConstructEdit(shape),
        )

      | SSpace =>
        switch (new_cursor_term_info.cursor_term_before) {
        | Exp(_, uexp_operand) =>
          switch (uexp_operand) {
          | Var(_, InVarHole(Keyword(k), _), _) =>
            switch (k) {
            | Let =>
              switch (get_cursor_pos(new_cursor_term_info.cursor_term_before)) {
              | OnText(pos) =>
                if (pos == 3) {
                  if (group_entry(
                        prev_group,
                        cardstacks_before,
                        ConstructEdit(SLet),
                      )) {
                    let prev_group' = {
                      ...prev_group,
                      group_entries:
                        ZList.replace_z(
                          {...prev_entry, edit_action: Ignore},
                          prev_group.group_entries,
                        ),
                    };
                    set_success_join(
                      prev_group',
                      new_entry_base,
                      ConstructEdit(SLet),
                    );
                  } else {
                    set_fail_join(
                      prev_group,
                      new_entry_base,
                      ConstructEdit(SLet),
                    );
                  };
                } else {
                  construct_space(
                    prev_group,
                    cardstacks_before,
                    new_entry_base,
                  );
                }
              | OnDelim(_, _)
              | OnOp(_) =>
                construct_space(prev_group, cardstacks_before, new_entry_base)
              }

            | Case =>
              switch (get_cursor_pos(new_cursor_term_info.cursor_term_before)) {
              | OnText(pos) =>
                if (pos == 4) {
                  if (group_entry(
                        prev_group,
                        cardstacks_before,
                        ConstructEdit(SCase),
                      )) {
                    let prev_group' = {
                      ...prev_group,
                      group_entries:
                        ZList.replace_z(
                          {...prev_entry, edit_action: Ignore},
                          prev_group.group_entries,
                        ),
                    };
                    set_success_join(
                      prev_group',
                      new_entry_base,
                      ConstructEdit(SCase),
                    );
                  } else {
                    set_fail_join(
                      prev_group,
                      new_entry_base,
                      ConstructEdit(SCase),
                    );
                  };
                } else {
                  construct_space(
                    prev_group,
                    cardstacks_before,
                    new_entry_base,
                  );
                }
              | OnDelim(_, _)
              | OnOp(_) =>
                construct_space(prev_group, cardstacks_before, new_entry_base)
              }
            }
          | ApPalette(_, _, _, _) => failwith("ApPalette is not implemented")
          | _ =>
            construct_space(prev_group, cardstacks_before, new_entry_base)
          }
        | _ => construct_space(prev_group, cardstacks_before, new_entry_base)
        }
      }

    | SApPalette(_) => failwith("ApPalette is not implemented")
    }
  | MoveTo(_)
  | MoveToBefore(_)
  | MoveLeft
  | MoveRight
  | MoveToNextHole
  | MoveToPrevHole => set_success_join(prev_group, new_entry_base, Ignore)
  | UpdateApPalette(_) =>
    failwith("ApPalette is not implemented in undo_history")
  };
};

let push_edit_state =
    (
      undo_history: t,
      cardstacks_before: Cardstacks.t,
      cardstacks_after: Cardstacks.t,
      action: Action.t,
    )
    : t => {
  let prev_group = ZList.prj_z(undo_history.groups);
  let (cursor_term_before, prev_is_empty_line, next_is_empty_line) =
    get_cursor_info(cardstacks_before);
  let (cursor_term_after, _, _) = get_cursor_info(cardstacks_after);
  let cursor_term_info = {
    cursor_term_before,
    cursor_term_after,
    prev_is_empty_line,
    next_is_empty_line,
  };

  switch (
    join_group(
      prev_group,
      (cursor_term_info, action, cardstacks_after),
      cardstacks_before,
    )
  ) {
  | Success(new_group) =>
    if (ZList.prj_z(new_group.group_entries).edit_action != Ignore) {
      {
        ...undo_history,
        groups: ([], new_group, ZList.prj_suffix(undo_history.groups)),
        latest_timestamp: Unix.time(),
      };
    } else {
      {
        ...undo_history,
        groups: ZList.replace_z(new_group, undo_history.groups),
      };
    }

  | Fail(prev_group', new_entry') =>
    let timestamp = Unix.time();
    let new_group = {
      group_entries: ([], new_entry', []),
      is_expanded: false,
      timestamp,
      display_timestamp: timestamp -. undo_history.latest_timestamp > 5.,
    };
    {
      ...undo_history,
      groups: (
        [],
        new_group,
        [prev_group', ...ZList.prj_suffix(undo_history.groups)],
      ),
      latest_timestamp: timestamp,
    };
  };
};

let set_all_hidden_history = (undo_history: t, expanded: bool): t => {
  let hidden_group = (group: undo_history_group) => {
    ...group,
    is_expanded: expanded,
  };
  {
    ...undo_history,
    groups: (
      List.map(hidden_group, ZList.prj_prefix(undo_history.groups)),
      hidden_group(ZList.prj_z(undo_history.groups)),
      List.map(hidden_group, ZList.prj_suffix(undo_history.groups)),
    ),
    all_hidden_history_expand: expanded,
  };
};
