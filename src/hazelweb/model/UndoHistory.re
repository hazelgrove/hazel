type cursor_term = CursorInfo.cursor_term;
type structure =
  | LetBinding
  | Lamda
  | CaseMatch
  | TypeAnn
  | ShapeEdit(option(MetaVar.t), Action.shape);
type edit_action =
  | DeleteToHole(MetaVar.t, cursor_term)
  | DeleteToNotHole(cursor_term)
  | DeleteHole(MetaVar.t)
  | DeleteEmptyLine
  | DeleteEdit
  | InsertHole(MetaVar.t, option(MetaVar.t))
  | InsertEdit(option(MetaVar.t))
  | InsertEmptyLine
  | Construct(structure)
  | DeleteTypeAnn
  | NotSet;

type info = {
  previous_action: Action.t,
  previous_cursor_term: cursor_term,
  current_cursor_term: cursor_term,
  prev_is_empty_line: bool, /* who's mpty line */
  next_is_empty_line: bool,
  edit_action,
};
type undo_history_entry = {
  cardstacks: Cardstacks.t,
  info: option(info),
};

type undo_history_group = {
  group_entries: ZList.t(undo_history_entry, undo_history_entry),
  is_expanded: bool,
  /* [is_complete: bool] if any cursor-moving action interupts the current edit,
     the current group becomes complete.
     Next action will start a new group */
  is_complete: bool,
};

type t = ZList.t(undo_history_group, undo_history_group);
let edit_action_is_DeleteEmptyLine = (edit_action: edit_action): bool => {
  switch (edit_action) {
  | DeleteEmptyLine => true
  | DeleteToHole(_, _)
  | InsertEdit(_)
  | InsertHole(_, _)
  | DeleteToNotHole(_)
  | DeleteHole(_)
  | DeleteEdit
  | Construct(_)
  | InsertEmptyLine
  | DeleteTypeAnn
  | NotSet => false
  };
};
let action_is_Schar = (action: Action.t): bool => {
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
    | SLine
    | SCase
    | SOp(_) => false
    | SApPalette(_) =>
      failwith("ApPalette is not implemented in undo_history")
    | SChar(_) => true
    }
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
let can_delete_typ_ann = (cursor_term: cursor_term): bool => {
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

type group_result =
  | Success(undo_history_group)
  | Fail(undo_history_group, undo_history_entry, bool);

let set_fail_join =
    (
      prev_group: undo_history_group,
      new_entry: undo_history_entry,
      new_edit_action: option(edit_action),
      is_complete_entry: bool,
    )
    : group_result => {
  let prev_group' = {...prev_group, is_complete: true};
  let new_entry' =
    switch (new_entry.info) {
    | None => new_entry
    | Some(new_info) =>
      switch (new_edit_action) {
      | None => {...new_entry, info: None}
      | Some(edit_action) => {
          ...new_entry,
          info: Some({...new_info, edit_action}),
        }
      }
    };
  Fail(prev_group', new_entry', is_complete_entry);
};

let set_success_join =
    (
      prev_group: undo_history_group,
      new_entry: undo_history_entry,
      new_edit_action: option(edit_action),
      is_complete: bool,
    )
    : group_result => {
  let new_entry' =
    switch (new_entry.info) {
    | None => new_entry
    | Some(new_info) =>
      switch (new_edit_action) {
      | None => {...new_entry, info: None}
      | Some(edit_action) => {
          ...new_entry,
          info: Some({...new_info, edit_action}),
        }
      }
    };
  Success(push_history_entry(prev_group, new_entry', is_complete));
};

let construct_holes =
    (prev_group: undo_history_group, new_entry: undo_history_entry)
    : group_result =>
  switch (new_entry.info) {
  | None => set_fail_join(prev_group, new_entry, None, true)
  | Some(new_entry_info) =>
    if (CursorInfo.is_empty_line(new_entry_info.previous_cursor_term)) {
      switch (CursorInfo.is_hole(new_entry_info.current_cursor_term)) {
      | None => failwith("impossible match")
      | Some(hole_id) =>
        set_fail_join(
          prev_group,
          new_entry,
          Some(InsertHole(hole_id, Some(hole_id - 1))),
          false,
        )
      };
    } else {
      switch (CursorInfo.is_hole(new_entry_info.current_cursor_term)) {
      | None => set_fail_join(prev_group, new_entry, None, false)
      | Some(hole_id) =>
        set_fail_join(
          prev_group,
          new_entry,
          Some(InsertHole(hole_id, None)),
          false,
        )
      };
    }
  };
let get_initial_term_before_delete =
    (group: undo_history_group, new_entry_info: info): cursor_term => {
  let suffix = ZList.prj_suffix(group.group_entries);
  let delete_edit_info_filter = (info: info): cursor_term =>
    switch (info.edit_action) {
    | DeleteEdit => info.previous_cursor_term
    | DeleteToHole(_, _)
    | DeleteToNotHole(_)
    | DeleteHole(_)
    | DeleteEmptyLine
    | InsertEdit(_)
    | InsertHole(_, _)
    | Construct(_)
    | InsertEmptyLine
    | DeleteTypeAnn
    | NotSet => new_entry_info.previous_cursor_term
    };
  switch (List.rev(suffix)) {
  | [] =>
    switch (ZList.prj_z(group.group_entries).info) {
    | None => new_entry_info.previous_cursor_term
    | Some(info) => delete_edit_info_filter(info)
    }
  | ls =>
    switch (List.hd(ls).info) {
    | None => new_entry_info.previous_cursor_term
    | Some(info) => delete_edit_info_filter(info)
    }
  };
};

let get_insert_hole = (group: undo_history_group): option(MetaVar.t) => {
  let suffix = ZList.prj_suffix(group.group_entries);
  let delete_edit_info_filter = (info: info): option(MetaVar.t) =>
    switch (info.edit_action) {
    | InsertEdit(hole) => hole
    | InsertHole(hole_id, _) => Some(hole_id)
    | DeleteEdit
    | DeleteToHole(_, _)
    | DeleteToNotHole(_)
    | DeleteHole(_)
    | DeleteEmptyLine
    | Construct(_)
    | InsertEmptyLine
    | DeleteTypeAnn
    | NotSet => None
    };
  switch (List.rev(suffix)) {
  | [] =>
    switch (ZList.prj_z(group.group_entries).info) {
    | None => None
    | Some(info) => delete_edit_info_filter(info)
    }
  | ls =>
    switch (List.hd(ls).info) {
    | None => None
    | Some(info) => delete_edit_info_filter(info)
    }
  };
};
let entry_to_start_a_group =
    (prev_group: undo_history_group, new_entry: undo_history_entry)
    : group_result => {
  switch (new_entry.info) {
  | None => set_fail_join(prev_group, new_entry, None, true)
  | Some(new_entry_info) =>
    let prev_cursor_pos = get_cursor_pos(new_entry_info.previous_cursor_term);
    let ontext_func =
        (
          ~jump_judge_func,
          ~prev_group: undo_history_group,
          ~new_entry: undo_history_entry,
          ~new_entry_info: info,
          ~pos: CharIndex.t,
          ~adjacent_is_empty_line: bool,
        )
        : group_result => {
      let prev_cursor_pos =
        get_cursor_pos(new_entry_info.previous_cursor_term);
      let new_cursor_pos = get_cursor_pos(new_entry_info.current_cursor_term);
      if (adjacent_is_empty_line && pos == 0) {
        /* whether delete the previous empty line */
        set_fail_join(
          prev_group,
          new_entry,
          Some(DeleteEmptyLine),
          false,
        );
      } else if (jump_judge_func(prev_cursor_pos, new_cursor_pos)) {
        /* jump to next term */
        set_fail_join(
          prev_group,
          new_entry,
          None,
          true,
        );
      } else {
        /* normal edit */
        switch (CursorInfo.is_hole(new_entry_info.current_cursor_term)) {
        | None =>
          if (CursorInfo.is_empty_line(new_entry_info.current_cursor_term)) {
            set_fail_join(
              prev_group,
              new_entry,
              Some(DeleteToNotHole(new_entry_info.previous_cursor_term)),
              true,
            );
          } else {
            set_fail_join(prev_group, new_entry, Some(DeleteEdit), false);
          }
        | Some(hole_id) =>
          set_fail_join(
            prev_group,
            new_entry,
            Some(DeleteToHole(hole_id, new_entry_info.previous_cursor_term)),
            true,
          )
        };
      };
    };

    let ondelim_undel =
        (
          ~prev_group: undo_history_group,
          ~new_entry: undo_history_entry,
          ~new_entry_info: info,
          ~adjacent_is_empty_line: bool,
        )
        : group_result =>
      if (adjacent_is_empty_line) {
        /* whether delete the previous empty line */
        set_fail_join(
          prev_group,
          new_entry,
          Some(DeleteEmptyLine),
          false,
        );
      } else {
        switch (CursorInfo.is_hole(new_entry_info.previous_cursor_term)) {
        | Some(hole_id) =>
          if (CursorInfo.is_exp_inside(new_entry_info.current_cursor_term)) {
            set_fail_join(prev_group, new_entry, None, true);
          } else {
            set_fail_join(
              prev_group,
              new_entry,
              Some(DeleteHole(hole_id)),
              true,
            );
          }
        | None =>
          /* move cursor to next term, just ignore this move */
          set_fail_join(prev_group, new_entry, None, true)
        };
      };

    let ondelim_del =
        (
          ~prev_group: undo_history_group,
          ~new_entry: undo_history_entry,
          ~new_entry_info: info,
          ~pos: DelimIndex.t,
        )
        : group_result => {
      switch (CursorInfo.is_hole(new_entry_info.previous_cursor_term)) {
      | Some(_) =>
        /* move cursor in the hole */
        set_fail_join(prev_group, new_entry, None, true)
      | None =>
        if (pos == 1
            && can_delete_typ_ann(new_entry_info.previous_cursor_term)) {
          /* num==1 is the position of ':' in an expression */
          set_fail_join(
            prev_group,
            new_entry,
            Some(DeleteTypeAnn),
            true,
          );
        } else {
          switch (CursorInfo.is_hole(new_entry_info.current_cursor_term)) {
          | Some(hole_id) =>
            /* delete and reach a hole */
            set_fail_join(
              prev_group,
              new_entry,
              Some(
                DeleteToHole(hole_id, new_entry_info.previous_cursor_term),
              ),
              true,
            )
          | None =>
            /* delete and not reach a hole */
            set_fail_join(
              prev_group,
              new_entry,
              Some(DeleteToNotHole(new_entry_info.previous_cursor_term)),
              true,
            )
          };
        }
      };
    };

    let op_del =
        (
          ~prev_group: undo_history_group,
          ~new_entry: undo_history_entry,
          ~new_entry_info: info,
        )
        : group_result => {
      switch (CursorInfo.is_hole(new_entry_info.current_cursor_term)) {
      | Some(hole_id) =>
        /* delete and reach a hole */
        set_fail_join(
          prev_group,
          new_entry,
          Some(DeleteToHole(hole_id, new_entry_info.previous_cursor_term)),
          true,
        )
      | None =>
        /* delete and not reach a hole */
        set_fail_join(
          prev_group,
          new_entry,
          Some(DeleteToNotHole(new_entry_info.previous_cursor_term)),
          true,
        )
      };
    };
    switch (new_entry_info.previous_action) {
    | Delete =>
      switch (prev_cursor_pos) {
      | OnText(pos) =>
        ontext_func(
          ~jump_judge_func=cursor_jump_after_delete,
          ~prev_group,
          ~new_entry,
          ~new_entry_info,
          ~pos,
          ~adjacent_is_empty_line=new_entry_info.next_is_empty_line,
        )
      | OnDelim(pos, side) =>
        switch (side) {
        | Before =>
          ondelim_del(~prev_group, ~new_entry, ~new_entry_info, ~pos)
        | After =>
          ondelim_undel(
            ~prev_group,
            ~new_entry,
            ~new_entry_info,
            ~adjacent_is_empty_line=new_entry_info.next_is_empty_line,
          )
        }
      | OnOp(side) =>
        switch (side) {
        | Before => op_del(~prev_group, ~new_entry, ~new_entry_info)
        | After =>
          /* move cursor to next term, just ignore this move */
          set_fail_join(prev_group, new_entry, None, true)
        }
      }
    | Backspace =>
      switch (prev_cursor_pos) {
      | OnText(pos) =>
        ontext_func(
          ~jump_judge_func=cursor_jump_after_backspace,
          ~prev_group,
          ~new_entry,
          ~new_entry_info,
          ~pos,
          ~adjacent_is_empty_line=new_entry_info.prev_is_empty_line,
        )
      | OnDelim(pos, side) =>
        switch (side) {
        | Before =>
          ondelim_undel(
            ~prev_group,
            ~new_entry,
            ~new_entry_info,
            ~adjacent_is_empty_line=new_entry_info.prev_is_empty_line,
          )

        | After => ondelim_del(~prev_group, ~new_entry, ~new_entry_info, ~pos)
        }
      | OnOp(side) =>
        switch (side) {
        | Before =>
          /* move cursor to next term, just ignore this move */
          set_fail_join(prev_group, new_entry, None, true)
        | After => op_del(~prev_group, ~new_entry, ~new_entry_info)
        }
      }
    | Construct(shape) =>
      switch (shape) {
      | SChar(_) =>
        /* if previous is hole then combine else if previous is char then combine else start a new group */
        switch (CursorInfo.is_hole(new_entry_info.previous_cursor_term)) {
        | None =>
          set_fail_join(
            prev_group,
            new_entry,
            Some(InsertEdit(None)),
            false,
          )
        | Some(hole_id) =>
          set_fail_join(
            prev_group,
            new_entry,
            Some(InsertEdit(Some(hole_id))),
            false,
          )
        }
      | SLine =>
        set_fail_join(prev_group, new_entry, Some(InsertEmptyLine), false)
      | SParenthesized
      | SList
      | SAsc
      | SLam
      | SListNil
      | SInj(_)
      | SLet
      | SCase =>
        switch (CursorInfo.is_hole(new_entry_info.previous_cursor_term)) {
        | None =>
          set_fail_join(
            prev_group,
            new_entry,
            Some(Construct(ShapeEdit(None, shape))),
            true,
          )
        | Some(hole_id) =>
          set_fail_join(
            prev_group,
            new_entry,
            Some(Construct(ShapeEdit(Some(hole_id), shape))),
            true,
          )
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
          switch (CursorInfo.is_hole(new_entry_info.previous_cursor_term)) {
          | None =>
            set_fail_join(
              prev_group,
              new_entry,
              Some(InsertEdit(None)),
              true,
            )
          | Some(hole_id) =>
            set_fail_join(
              prev_group,
              new_entry,
              Some(InsertEdit(Some(hole_id))),
              true,
            )
          }
        | SSpace =>
          switch (new_entry_info.previous_cursor_term) {
          | Exp(_, uexp_operand) =>
            switch (uexp_operand) {
            | Var(_, InVarHole(Keyword(k), _), _) =>
              switch (k) {
              | Let =>
                set_fail_join(
                  prev_group,
                  new_entry,
                  Some(Construct(LetBinding)),
                  true,
                )
              | Case =>
                set_fail_join(
                  prev_group,
                  new_entry,
                  Some(Construct(CaseMatch)),
                  true,
                )
              }
            | EmptyHole(_)
            | Var(_, _, _)
            | NumLit(_, _)
            | BoolLit(_, _)
            | ListNil(_)
            | Lam(_, _, _, _)
            | Inj(_, _, _)
            | Case(_, _, _, _)
            | Parenthesized(_) => construct_holes(prev_group, new_entry)
            | ApPalette(_, _, _, _) =>
              failwith("ApPalette is not implemented")
            }
          | Pat(_, _)
          | Typ(_, _)
          | ExpOp(_, _)
          | PatOp(_, _)
          | TypOp(_, _)
          | Line(_, _)
          | Rule(_, _) => construct_holes(prev_group, new_entry)
          }
        }
      | SApPalette(_) => failwith("ApPalette is not implemented")
      }
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
};
let join_group =
    (prev_group: undo_history_group, new_entry: undo_history_entry)
    : group_result => {
  let prev_last_entry = ZList.prj_z(prev_group.group_entries);
  let prev_complete = prev_group.is_complete;
  switch (prev_last_entry.info, new_entry.info) {
  | (_, None) => set_fail_join(prev_group, new_entry, None, true)
  | (None, Some(_)) => entry_to_start_a_group(prev_group, new_entry)
  | (Some(prev_entry_info), Some(new_entry_info)) =>
    switch (prev_entry_info.previous_action, new_entry_info.previous_action) {
    | (prev_ac, Delete) =>
      if (prev_ac != Delete || prev_complete) {
        entry_to_start_a_group(prev_group, new_entry);
      } else {
        let prev_cursor_pos =
          get_cursor_pos(prev_entry_info.current_cursor_term);
        let new_cursor_pos =
          get_cursor_pos(new_entry_info.current_cursor_term);
        switch (prev_cursor_pos) {
        | OnText(_) =>
          if (cursor_jump_after_delete(prev_cursor_pos, new_cursor_pos)) {
            /* jump to next term */
            set_fail_join(
              prev_group,
              new_entry,
              None,
              true,
            );
          } else {
            switch (CursorInfo.is_hole(new_entry_info.current_cursor_term)) {
            | None =>
              if (CursorInfo.is_empty_line(new_entry_info.current_cursor_term)) {
                let initial_term =
                  get_initial_term_before_delete(prev_group, new_entry_info);

                set_fail_join(
                  prev_group,
                  new_entry,
                  Some(DeleteToNotHole(initial_term)),
                  true,
                );
              } else {
                set_success_join(
                  prev_group,
                  new_entry,
                  Some(DeleteEdit),
                  false,
                );
              }

            | Some(hole_id) =>
              let initial_term =
                get_initial_term_before_delete(prev_group, new_entry_info);
              set_success_join(
                prev_group,
                new_entry,
                Some(DeleteToHole(hole_id, initial_term)),
                true,
              );
            };
          }
        | OnDelim(num, side) =>
          switch (side) {
          | Before =>
            switch (CursorInfo.is_hole(prev_entry_info.current_cursor_term)) {
            | Some(_) =>
              /* move cursor in the hole */
              set_fail_join(prev_group, new_entry, None, true)
            | None =>
              if (num == 1
                  && can_delete_typ_ann(prev_entry_info.current_cursor_term)) {
                /* num==1 is the position of ':' in an expression */
                set_fail_join(
                  prev_group,
                  new_entry,
                  Some(DeleteTypeAnn),
                  true,
                );
              } else {
                switch (
                  CursorInfo.is_hole(new_entry_info.current_cursor_term)
                ) {
                | Some(hole_id) =>
                  /* delete and reach a hole */
                  let initial_term =
                    get_initial_term_before_delete(
                      prev_group,
                      new_entry_info,
                    );
                  set_success_join(
                    prev_group,
                    new_entry,
                    Some(DeleteToHole(hole_id, initial_term)),
                    true,
                  );

                | None =>
                  /* delete and not reach a hole */
                  let initial_term =
                    get_initial_term_before_delete(
                      prev_group,
                      new_entry_info,
                    );
                  set_success_join(
                    prev_group,
                    new_entry,
                    Some(DeleteToNotHole(initial_term)),
                    true,
                  );
                };
              }
            }
          | After =>
            switch (CursorInfo.is_hole(prev_entry_info.current_cursor_term)) {
            | Some(hole_id) =>
              if (CursorInfo.is_exp_inside(new_entry_info.current_cursor_term)) {
                set_fail_join(prev_group, new_entry, None, true);
              } else {
                set_fail_join(
                  prev_group,
                  new_entry,
                  Some(DeleteHole(hole_id)),
                  true,
                );
              }
            | None =>
              /* move cursor to next term, just ignore this move */
              set_fail_join(prev_group, new_entry, None, true)
            }
          }
        | OnOp(side) =>
          switch (side) {
          | Before =>
            switch (CursorInfo.is_hole(new_entry_info.current_cursor_term)) {
            | Some(hole_id) =>
              /* delete and reach a hole */
              let initial_term =
                get_initial_term_before_delete(prev_group, new_entry_info);
              set_success_join(
                prev_group,
                new_entry,
                Some(DeleteToHole(hole_id, initial_term)),
                true,
              );

            | None =>
              /* delete and not reach a hole */
              let initial_term =
                get_initial_term_before_delete(prev_group, new_entry_info);
              set_success_join(
                prev_group,
                new_entry,
                Some(DeleteToNotHole(initial_term)),
                true,
              );
            }
          | After =>
            /* move cursor to next term, just ignore this move */
            set_fail_join(prev_group, new_entry, None, true)
          }
        };
      }
    | (prev_ac, Backspace) =>
      if (prev_ac != Backspace || prev_complete) {
        entry_to_start_a_group(prev_group, new_entry);
      } else {
        let prev_cursor_pos =
          get_cursor_pos(prev_entry_info.current_cursor_term);
        let new_cursor_pos =
          get_cursor_pos(new_entry_info.current_cursor_term);
        switch (prev_cursor_pos) {
        | OnText(_) =>
          if (prev_entry_info.prev_is_empty_line) {
            /* whether delete the previous empty line */
            if (edit_action_is_DeleteEmptyLine(prev_entry_info.edit_action)) {
              set_success_join(
                prev_group,
                new_entry,
                Some(DeleteEmptyLine),
                false,
              );
            } else {
              set_fail_join(
                prev_group,
                new_entry,
                Some(DeleteEmptyLine),
                false,
              );
            };
          } else if (cursor_jump_after_backspace(
                       prev_cursor_pos,
                       new_cursor_pos,
                     )) {
            /* jump to next term */
            set_fail_join(
              prev_group,
              new_entry,
              None,
              true,
            );
          } else {
            switch (CursorInfo.is_hole(new_entry_info.current_cursor_term)) {
            | None =>
              if (CursorInfo.is_empty_line(new_entry_info.current_cursor_term)) {
                let initial_term =
                  get_initial_term_before_delete(prev_group, new_entry_info);
                set_success_join(
                  prev_group,
                  new_entry,
                  Some(DeleteToNotHole(initial_term)),
                  true,
                );
              } else {
                set_success_join(
                  prev_group,
                  new_entry,
                  Some(DeleteEdit),
                  false,
                );
              }
            | Some(hole_id) =>
              let initial_term =
                get_initial_term_before_delete(prev_group, new_entry_info);
              set_success_join(
                prev_group,
                new_entry,
                Some(DeleteToHole(hole_id, initial_term)),
                true,
              );
            };
          }
        | OnDelim(num, side) =>
          switch (side) {
          | Before =>
            switch (CursorInfo.is_hole(prev_entry_info.current_cursor_term)) {
            | Some(hole_id) =>
              if (prev_entry_info.prev_is_empty_line) {
                /* whether delete the previous empty line */
                if (edit_action_is_DeleteEmptyLine(
                      prev_entry_info.edit_action,
                    )) {
                  set_success_join(
                    prev_group,
                    new_entry,
                    Some(DeleteEmptyLine),
                    false,
                  );
                } else {
                  set_fail_join(
                    prev_group,
                    new_entry,
                    Some(DeleteEmptyLine),
                    false,
                  );
                };
              } else if (CursorInfo.is_exp_inside(
                           new_entry_info.current_cursor_term,
                         )) {
                set_fail_join(prev_group, new_entry, None, true);
              } else {
                set_fail_join(
                  prev_group,
                  new_entry,
                  Some(DeleteHole(hole_id)),
                  true,
                );
              }
            | None =>
              /* move cursor to next term, just ignore this move */
              if (prev_entry_info.prev_is_empty_line) {
                /* whether delete the previous empty line */
                if (edit_action_is_DeleteEmptyLine(
                      prev_entry_info.edit_action,
                    )) {
                  set_success_join(
                    prev_group,
                    new_entry,
                    Some(DeleteEmptyLine),
                    false,
                  );
                } else {
                  set_fail_join(
                    prev_group,
                    new_entry,
                    Some(DeleteEmptyLine),
                    false,
                  );
                };
              } else {
                set_fail_join(prev_group, new_entry, None, true);
              }
            }

          | After =>
            switch (CursorInfo.is_hole(prev_entry_info.current_cursor_term)) {
            | Some(_) =>
              /* move cursor in the hole */
              set_fail_join(prev_group, new_entry, None, true)
            | None =>
              if (num == 1
                  && can_delete_typ_ann(prev_entry_info.current_cursor_term)) {
                /* num==1 is the position of ':' in an expression */
                set_fail_join(
                  prev_group,
                  new_entry,
                  Some(DeleteTypeAnn),
                  true,
                );
              } else {
                switch (
                  CursorInfo.is_hole(new_entry_info.current_cursor_term)
                ) {
                | Some(hole_id) =>
                  /* delete and reach a hole */
                  let initial_term =
                    get_initial_term_before_delete(
                      prev_group,
                      new_entry_info,
                    );
                  set_success_join(
                    prev_group,
                    new_entry,
                    Some(DeleteToHole(hole_id, initial_term)),
                    true,
                  );
                | None =>
                  /* delete and not reach a hole */
                  let initial_term =
                    get_initial_term_before_delete(
                      prev_group,
                      new_entry_info,
                    );
                  set_success_join(
                    prev_group,
                    new_entry,
                    Some(DeleteToNotHole(initial_term)),
                    true,
                  );
                };
              }
            }
          }
        | OnOp(side) =>
          switch (side) {
          | Before =>
            /* move cursor to next term, just ignore this move */
            set_fail_join(prev_group, new_entry, None, true)
          | After =>
            switch (CursorInfo.is_hole(new_entry_info.current_cursor_term)) {
            | Some(hole_id) =>
              /* delete and reach a hole */

              let initial_term =
                get_initial_term_before_delete(prev_group, new_entry_info);
              set_success_join(
                prev_group,
                new_entry,
                Some(DeleteToHole(hole_id, initial_term)),
                true,
              );
            | None =>
              /* delete and not reach a hole */
              let initial_term =
                get_initial_term_before_delete(prev_group, new_entry_info);
              set_success_join(
                prev_group,
                new_entry,
                Some(DeleteToNotHole(initial_term)),
                true,
              );
            }
          }
        };
      }
    | (prev_ac, Construct(shape_2)) =>
      if (prev_complete) {
        entry_to_start_a_group(prev_group, new_entry);
      } else {
        switch (shape_2) {
        | SLine =>
          if (action_is_Sline(prev_ac)) {
            set_success_join(
              prev_group,
              new_entry,
              Some(InsertEmptyLine),
              false,
            );
          } else {
            set_fail_join(
              prev_group,
              new_entry,
              Some(InsertEmptyLine),
              false,
            );
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
          switch (CursorInfo.is_hole(prev_entry_info.current_cursor_term)) {
          | None =>
            set_fail_join(
              prev_group,
              new_entry,
              Some(Construct(ShapeEdit(None, shape_2))),
              true,
            )
          | Some(hole_id) =>
            set_success_join(
              prev_group,
              new_entry,
              Some(Construct(ShapeEdit(Some(hole_id), shape_2))),
              true,
            )
          }
        | SChar(_) =>
          /* if previous is hole then combine else if previous is char then combine else start a new group */
          switch (CursorInfo.is_hole(prev_entry_info.current_cursor_term)) {
          | None =>
            if (action_is_Schar(prev_ac)) {
              set_success_join(
                prev_group,
                new_entry,
                Some(InsertEdit(get_insert_hole(prev_group))),
                false,
              );
            } else {
              set_fail_join(
                prev_group,
                new_entry,
                Some(InsertEdit(get_insert_hole(prev_group))),
                false,
              );
            }
          | Some(hole_id) =>
            set_success_join(
              prev_group,
              new_entry,
              Some(InsertEdit(Some(hole_id))),
              false,
            )
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
            switch (CursorInfo.is_hole(prev_entry_info.current_cursor_term)) {
            | None =>
              set_fail_join(
                prev_group,
                new_entry,
                Some(Construct(ShapeEdit(None, shape_2))),
                true,
              )
            | Some(hole_id) =>
              set_success_join(
                prev_group,
                new_entry,
                Some(Construct(ShapeEdit(Some(hole_id), shape_2))),
                true,
              )
            }
          | SSpace =>
            switch (prev_entry_info.current_cursor_term) {
            | Exp(_, uexp_operand) =>
              switch (uexp_operand) {
              | Var(_, InVarHole(Keyword(k), _), _) =>
                switch (k) {
                | Let =>
                  let prev_group' = {
                    ...prev_group,
                    group_entries:
                      ZList.replace_z(
                        {...prev_last_entry, info: None},
                        prev_group.group_entries,
                      ),
                  };
                  set_success_join(
                    prev_group',
                    new_entry,
                    Some(Construct(LetBinding)),
                    true,
                  );
                | Case =>
                  let prev_group' = {
                    ...prev_group,
                    group_entries:
                      ZList.replace_z(
                        {...prev_last_entry, info: None},
                        prev_group.group_entries,
                      ),
                  };
                  set_success_join(
                    prev_group',
                    new_entry,
                    Some(Construct(CaseMatch)),
                    true,
                  );
                }
              | EmptyHole(_)
              | Var(_, _, _)
              | NumLit(_, _)
              | BoolLit(_, _)
              | ListNil(_)
              | Lam(_, _, _, _)
              | Inj(_, _, _)
              | Case(_, _, _, _)
              | Parenthesized(_) => construct_holes(prev_group, new_entry)
              | ApPalette(_, _, _, _) =>
                failwith("ApPalette is not implemented")
              }
            | Pat(_, _)
            | Typ(_, _)
            | ExpOp(_, _)
            | PatOp(_, _)
            | TypOp(_, _)
            | Line(_, _)
            | Rule(_, _) => construct_holes(prev_group, new_entry)
            }
          }

        | SApPalette(_) => failwith("ApPalette is not implemented")
        };
      }
    | (_, UpdateApPalette(_)) =>
      failwith("ApPalette is not implemented in undo_history")
    | (_, MoveTo(_))
    | (_, MoveToBefore(_))
    | (_, MoveLeft)
    | (_, MoveRight)
    | (_, MoveToNextHole)
    | (_, MoveToPrevHole) =>
      failwith(
        "Impossible match. Not undoable actions will not be added into history",
      )
    }
  };
};

let push_edit_state =
    (
      undo_history: t,
      prev_cardstacks: Cardstacks.t,
      cur_cardstacks: Cardstacks.t,
      action: Action.t,
    )
    : t => {
  let prev_group = ZList.prj_z(undo_history);
  if (undoable_action(action)) {
    let (prev_cursor_term, prev_is_empty_line, next_is_empty_line) =
      get_cursor_info(prev_cardstacks);
    let (cur_cursor_term, _, _) = get_cursor_info(cur_cardstacks);
    let new_entry_info = {
      previous_action: action,
      previous_cursor_term: prev_cursor_term,
      current_cursor_term: cur_cursor_term,
      prev_is_empty_line,
      next_is_empty_line,
      edit_action: NotSet,
    };
    let new_entry = {cardstacks: cur_cardstacks, info: Some(new_entry_info)};
    switch (join_group(prev_group, new_entry)) {
    | Success(new_group) => ([], new_group, ZList.prj_suffix(undo_history))
    | Fail(prev_group', new_entry', is_complete_entry) =>
      let new_group = {
        group_entries: ([], new_entry', []),
        is_expanded: false,
        is_complete: is_complete_entry,
      };
      ([], new_group, [prev_group', ...ZList.prj_suffix(undo_history)]);
    };
  } else {
    /* if any cursor-moving action interupts the current edit,
       the current group becomes complete. */
    let prev_group' = {...prev_group, is_complete: true};
    ZList.replace_z(prev_group', undo_history);
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
