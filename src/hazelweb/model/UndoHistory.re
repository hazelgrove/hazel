open Sexplib.Std;

type cursor_term = CursorInfo.cursor_term;

type delete_edit =
  | Term(cursor_term)
  | Space
  | EmptyLine
  | TypeAnn;

type var_edit =
  | Insert
  | DeleteInsert
  | Edit;

type edit_action =
  | Var(var_edit)
  | DeleteEdit(delete_edit)
  | ConstructEdit(Action.shape)
  | MatchRule
  | Init;

type cursor_term_info = {
  cursor_term_before: cursor_term,
  cursor_term_after: cursor_term,
  zexp_before: ZExp.t,
  zexp_after: ZExp.t,
  prev_is_empty_line: bool,
  next_is_empty_line: bool,
};

type undo_history_entry = {
  cardstacks: Cardstacks.t,
  caret_jumps_to: option(Cardstacks.t),
  cursor_term_info,
  previous_action: option(Action.t),
  edit_action,
  timestamp: float,
};

type undo_history_group = {
  group_entries: ZList.t(undo_history_entry, undo_history_entry),
  is_expanded: bool,
};

[@deriving sexp]
type t = {
  groups: [@sexp.opaque] ZList.t(undo_history_group, undo_history_group),
  all_hidden_history_expand: bool,
  /* mute the auto-scrolling when disable_auto_scrolling is true */
  disable_auto_scrolling: bool,
  preview_on_hover: bool,
  hover_recover_group_id: int,
  hover_recover_elt_id: int,
  cur_group_id: int,
  cur_elt_id: int,
};

let update_disable_auto_scrolling = (disable_auto_scrolling: bool, history: t) => {
  {...history, disable_auto_scrolling};
};
let get_cardstacks = (history: t, ignore_caret_jump: bool): Cardstacks.t => {
  let cur_entry = ZList.prj_z(ZList.prj_z(history.groups).group_entries);
  if (ignore_caret_jump) {
    cur_entry.cardstacks;
  } else {
    switch (cur_entry.caret_jumps_to) {
    | None => cur_entry.cardstacks
    | Some(cardstacks) => cardstacks
    };
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

let push_history_entry =
    (
      ~prev_group: undo_history_group,
      ~new_entry: undo_history_entry,
      ~is_expanded: bool,
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
    is_expanded,
  };
};

/* return true if caret jump to another term when new action applied */
let caret_jump =
    (prev_group: undo_history_group, new_cardstacks_before: Cardstacks.t)
    : bool => {
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  let prev_step =
    prev_entry.cardstacks |> Cardstacks.get_program |> Program.get_steps;
  let new_step =
    new_cardstacks_before |> Cardstacks.get_program |> Program.get_steps;
  prev_step != new_step;
};

/* return true if new edit_action can be grouped with the preivous edit_action */
let group_edit_action =
    (edit_action_prev: edit_action, edit_action_next: edit_action): bool =>
  switch (edit_action_prev, edit_action_next) {
  | (MatchRule, MatchRule) => true
  | (MatchRule, _) => false
  | (Var(_), Var(_)) => true
  | (Var(_), DeleteEdit(delete_edit)) =>
    switch (delete_edit) {
    | Term(_) => true
    | Space
    | EmptyLine
    | TypeAnn => false
    }
  | (Var(_), ConstructEdit(construct_edit)) =>
    switch (construct_edit) {
    | SLet
    | SCase => true
    | _ => false
    }
  | (Var(_), _) => false
  | (DeleteEdit(delete_edit_1), DeleteEdit(delete_edit_2)) =>
    switch (delete_edit_1, delete_edit_2) {
    | (Space, Space)
    | (EmptyLine, EmptyLine) => true
    | (Term(term_1), Term(term_2)) =>
      switch (term_1, term_2) {
      | (Rule(_, _), Rule(_, _)) => true
      | _ => false
      }
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
  | (Init, _) => false
  };

/* return true if new entry can be grouped into the previous group */
let group_entry =
    (
      ~prev_group: undo_history_group,
      ~new_cardstacks_before: Cardstacks.t,
      ~new_edit_action: edit_action,
    )
    : bool => {
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  let can_group_edit_action =
    group_edit_action(prev_entry.edit_action, new_edit_action);

  /* edit actions like construct space/new lines should be grouped together,
     so cursor jump should be ignored in those cases */
  let ignore_caret_jump =
    switch (prev_entry.edit_action, new_edit_action) {
    | (DeleteEdit(delete_edit_1), DeleteEdit(delete_edit_2)) =>
      switch (delete_edit_1, delete_edit_2) {
      | (Space, Space)
      | (EmptyLine, EmptyLine) => true
      | (Term(term_1), Term(term_2)) =>
        switch (term_1, term_2) {
        | (Rule(_, _), Rule(_, _)) => true
        | _ => false
        }
      | _ => false
      }
    | (ConstructEdit(construct_edit_1), ConstructEdit(construct_edit_2)) =>
      switch (construct_edit_1, construct_edit_2) {
      | (SOp(SSpace), SOp(SSpace))
      | (SLine, SLine) => true
      | _ => false
      }
    | _ => false
    };

  can_group_edit_action
  && (!caret_jump(prev_group, new_cardstacks_before) || ignore_caret_jump);
};

type comp_len_typ =
  | MaxLen
  | Move
  | Len(int);

let comp_len_lt =
    (cursor_len_1: comp_len_typ, cursor_len_2: comp_len_typ): bool => {
  switch (cursor_len_1, cursor_len_2) {
  | (MaxLen, MaxLen) => false
  | (_, MaxLen) => true
  | (_, Move) => false
  | (MaxLen, _) => false
  | (Move, _) => true
  | (Len(len1), Len(len2)) => len1 < len2
  };
};

let cursor_term_len = (cursor_term: cursor_term): comp_len_typ => {
  switch (cursor_term) {
  | Exp(_, operand) =>
    switch (operand) {
    | EmptyHole(_) => Move
    | Var(_, _, var) => Len(Var.length(var))
    | IntLit(_, num)
    | FloatLit(_, num) => Len(String.length(num))
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
    | EmptyHole(_) => Move
    | Wild(_) => Len(1)
    | Var(_, _, var) => Len(Var.length(var))
    | IntLit(_, num)
    | FloatLit(_, num) => Len(String.length(num))
    | BoolLit(_, _)
    | ListNil(_)
    | Parenthesized(_)
    | Inj(_, _, _) => MaxLen
    }
  | Typ(_, operand) =>
    switch (operand) {
    | Hole => Move
    | Unit
    | Int
    | Float
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
    | EmptyLine => Move
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
    (
      prev_group: undo_history_group,
      new_cardstacks_before: Cardstacks.t,
      new_cursor_term_info: cursor_term_info,
    )
    : cursor_term =>
  if (group_entry(
        ~prev_group,
        ~new_cardstacks_before,
        ~new_edit_action=
          DeleteEdit(Term(new_cursor_term_info.cursor_term_before)),
      )) {
    let rec max_len_term =
            (ls: list(undo_history_entry), cursor_term: cursor_term)
            : cursor_term => {
      switch (ls) {
      | [] => cursor_term
      | [elt] =>
        switch (
          elt.cursor_term_info.cursor_term_before,
          elt.cursor_term_info.cursor_term_after,
        ) {
        | (Exp(_, exp_1), Exp(_, exp_2)) =>
          switch (exp_1, exp_2) {
          | (Var(_, _, _), Var(_, _, _))
          | (IntLit(_, _), IntLit(_, _))
          | (FloatLit(_, _), FloatLit(_, _))
          | (BoolLit(_, _), BoolLit(_, _)) =>
            comp_len_larger(
              cursor_term,
              comp_len_larger(
                elt.cursor_term_info.cursor_term_after,
                elt.cursor_term_info.cursor_term_before,
              ),
            )
          | _ =>
            comp_len_larger(
              cursor_term,
              elt.cursor_term_info.cursor_term_after,
            )
          }
        | (Pat(_, pat_1), Pat(_, pat_2)) =>
          switch (pat_1, pat_2) {
          | (Var(_, _, _), Var(_, _, _))
          | (IntLit(_, _), IntLit(_, _))
          | (FloatLit(_, _), FloatLit(_, _))
          | (BoolLit(_, _), BoolLit(_, _)) =>
            comp_len_larger(
              cursor_term,
              comp_len_larger(
                elt.cursor_term_info.cursor_term_after,
                elt.cursor_term_info.cursor_term_before,
              ),
            )
          | _ =>
            comp_len_larger(
              cursor_term,
              elt.cursor_term_info.cursor_term_after,
            )
          }
        | _ =>
          comp_len_larger(cursor_term, elt.cursor_term_info.cursor_term_after)
        }
      | [head, ...tail] =>
        let larger_term =
          comp_len_larger(
            cursor_term,
            head.cursor_term_info.cursor_term_after,
          );
        max_len_term(tail, larger_term);
      };
    };
    max_len_term(
      [
        ZList.prj_z(prev_group.group_entries),
        ...ZList.prj_suffix(prev_group.group_entries),
      ],
      new_cursor_term_info.cursor_term_before,
    );
  } else {
    new_cursor_term_info.cursor_term_before;
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

let rec get_earlist_entry =
        (ls: list(undo_history_entry)): option(undo_history_entry) => {
  switch (ls) {
  | [] => None
  | [earliest_elt] => Some(earliest_elt)
  | [_, ...tail] => get_earlist_entry(tail)
  };
};

let delete_edit =
    (
      ~prev_group: undo_history_group,
      ~new_cardstacks_before: Cardstacks.t,
      ~new_cursor_term_info: cursor_term_info,
    )
    : option(edit_action) =>
  if (ZExp.erase(new_cursor_term_info.zexp_before)
      != ZExp.erase(new_cursor_term_info.zexp_after)) {
    if (CursorInfo.is_empty_line(new_cursor_term_info.cursor_term_before)) {
      Some(DeleteEdit(EmptyLine));
    } else if (CursorInfo.is_empty_line(
                 new_cursor_term_info.cursor_term_after,
               )
               || CursorInfo.is_hole(new_cursor_term_info.cursor_term_after)) {
      /* delete the whole term */
      let initial_term =
        get_original_deleted_term(
          prev_group,
          new_cardstacks_before,
          new_cursor_term_info,
        );
      Some(DeleteEdit(Term(initial_term)));
    } else {
      let prev_entry = ZList.prj_z(prev_group.group_entries);
      if (!caret_jump(prev_group, new_cardstacks_before)
          && (
            prev_entry.edit_action == Var(Insert)
            || prev_entry.edit_action == Var(DeleteInsert)
          )) {
        Some(Var(DeleteInsert));
      } else {
        Some(Var(Edit));
      };
    };
  } else {
    None;
  };
let delim_edge_handle =
    (~new_cursor_term_info: cursor_term_info, ~adjacent_is_empty_line: bool)
    : option(edit_action) =>
  if (ZExp.erase(new_cursor_term_info.zexp_before)
      != ZExp.erase(new_cursor_term_info.zexp_after)) {
    if (adjacent_is_empty_line) {
      /* delete adjacent empty line */
      Some(DeleteEdit(EmptyLine));
    } else if (CursorInfo.is_hole(new_cursor_term_info.cursor_term_before)) {
      /* delete space */
      Some(DeleteEdit(Space));
    } else {
      None;
          /* jump to next term */
    };
  } else {
    None;
  };
let delete =
    (
      ~prev_group: undo_history_group,
      ~new_cardstacks_before: Cardstacks.t,
      ~new_cursor_term_info: cursor_term_info,
    )
    : option(edit_action) => {
  let cursor_pos = get_cursor_pos(new_cursor_term_info.cursor_term_before);

  switch (cursor_pos) {
  | OnText(_) =>
    if (CursorInfo.caret_is_after_zoperand(new_cursor_term_info.zexp_before)) {
      delim_edge_handle(
        ~new_cursor_term_info,
        ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
      );
    } else {
      //edit var
      delete_edit(
        ~prev_group,
        ~new_cardstacks_before,
        ~new_cursor_term_info,
      );
    }
  | OnDelim(pos, side) =>
    switch (side) {
    | Before =>
      if (CursorInfo.is_hole(new_cursor_term_info.cursor_term_before)
          || ZExp.erase(new_cursor_term_info.zexp_before)
          == ZExp.erase(new_cursor_term_info.zexp_after)) {
        None;
            /* move cursor in the hole */
      } else if (pos == 1
                 && has_typ_ann(new_cursor_term_info.cursor_term_before)) {
        /* num==1 is the position of ':' in an expression */
        Some(
          DeleteEdit(TypeAnn),
        );
      } else {
        /* delete the whole term */
        let initial_term =
          get_original_deleted_term(
            prev_group,
            new_cardstacks_before,
            new_cursor_term_info,
          );
        Some(DeleteEdit(Term(initial_term)));
      }
    | After =>
      delim_edge_handle(
        ~new_cursor_term_info,
        ~adjacent_is_empty_line=new_cursor_term_info.next_is_empty_line,
      )
    }
  | OnOp(side) =>
    switch (side) {
    | Before =>
      /* delete and reach a hole */
      let initial_term =
        get_original_deleted_term(
          prev_group,
          new_cardstacks_before,
          new_cursor_term_info,
        );
      Some(DeleteEdit(Term(initial_term)));
    | After =>
      /* move cursor to next term, just ignore this move */
      None
    }
  };
};

let backspace =
    (
      ~prev_group: undo_history_group,
      ~new_cardstacks_before: Cardstacks.t,
      ~new_cursor_term_info: cursor_term_info,
    )
    : option(edit_action) => {
  let cursor_pos = get_cursor_pos(new_cursor_term_info.cursor_term_before);
  switch (cursor_pos) {
  | OnText(_) =>
    if (CursorInfo.caret_is_before_zoperand(new_cursor_term_info.zexp_before)) {
      delim_edge_handle(
        ~new_cursor_term_info,
        ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
      );
    } else {
      //edit var
      delete_edit(
        ~prev_group,
        ~new_cardstacks_before,
        ~new_cursor_term_info,
      );
    }
  | OnDelim(pos, side) =>
    switch (side) {
    | Before =>
      delim_edge_handle(
        ~new_cursor_term_info,
        ~adjacent_is_empty_line=new_cursor_term_info.prev_is_empty_line,
      )
    | After =>
      if (CursorInfo.is_hole(new_cursor_term_info.cursor_term_before)) {
        None;
            /* move cursor in the hole */
      } else if (pos == 1
                 && has_typ_ann(new_cursor_term_info.cursor_term_before)) {
        /* num==1 is the position of ':' in an expression */
        Some(
          DeleteEdit(TypeAnn),
        );
      } else {
        /* delete the whole term */
        let initial_term =
          get_original_deleted_term(
            prev_group,
            new_cardstacks_before,
            new_cursor_term_info,
          );
        Some(DeleteEdit(Term(initial_term)));
      }
    }
  | OnOp(side) =>
    switch (side) {
    | Before =>
      /* move cursor to next term, just ignore this move */
      None
    | After =>
      /* delete and reach a hole */
      let initial_term =
        get_original_deleted_term(
          prev_group,
          new_cardstacks_before,
          new_cursor_term_info,
        );
      Some(DeleteEdit(Term(initial_term)));
    }
  };
};

let get_new_edit_action =
    (
      ~prev_group: undo_history_group,
      ~new_cardstacks_before: Cardstacks.t,
      ~new_cursor_term_info: cursor_term_info,
      ~action: option(Action.t),
    )
    : option(edit_action) => {
  switch (action) {
  | None => None
  | Some(action') =>
    switch (action') {
    | Delete =>
      delete(~prev_group, ~new_cardstacks_before, ~new_cursor_term_info)
    | Backspace =>
      backspace(~prev_group, ~new_cardstacks_before, ~new_cursor_term_info)
    | Construct(shape) =>
      switch (shape) {
      | SLine =>
        if (ZExp.erase(new_cursor_term_info.zexp_before)
            != ZExp.erase(new_cursor_term_info.zexp_after)) {
          switch (
            CursorInfo.get_outer_zrules(new_cursor_term_info.zexp_before)
          ) {
          | None => Some(ConstructEdit(shape))
          | Some(zrules_before) =>
            switch (
              CursorInfo.get_outer_zrules(new_cursor_term_info.zexp_after)
            ) {
            | None => Some(ConstructEdit(shape))
            | Some(zrules_after) =>
              if (ZList.length(zrules_before) < ZList.length(zrules_after)) {
                Some(MatchRule);
              } else {
                Some(ConstructEdit(shape));
              }
            }
          };
        } else {
          None;
        }
      | SParenthesized
      | SList
      | SAsc
      | SLam
      | SListNil
      | SInj(_)
      | SLet
      | SCase => Some(ConstructEdit(shape))
      | SChar(_) =>
        if (group_entry(
              ~prev_group,
              ~new_cardstacks_before,
              ~new_edit_action=Var(Edit),
            )) {
          switch (get_earlist_entry(ZList.join(prev_group.group_entries))) {
          | None => Some(Var(Insert))
          | Some(earlist_entry) =>
            if (earlist_entry.edit_action == Var(Insert)) {
              Some(Var(Insert));
            } else {
              Some(Var(Edit));
            }
          };
        } else if (CursorInfo.is_hole(new_cursor_term_info.cursor_term_before)
                   || !
                        CursorInfo.cursor_term_is_editable(
                          new_cursor_term_info.cursor_term_before,
                        )) {
          Some(Var(Insert));
        } else {
          Some(Var(Edit));
        }

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
        | SOr => Some(ConstructEdit(shape))
        | SSpace =>
          switch (new_cursor_term_info.cursor_term_before) {
          | Exp(pos, uexp_operand) =>
            switch (uexp_operand) {
            | Var(_, InVarHole(Keyword(k), _), _) =>
              switch (k) {
              | Let =>
                switch (
                  get_cursor_pos(new_cursor_term_info.cursor_term_before)
                ) {
                | OnText(pos) =>
                  if (pos == 3) {
                    /* the caret is at the end of "let" */
                    Some(
                      ConstructEdit(SLet),
                    );
                  } else {
                    Some(ConstructEdit(SOp(SSpace)));
                  }
                | OnDelim(_, _)
                | OnOp(_) => Some(ConstructEdit(SOp(SSpace)))
                }

              | Case =>
                switch (
                  get_cursor_pos(new_cursor_term_info.cursor_term_before)
                ) {
                | OnText(pos) =>
                  if (pos == 4) {
                    /* the caret is at the end of "case" */
                    Some(
                      ConstructEdit(SCase),
                    );
                  } else {
                    Some(ConstructEdit(SOp(SSpace)));
                  }
                | OnDelim(_, _)
                | OnOp(_) => Some(ConstructEdit(SOp(SSpace)))
                }
              }
            | Var(_, _, var) =>
              switch (pos) {
              | OnText(index) =>
                let (left_var, _) = Var.split(index, var);
                if (Var.is_let(left_var)) {
                  Some(ConstructEdit(SLet));
                } else if (Var.is_case(left_var)) {
                  Some(ConstructEdit(SCase));
                } else {
                  Some(ConstructEdit(SOp(SSpace)));
                };
              | OnDelim(_, _)
              | OnOp(_) => Some(ConstructEdit(SOp(SSpace)))
              }

            | ApPalette(_, _, _, _) =>
              failwith("ApPalette is not implemented")
            | _ => Some(ConstructEdit(SOp(SSpace)))
            }
          | _ => Some(ConstructEdit(SOp(SSpace)))
          }
        }

      | SApPalette(_) => failwith("ApPalette is not implemented")
      }
    | MoveTo(_)
    | MoveToBefore(_)
    | MoveLeft
    | MoveRight
    | MoveToNextHole
    | MoveToPrevHole
    | SwapUp /* what's that */
    | SwapDown
    | SwapLeft
    | SwapRight => None
    | UpdateApPalette(_) =>
      failwith("ApPalette is not implemented in undo_history")
    }
  };
};
let get_cursor_info =
    (
      ~new_cardstacks_after: Cardstacks.t,
      ~new_cardstacks_before: Cardstacks.t,
    )
    : cursor_term_info => {
  let zexp_before =
    new_cardstacks_before |> Cardstacks.get_program |> Program.get_zexp;
  let (prev_is_empty_line, next_is_empty_line) =
    CursorInfo.adjacent_is_emptyline(zexp_before);
  let cursor_info_before =
    new_cardstacks_before |> Cardstacks.get_program |> Program.get_cursor_info;
  let cursor_term_before = cursor_info_before.cursor_term;
  let zexp_after =
    new_cardstacks_after |> Cardstacks.get_program |> Program.get_zexp;
  let cursor_info_after =
    new_cardstacks_after |> Cardstacks.get_program |> Program.get_cursor_info;
  let cursor_term_after = cursor_info_after.cursor_term;

  {
    cursor_term_before,
    cursor_term_after,
    zexp_before,
    zexp_after,
    prev_is_empty_line,
    next_is_empty_line,
  };
};

let push_edit_state =
    (
      undo_history: t,
      new_cardstacks_before: Cardstacks.t,
      new_cardstacks_after: Cardstacks.t,
      action: option(Action.t),
    )
    : t => {
  let prev_group = ZList.prj_z(undo_history.groups);
  let new_cursor_term_info =
    get_cursor_info(~new_cardstacks_before, ~new_cardstacks_after);
  let new_edit_action =
    get_new_edit_action(
      ~prev_group,
      ~new_cardstacks_before,
      ~new_cursor_term_info,
      ~action,
    );
  let timestamp = Unix.time();
  switch (new_edit_action) {
  | None =>
    let prev_entry = ZList.prj_z(prev_group.group_entries);
    let new_entry = {
      ...prev_entry,
      caret_jumps_to:
        if (caret_jump(prev_group, new_cardstacks_after)) {
          Some(new_cardstacks_after);
        } else {
          None;
        },
      /* cardstacks: prev_entry.cardstacks new_cardstacks_after, */
    };

    let new_group = {
      ...prev_group,
      group_entries: ZList.replace_z(new_entry, prev_group.group_entries),
    };
    {
      ...undo_history,
      groups: ZList.replace_z(new_group, undo_history.groups),
      disable_auto_scrolling: false,
    };
  | Some(new_edit_action) =>
    let new_entry = {
      cardstacks: new_cardstacks_after,
      caret_jumps_to: None,
      cursor_term_info: new_cursor_term_info,
      previous_action: action,
      edit_action: new_edit_action,
      timestamp,
    };
    if (group_entry(~prev_group, ~new_cardstacks_before, ~new_edit_action)) {
      let new_group =
        push_history_entry(
          ~prev_group,
          ~new_entry,
          ~is_expanded=undo_history.all_hidden_history_expand,
        );
      {
        ...undo_history,
        groups: ([], new_group, ZList.prj_suffix(undo_history.groups)),
        disable_auto_scrolling: false,
        hover_recover_group_id: 0,
        hover_recover_elt_id: 0,
        cur_group_id: 0,
        cur_elt_id: 0,
      };
    } else {
      let new_group = {
        group_entries: ([], new_entry, []),
        is_expanded: undo_history.all_hidden_history_expand,
      };
      {
        ...undo_history,
        groups: (
          [],
          new_group,
          [prev_group, ...ZList.prj_suffix(undo_history.groups)],
        ),
        disable_auto_scrolling: false,
        hover_recover_group_id: 0,
        hover_recover_elt_id: 0,
        cur_group_id: 0,
        cur_elt_id: 0,
      };
    };
  };
};

let update_groups =
    (
      new_groups: ZList.t(undo_history_group, undo_history_group),
      undo_history: t,
    )
    : t => {
  {
    ...undo_history,
    groups: new_groups,
    hover_recover_group_id: List.length(ZList.prj_prefix(new_groups)),
    hover_recover_elt_id:
      List.length(ZList.prj_prefix(ZList.prj_z(new_groups).group_entries)),
    cur_group_id: List.length(ZList.prj_prefix(new_groups)),
    cur_elt_id:
      List.length(ZList.prj_prefix(ZList.prj_z(new_groups).group_entries)),
  };
};
let shift_to_prev = (history: t): t => {
  let cur_group = ZList.prj_z(history.groups);
  /* shift to the previous state in the same group */
  switch (ZList.shift_next(cur_group.group_entries)) {
  | None =>
    /* if current group doesn't have previous state, shfit to the previous group */
    switch (ZList.shift_next(history.groups)) {
    | None => history
    | Some(new_groups) =>
      let new_group = ZList.prj_z(new_groups);
      let new_entries = ZList.shift_begin(new_group.group_entries);

      let new_group' = {
        /* is_expanded=true because the selected group should be expanded */
        group_entries: new_entries,
        is_expanded: true,
      };
      let new_groups = ZList.replace_z(new_group', new_groups);
      update_groups(new_groups, history);
    }
  | Some(new_entries) =>
    let new_group = {
      /* is_expanded=true because the selected group should be expanded */
      group_entries: new_entries,
      is_expanded: true,
    };
    let new_groups = ZList.replace_z(new_group, history.groups);
    update_groups(new_groups, history);
  };
};

let shift_to_next = (history: t): t => {
  let cur_group = ZList.prj_z(history.groups);
  /* shift to the previous state in the same group */
  switch (ZList.shift_prev(cur_group.group_entries)) {
  | None =>
    /* if current group doesn't have previous state, shfit to the previous group */
    switch (ZList.shift_prev(history.groups)) {
    | None => history
    | Some(new_groups) =>
      let new_group = ZList.prj_z(new_groups);
      let new_entries = ZList.shift_end(new_group.group_entries);
      let new_group' = {
        /* is_expanded=true because the selected group should be expanded */
        group_entries: new_entries,
        is_expanded: true,
      };
      let new_groups = ZList.replace_z(new_group', new_groups);
      update_groups(new_groups, history);
    }
  | Some(new_entries) =>
    let new_group = {
      /* is_expanded=true because the selected group should be expanded */
      group_entries: new_entries,
      is_expanded: true,
    };
    let new_groups = ZList.replace_z(new_group, history.groups);
    update_groups(new_groups, history);
  };
};

let shift_history =
    (group_id: int, elt_id: int, is_mouseenter: bool, undo_history: t): t => {
  switch (ZList.shift_to(group_id, undo_history.groups)) {
  | None => failwith("Impossible match, because undo_history is non-empty")
  | Some(new_groups) =>
    let cur_group = ZList.prj_z(new_groups);
    /* shift to the element with elt_id */
    switch (ZList.shift_to(elt_id, cur_group.group_entries)) {
    | None => failwith("Impossible because group_entries is non-empty")
    | Some(new_group_entries) =>
      let (cur_group_id, cur_elt_id) = (group_id, elt_id);
      let (hover_recover_group_id, hover_recover_elt_id) =
        if (is_mouseenter) {
          (
            undo_history.hover_recover_group_id,
            undo_history.hover_recover_elt_id,
          );
        } else {
          (group_id, elt_id);
        };
      {
        ...undo_history,
        groups:
          ZList.replace_z(
            {...cur_group, group_entries: new_group_entries},
            new_groups,
          ),
        disable_auto_scrolling: true,
        hover_recover_group_id,
        hover_recover_elt_id,
        cur_group_id,
        cur_elt_id,
      };
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
