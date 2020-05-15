open Sexplib.Std;

type cursor_term = CursorInfo.cursor_term;

type delete_edit =
  | Term(cursor_term)
  | Space
  | EmptyLine
  | TypeAnn;

type var_edit =
  | Insert
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
  /* mute the auto-scrolling when is_hover is true */
  is_hover: bool,
  show_hover_effect: bool,
  hover_recover_group_id: int,
  hover_recover_elt_id: int,
  cur_group_id: int,
  cur_elt_id: int,
};

let update_is_hover = (is_hover: bool, history: t) => {
  {...history, is_hover};
};
let get_cardstacks = (history: t): Cardstacks.t => {
  ZList.prj_z(ZList.prj_z(history.groups).group_entries).cardstacks;
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
    (~prev_group: undo_history_group, ~new_entry: undo_history_entry)
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

/* return true if cursor jump to another term when new action applied */
let cursor_jump =
    (prev_group: undo_history_group, cardstacks_before: Cardstacks.t): bool => {
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  let cursor_info =
    prev_entry.cardstacks |> Cardstacks.get_program |> Program.get_cursor_info;
  let prev_cardstacks_cursor_term = cursor_info.cursor_term;
  let prev_step =
    prev_entry.cardstacks |> Cardstacks.get_program |> Program.get_steps;
  let new_step =
    cardstacks_before |> Cardstacks.get_program |> Program.get_steps;
  prev_step != new_step
  || prev_cardstacks_cursor_term
  != prev_entry.cursor_term_info.cursor_term_after;
};

/* return true if new edit_action can be grouped with the preivous edit_action */
let group_edit_action =
    (edit_action_1: edit_action, edit_action_2: edit_action): bool =>
  switch (edit_action_1, edit_action_2) {
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
      ~cardstacks_before: Cardstacks.t,
      ~new_edit_action: edit_action,
    )
    : bool => {
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  let can_group_edit_action =
    group_edit_action(prev_entry.edit_action, new_edit_action);

  /* edit actions like construct space/new lines should be grouped together,
     so cursor jump should be ignored in those cases */
  let ignore_cursor_jump =
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
  && (!cursor_jump(prev_group, cardstacks_before) || ignore_cursor_jump);
};

let get_longer_term =
    (cursor_term_next: cursor_term, cursor_term_prev: cursor_term)
    : cursor_term => {
  switch (cursor_term_next, cursor_term_prev) {
  | (Exp(_, exp1), Exp(_, exp2)) =>
    switch (exp1, exp2) {
    | (EmptyHole(_), _) => cursor_term_prev
    | (_, EmptyHole(_)) => cursor_term_next
    | (Var(_, _, var_1), Var(_, _, var_2)) =>
      if (Var.length(var_1) > Var.length(var_2)) {
        cursor_term_next;
      } else {
        cursor_term_prev;
      }
    | (IntLit(_, num_1), IntLit(_, num_2))
    | (FloatLit(_, num_1), FloatLit(_, num_2)) =>
      if (Var.length(num_1) > Var.length(num_2)) {
        cursor_term_next;
      } else {
        cursor_term_prev;
      }
    | (BoolLit(_, _), _)
    | (ListNil(_), _)
    | (Lam(_, _, _, _), _)
    | (Inj(_, _, _), _)
    | (Case(_, _, _, _), _)
    | (Parenthesized(_), _) => cursor_term_next
    | _ => cursor_term_prev
    }
  | (Pat(_, pat1), Pat(_, pat2)) =>
    switch (pat1, pat2) {
    | (EmptyHole(_), _) => cursor_term_prev
    | (_, EmptyHole(_)) => cursor_term_next
    | (Var(_, _, var_1), Var(_, _, var_2)) =>
      if (Var.length(var_1) > Var.length(var_2)) {
        cursor_term_next;
      } else {
        cursor_term_prev;
      }
    | (IntLit(_, num_1), IntLit(_, num_2))
    | (FloatLit(_, num_1), FloatLit(_, num_2)) =>
      if (Var.length(num_1) > Var.length(num_2)) {
        cursor_term_next;
      } else {
        cursor_term_prev;
      }
    | (Wild(_), _)
    | (BoolLit(_, _), _)
    | (ListNil(_), _)
    | (Inj(_, _, _), _)
    | (Parenthesized(_), _) => cursor_term_next
    | _ => cursor_term_prev
    }
  | (Exp(_, exp), Line(_, line)) =>
    switch (line, exp) {
    | (LetLine(_, _, _), _)
    | (ExpLine(_), _) => cursor_term_prev
    | (EmptyLine, _) => cursor_term_next
    }
  | (Pat(_, pat), Line(_, line)) =>
    switch (line, pat) {
    | (LetLine(_, _, _), _)
    | (ExpLine(_), _) => cursor_term_prev
    | (EmptyLine, _) => cursor_term_next
    }
  | (Line(_, line), Exp(_, exp)) =>
    switch (line, exp) {
    | (LetLine(_, _, _), _)
    | (ExpLine(_), _) => cursor_term_next
    | (EmptyLine, _) => cursor_term_prev
    }
  | (Line(_, line), Pat(_, pat)) =>
    switch (line, pat) {
    | (LetLine(_, _, _), _)
    | (ExpLine(_), _) => cursor_term_next
    | (EmptyLine, _) => cursor_term_prev
    }
  | (Line(_, line), Pat(_, pat)) =>
    switch (line, pat) {
    | (LetLine(_, _, _), _)
    | (ExpLine(_), _) => cursor_term_next
    | (EmptyLine, _) => cursor_term_prev
    }
  | (ExpOp(_, _), _)
  | (PatOp(_, _), _)
  | (TypOp(_, _), _)
  | (Rule(_, _), _) => cursor_term_next
  | (_, _) => cursor_term_prev
  };
};

let get_original_deleted_term =
    (group: undo_history_group, new_cursor_term_info: cursor_term_info)
    : cursor_term => {
  let rec max_len_term =
          (ls: list(undo_history_entry), cursor_term: cursor_term)
          : cursor_term => {
    switch (ls) {
    | [] => cursor_term
    | [elt] =>
      get_longer_term(
        cursor_term,
        get_longer_term(
          elt.cursor_term_info.cursor_term_after,
          elt.cursor_term_info.cursor_term_before,
        ),
      )

    | [head, ...tail] =>
      let larger_term =
        get_longer_term(cursor_term, head.cursor_term_info.cursor_term_after);
      max_len_term(tail, larger_term);
    };
  };
  max_len_term(
    [
      ZList.prj_z(group.group_entries),
      ...ZList.prj_suffix(group.group_entries),
    ],
    new_cursor_term_info.cursor_term_before,
  );
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

let delete_edit =
    (~prev_group: undo_history_group, ~new_cursor_term_info: cursor_term_info)
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
        get_original_deleted_term(prev_group, new_cursor_term_info);
      Some(DeleteEdit(Term(initial_term)));
    } else {
      Some
        (Var(Edit));
        /* edit the term */
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
    (~prev_group: undo_history_group, ~new_cursor_term_info: cursor_term_info)
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
      delete_edit(~prev_group, ~new_cursor_term_info);
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
          get_original_deleted_term(prev_group, new_cursor_term_info);
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
        get_original_deleted_term(prev_group, new_cursor_term_info);
      Some(DeleteEdit(Term(initial_term)));
    | After =>
      /* move cursor to next term, just ignore this move */
      None
    }
  };
};

let backspace =
    (~prev_group: undo_history_group, ~new_cursor_term_info: cursor_term_info)
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
      delete_edit(~prev_group, ~new_cursor_term_info);
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
          get_original_deleted_term(prev_group, new_cursor_term_info);
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
        get_original_deleted_term(prev_group, new_cursor_term_info);
      Some(DeleteEdit(Term(initial_term)));
    }
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
let get_new_edit_action =
    (
      ~prev_group: undo_history_group,
      ~cardstacks_before: Cardstacks.t,
      ~new_cursor_term_info: cursor_term_info,
      ~action: option(Action.t),
    )
    : option(edit_action) => {
  switch (action) {
  | None => None
  | Some(action') =>
    switch (action') {
    | Delete => delete(~prev_group, ~new_cursor_term_info)
    | Backspace => backspace(~prev_group, ~new_cursor_term_info)
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
              ~cardstacks_before,
              ~new_edit_action=Var(Edit),
            )) {
          switch (get_earlist_entry(ZList.join(prev_group.group_entries))) {
          | None => Some(Var(Insert))
          | Some(earlist_entry) =>
            if (CursorInfo.is_hole(
                  earlist_entry.cursor_term_info.cursor_term_before,
                )) {
              Some(Var(Insert));
            } else {
              Some(Var(Edit));
            }
          };
        } else if (CursorInfo.is_hole(new_cursor_term_info.cursor_term_before)) {
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
    (~cardstacks_after: Cardstacks.t, ~cardstacks_before: Cardstacks.t)
    : cursor_term_info => {
  let zexp_before =
    ZList.prj_z(ZList.prj_z(cardstacks_before).zcards).program
    |> Program.get_zexp;
  let (prev_is_empty_line, next_is_empty_line) =
    CursorInfo.adjacent_is_emptyline(zexp_before);
  let cursor_info_before =
    cardstacks_before |> Cardstacks.get_program |> Program.get_cursor_info;
  let cursor_term_before = cursor_info_before.cursor_term;
  let zexp_after =
    ZList.prj_z(ZList.prj_z(cardstacks_after).zcards).program
    |> Program.get_zexp;
  let cursor_info_after =
    cardstacks_after |> Cardstacks.get_program |> Program.get_cursor_info;
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
      cardstacks_before: Cardstacks.t,
      cardstacks_after: Cardstacks.t,
      action: option(Action.t),
    )
    : t => {
  let prev_group = ZList.prj_z(undo_history.groups);
  let new_cursor_term_info =
    get_cursor_info(~cardstacks_before, ~cardstacks_after);
  let new_edit_action =
    get_new_edit_action(
      ~prev_group,
      ~cardstacks_before,
      ~new_cursor_term_info,
      ~action,
    );
  let timestamp = Unix.time();
  switch (new_edit_action) {
  | None =>
    let new_entry = {
      ...ZList.prj_z(prev_group.group_entries),
      cardstacks: cardstacks_after,
    };

    let new_group = {
      ...prev_group,
      group_entries: ZList.replace_z(new_entry, prev_group.group_entries),
    };
    {
      ...undo_history,
      groups: ZList.replace_z(new_group, undo_history.groups),
      is_hover: false,
    };
  | Some(new_edit_action) =>
    let new_entry = {
      cardstacks: cardstacks_after,
      cursor_term_info: new_cursor_term_info,
      previous_action: action,
      edit_action: new_edit_action,
      timestamp,
    };
    if (group_entry(~prev_group, ~cardstacks_before, ~new_edit_action)) {
      let new_group = push_history_entry(~prev_group, ~new_entry);
      {
        ...undo_history,
        groups: ([], new_group, ZList.prj_suffix(undo_history.groups)),
        is_hover: false,
        hover_recover_group_id: 0,
        hover_recover_elt_id: 0,
        cur_group_id: 0,
        cur_elt_id: 0,
      };
    } else {
      let new_group = {
        group_entries: ([], new_entry, []),
        is_expanded: false,
      };
      {
        ...undo_history,
        groups: (
          [],
          new_group,
          [prev_group, ...ZList.prj_suffix(undo_history.groups)],
        ),
        is_hover: false,
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
    (
      group_id: int,
      elt_id: int,
      is_click: bool,
      is_mouseenter: bool,
      undo_history: t,
    )
    : t => {
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
        is_hover: !is_click,
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
