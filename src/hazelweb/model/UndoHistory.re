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
  | Init
  | Ignore; /* cursor move and init state */

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
  recent_non_ignore_group_id: int,
  recent_non_ignore_elt_id: int,
};
let update_id = (recent_non_ignore_group_id: int, recent_non_ignore_elt_id: int, history: t): t => {
  {...history, recent_non_ignore_group_id, recent_non_ignore_elt_id};
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
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  if (new_entry.edit_action == Ignore) {
    /* if edit_action is Ignore, the successory history should not be cleared */
    if (prev_entry.edit_action == Ignore) {
      {
        /* only store 1 cursor-move (Ignore) entry if there are consecutive cursor move actions */
        ...prev_group,
        group_entries: ZList.replace_z(new_entry, prev_group.group_entries),
      };
    } else {
      {
        /* push new Ignore entry into history */

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
      /* push new non-Ignore entry, and clear the successory history entries */

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

let rec get_recent_non_ignore_entry =
        (ls: list(undo_history_entry)): option(undo_history_entry) => {
  switch (ls) {
  | [] => None
  | [head, ...tail] =>
    if (head.edit_action == Ignore) {
      get_recent_non_ignore_entry(tail);
    } else {
      Some(head);
    }
  };
};

let get_recent_non_ignore_entry_index =
    (ls: list(undo_history_entry)): option(int) => {
  let rec helper_func =
          (ls: list(undo_history_entry), index: int): option(int) => {
    switch (ls) {
    | [] => None
    | [head, ...tail] =>
      if (head.edit_action == Ignore) {
        helper_func(tail, index + 1);
      } else {
        Some(index);
      }
    };
  };
  helper_func(ls, 0);
};
/* return true if cursor jump to another term when new action applied */
let cursor_jump =
    (prev_group: undo_history_group, cardstacks_before: Cardstacks.t): bool => {
  switch (
    get_recent_non_ignore_entry([
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

/* return true if new edit_action can be grouped with the preivous edit_action */
let group_edit_action =
    (edit_action_1: edit_action, edit_action_2: edit_action): bool =>
  switch (edit_action_1, edit_action_2) {
  | (Ignore, _)
  | (_, Ignore)
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
    switch (prev_entry.edit_action, new_edit_action) {
    /* ignore entries with "Ignore" edit_action */
    | (Ignore, _) =>
      switch (
        get_recent_non_ignore_entry([
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

  /* edit actions like construct space/new lines should be grouped together,
     so cursor jump should be ignored in those cases */
  let ignore_cursor_jump =
    switch (
      get_recent_non_ignore_entry([
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
      }
    };
  can_group_edit_action
  && (!cursor_jump(prev_group, cardstacks_before) || ignore_cursor_jump);
};

type comp_len_typ =
  | MaxLen
  | Ignore
  | Len(int);

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
    | EmptyHole(_) => Ignore
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
    | Hole => Ignore
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
          cursor_term,
          comp_len_larger(
            elt.cursor_term_info.cursor_term_after,
            elt.cursor_term_info.cursor_term_before,
          ),
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
    : edit_action =>
  if (ZExp.erase(new_cursor_term_info.zexp_before)
      != ZExp.erase(new_cursor_term_info.zexp_after)) {
    if (CursorInfo.is_empty_line(new_cursor_term_info.cursor_term_after)
        || CursorInfo.is_hole(new_cursor_term_info.cursor_term_after)) {
      /* delete the whole term */
      let initial_term =
        get_original_deleted_term(prev_group, new_cursor_term_info);
      DeleteEdit(Term(initial_term));
    } else {
      Var
        (Edit);
        /* edit the term */
    };
  } else {
    Ignore;
  };
let delim_edge_handle =
    (~new_cursor_term_info: cursor_term_info, ~adjacent_is_empty_line: bool)
    : edit_action =>
  if (ZExp.erase(new_cursor_term_info.zexp_before)
      != ZExp.erase(new_cursor_term_info.zexp_after)) {
    if (adjacent_is_empty_line) {
      /* delete adjacent empty line */
      DeleteEdit(EmptyLine);
    } else if (CursorInfo.is_hole(new_cursor_term_info.cursor_term_before)) {
      /* delete space */
      DeleteEdit(Space);
    } else {
      Ignore;
            /* jump to next term */
    };
  } else {
    Ignore;
  };
let delete =
    (~prev_group: undo_history_group, ~new_cursor_term_info: cursor_term_info)
    : edit_action => {
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
        Ignore;
              /* move cursor in the hole */
      } else if (pos == 1
                 && has_typ_ann(new_cursor_term_info.cursor_term_before)) {
        /* num==1 is the position of ':' in an expression */
        DeleteEdit(
          TypeAnn,
        );
      } else {
        /* delete the whole term */
        let initial_term =
          get_original_deleted_term(prev_group, new_cursor_term_info);
        DeleteEdit(Term(initial_term));
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
      DeleteEdit(Term(initial_term));
    | After =>
      /* move cursor to next term, just ignore this move */
      Ignore
    }
  };
};

let backspace =
    (~prev_group: undo_history_group, ~new_cursor_term_info: cursor_term_info)
    : edit_action => {
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
        Ignore;
              /* move cursor in the hole */
      } else if (pos == 1
                 && has_typ_ann(new_cursor_term_info.cursor_term_before)) {
        /* num==1 is the position of ':' in an expression */
        DeleteEdit(
          TypeAnn,
        );
      } else {
        /* delete the whole term */
        let initial_term =
          get_original_deleted_term(prev_group, new_cursor_term_info);
        DeleteEdit(Term(initial_term));
      }
    }
  | OnOp(side) =>
    switch (side) {
    | Before =>
      /* move cursor to next term, just ignore this move */
      Ignore
    | After =>
      /* delete and reach a hole */
      let initial_term =
        get_original_deleted_term(prev_group, new_cursor_term_info);
      DeleteEdit(Term(initial_term));
    }
  };
};

let rec get_earlist_non_ignore_entry =
        (ls: list(undo_history_entry), result: option(undo_history_entry))
        : option(undo_history_entry) => {
  switch (ls) {
  | [] => result
  | [head, ...tail] =>
    if (head.edit_action == Ignore) {
      get_earlist_non_ignore_entry(tail, result);
    } else {
      get_earlist_non_ignore_entry(tail, Some(head));
    }
  };
};

let get_new_edit_action =
    (
      ~prev_group: undo_history_group,
      ~cardstacks_before: Cardstacks.t,
      ~new_cursor_term_info: cursor_term_info,
      ~action: option(Action.t),
    )
    : edit_action => {
  switch (action) {
  | None => Ignore
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
          | None => ConstructEdit(shape)
          | Some(zrules_before) =>
            switch (
              CursorInfo.get_outer_zrules(new_cursor_term_info.zexp_after)
            ) {
            | None => ConstructEdit(shape)
            | Some(zrules_after) =>
              if (ZList.length(zrules_before) < ZList.length(zrules_after)) {
                MatchRule;
              } else {
                ConstructEdit(shape);
              }
            }
          };
        } else {
          Ignore;
        }
      | SParenthesized
      | SList
      | SAsc
      | SLam
      | SListNil
      | SInj(_)
      | SLet
      | SCase => ConstructEdit(shape)
      | SChar(_) =>
        if (group_entry(
              ~prev_group,
              ~cardstacks_before,
              ~new_edit_action=Var(Edit),
            )) {
          switch (
            get_earlist_non_ignore_entry(
              ZList.join(prev_group.group_entries),
              None,
            )
          ) {
          | None => Var(Insert)
          | Some(earlist_non_ignore_entry) =>
            if (CursorInfo.is_hole(
                  earlist_non_ignore_entry.cursor_term_info.cursor_term_before,
                )) {
              Var(Insert);
            } else {
              Var(Edit);
            }
          };
        } else if (CursorInfo.is_hole(new_cursor_term_info.cursor_term_before)) {
          Var(Insert);
        } else {
          Var(Edit);
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
        | SOr => ConstructEdit(shape)
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
                    ConstructEdit(SLet);
                  } else {
                    JSUtil.log("pos!=3");
                    ConstructEdit(SOp(SSpace));
                  }
                | OnDelim(_, _)
                | OnOp(_) => ConstructEdit(SOp(SSpace))
                }

              | Case =>
                switch (
                  get_cursor_pos(new_cursor_term_info.cursor_term_before)
                ) {
                | OnText(pos) =>
                  if (pos == 4) {
                    /* the caret is at the end of "case" */
                    ConstructEdit(
                      SCase,
                    );
                  } else {
                    ConstructEdit(SOp(SSpace));
                  }
                | OnDelim(_, _)
                | OnOp(_) => ConstructEdit(SOp(SSpace))
                }
              }
            | Var(_, _, var) =>
              switch (pos) {
              | OnText(index) =>
                let (left_var, _) = Var.split(index, var);
                if (Var.is_let(left_var)) {
                  ConstructEdit(SLet);
                } else if (Var.is_case(left_var)) {
                  ConstructEdit(SCase);
                } else {
                  ConstructEdit(SOp(SSpace));
                };
              | OnDelim(_, _)
              | OnOp(_) => ConstructEdit(SOp(SSpace))
              }

            | ApPalette(_, _, _, _) =>
              failwith("ApPalette is not implemented")
            | _ => ConstructEdit(SOp(SSpace))
            }
          | _ => ConstructEdit(SOp(SSpace))
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
    | SwapRight => Ignore
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
  let new_entry = {
    cardstacks: cardstacks_after,
    cursor_term_info: new_cursor_term_info,
    previous_action: action,
    edit_action: new_edit_action,
    timestamp,
  };
  if (group_entry(~prev_group, ~cardstacks_before, ~new_edit_action)) {
    let new_group = push_history_entry(~prev_group, ~new_entry);
    if (new_edit_action == Ignore) {
      switch (
        get_recent_non_ignore_entry_index([
          ZList.prj_z(new_group.group_entries),
          ...ZList.prj_suffix(new_group.group_entries),
        ])
      ) {
      | None =>
        failwith(
          "Impossible, Ignore entry must be grouped into a group with non-Ignore entry",
        )
      | Some(index) =>
        {
          ...undo_history,
          groups: ZList.replace_z(new_group, undo_history.groups),
          is_hover: false,
          hover_recover_elt_id:
            index + List.length(ZList.prj_prefix(new_group.group_entries)),
          recent_non_ignore_elt_id:
            index + List.length(ZList.prj_prefix(new_group.group_entries)),
        };
      };
    } else {
      {
        ...undo_history,
        groups: ([], new_group, ZList.prj_suffix(undo_history.groups)),
        is_hover: false,
        hover_recover_group_id: 0,
        hover_recover_elt_id:
          List.length(ZList.prj_prefix(new_group.group_entries)),
        recent_non_ignore_group_id: 0,
        recent_non_ignore_elt_id: List.length(ZList.prj_prefix(new_group.group_entries)),
      };
    };
  } else {
    let new_group = {group_entries: ([], new_entry, []), is_expanded: false};
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
      recent_non_ignore_group_id: 0,
      recent_non_ignore_elt_id: 0,
    };
  };
};

let rec shift_to_prev_non_ignore =
        (history: t, recent_non_ignore_history: t): t => {
  let cur_group = ZList.prj_z(history.groups);
  /* shift to the previous state in the same group */
  switch (ZList.shift_next(cur_group.group_entries)) {
  | None =>
    /* if current group doesn't have previous state, shfit to the previous group */
    switch (ZList.shift_next(history.groups)) {
    | None => recent_non_ignore_history
    | Some(new_groups) =>
      let new_group = ZList.prj_z(new_groups);
      let new_entries = ZList.shift_begin(new_group.group_entries);
      if (ZList.prj_z(new_entries).edit_action == Ignore) {
        let new_history = {...history, groups: new_groups};
        shift_to_prev_non_ignore(new_history, recent_non_ignore_history);
      } else {
        let new_group' = {
          /* is_expanded=true because the selected group should be expanded */
          group_entries: new_entries,
          is_expanded: true,
        };
        let new_groups = ZList.replace_z(new_group', new_groups);
        {
          ...history,
          groups: new_groups,
          hover_recover_group_id: List.length(ZList.prj_prefix(new_groups)),
          hover_recover_elt_id: List.length(ZList.prj_prefix(new_entries)),
          recent_non_ignore_group_id: List.length(ZList.prj_prefix(new_groups)),
          recent_non_ignore_elt_id: List.length(ZList.prj_prefix(new_entries)),
        };
      };
    }
  | Some(new_entries) =>
    if (ZList.prj_z(new_entries).edit_action == Ignore) {
      let new_group = {
        ...cur_group,
        /* is_expanded=true because the selected group should be expanded */
        group_entries: new_entries,
      };
      let new_groups = ZList.replace_z(new_group, history.groups);
      let new_history = {...history, groups: new_groups};
      shift_to_prev_non_ignore(new_history, recent_non_ignore_history);
    } else {
      let new_group = {
        /* is_expanded=true because the selected group should be expanded */
        group_entries: new_entries,
        is_expanded: true,
      };
      let new_groups = ZList.replace_z(new_group, history.groups);
      {
        ...history,
        groups: new_groups,
        hover_recover_group_id: List.length(ZList.prj_prefix(new_groups)),
        hover_recover_elt_id: List.length(ZList.prj_prefix(new_entries)),
        recent_non_ignore_group_id: List.length(ZList.prj_prefix(new_groups)),
        recent_non_ignore_elt_id: List.length(ZList.prj_prefix(new_entries)),
      };
    }
  };
};

let shift_to_prev = (history: t): t => {
  shift_to_prev_non_ignore(history, history);
};

let rec shift_to_next_non_ignore =
        (history: t, recent_non_ignore_history: t): t => {
  let cur_group = ZList.prj_z(history.groups);
  /* shift to the previous state in the same group */
  switch (ZList.shift_prev(cur_group.group_entries)) {
  | None =>
    /* if current group doesn't have previous state, shfit to the previous group */
    switch (ZList.shift_prev(history.groups)) {
    | None => recent_non_ignore_history
    | Some(new_groups) =>
      let new_group = ZList.prj_z(new_groups);
      let new_entries = ZList.shift_end(new_group.group_entries);
      if (ZList.prj_z(new_entries).edit_action == Ignore) {
        let new_history = {...history, groups: new_groups};
        shift_to_next_non_ignore(new_history, recent_non_ignore_history);
      } else {
        let new_group' = {
          /* is_expanded=true because the selected group should be expanded */
          group_entries: new_entries,
          is_expanded: true,
        };
        let new_groups = ZList.replace_z(new_group', new_groups);
        {
          ...history,
          groups: new_groups,
          hover_recover_group_id: List.length(ZList.prj_prefix(new_groups)),
          hover_recover_elt_id: List.length(ZList.prj_prefix(new_entries)),
          recent_non_ignore_group_id: List.length(ZList.prj_prefix(new_groups)),
          recent_non_ignore_elt_id: List.length(ZList.prj_prefix(new_entries)),
        };
      };
    }
  | Some(new_entries) =>
    if (ZList.prj_z(new_entries).edit_action == Ignore) {
      let new_group = {
        ...cur_group,
        /* is_expanded=true because the selected group should be expanded */
        group_entries: new_entries,
      };
      let new_groups = ZList.replace_z(new_group, history.groups);
      let new_history = {
        ...history,
        groups: new_groups,
        hover_recover_group_id: List.length(ZList.prj_prefix(new_groups)),
        hover_recover_elt_id: List.length(ZList.prj_prefix(new_entries)),
        recent_non_ignore_group_id: List.length(ZList.prj_prefix(new_groups)),
        recent_non_ignore_elt_id: List.length(ZList.prj_prefix(new_entries)),
      };
      shift_to_next_non_ignore(new_history, recent_non_ignore_history);
    } else {
      let new_group = {
        /* is_expanded=true because the selected group should be expanded */
        group_entries: new_entries,
        is_expanded: true,
      };
      let new_groups = ZList.replace_z(new_group, history.groups);
      {
        ...history,
        groups: new_groups,
        hover_recover_group_id: List.length(ZList.prj_prefix(new_groups)),
        hover_recover_elt_id: List.length(ZList.prj_prefix(new_entries)),
        recent_non_ignore_group_id: List.length(ZList.prj_prefix(new_groups)),
        recent_non_ignore_elt_id: List.length(ZList.prj_prefix(new_entries)),
      };
    }
  };
};

let shift_to_next = (history: t): t => {
  shift_to_next_non_ignore(history, history);
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
