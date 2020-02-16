type cursor_term = CursorInfo.cursor_term;
type structure =
  | LetBinding
  | Lamda
  | CaseMatch
  | TypeAnn
type edit_action =
  | DeleteToHole(int)
  | InsertToHole(int)
  | DeleteToNotHole
  | DeleteHole(int)
  | DeleteEmptyLine
  | DeleteEdit(cursor_term)
  | InsertEdit(cursor_term)
  | Construct(structure)
  | DeleteStructure(structure)
  | DeleteTypeAnn
  | MoveCursor
  | NotSet

type info = {
  previous_action: Action.t,
  previous_cursor_term: cursor_term,
  current_cursor_term: cursor_term,
  prev_is_empty_line: bool,
  edit_action,
}
type undo_history_entry = {
  cardstacks: Cardstacks.t,
  info: option(cursor_info);
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


let get_cursor_info =
    (cardstacks: Cardstacks.t): (option(cursor_term), bool) => {
  let zexp =
    ZList.prj_z(ZList.prj_z(cardstacks).zcards).program |> Program.get_zexp;
  CursorInfo.extract_cursor_term(zexp);
};

let undoable_action = (action: option(Action.t)): bool => {
  switch (action) {
  | None =>
    failwith(
      "Impossible match. None of None-action will be pushed into history",
    )
  | Some(action') =>
    switch (action') {
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
    }
  };
};
let get_last_history_entry = (group: undo_history_group):undo_history_entry => {
  switch(ZList.prj_suffix(group.group_entries)){
  | [] => ZList.prj_z(group.group_entries)
  | [head,..tail] => head;
  }
}
let get_cursor_pos = (cursor_term:cursor_term): CursorPosition.t => {
  switch (cursor_term) {
    | Exp(cursor_pos, _)
    | Pat(cursor_pos, _)
    | Typ(cursor_pos, _)
    | ExpOp(cursor_pos, _)
    | PatOp(cursor_pos, _)
    | TypOp(cursor_pos, _)
    | Line(cursor_pos, _)
    | Rule(cursor_pos, _) => cursor_pos
    }
}
let can_delete_typ_ann = cursor_term => {
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
  }
};
let push_history_entry = (prev_group: undo_history_group, new_entry:undo_history_entry): undo_history_group =>{
  (
    [],
    new_entry,
    [
      ZList.prj_z(prev_group.group_entries),
      ...ZList.prj_suffix(prev_group.group_entries),
    ],
  );
}
let set_edit_action_of_entry = (entry:undo_history_entry, edit_action):undo_history_entry => {
  switch(entry.info){
  | None => entry
  | Some(entry_info) => {
    ...entry
    info: {
      ...entry_info,
      edit_action: edit_action,
    }
  } 
  }
}

let cursor_jump_after_delete = (cursor_pos1:CursorPosition.t, cursor_pos2:CursorPosition.t):bool => {
  switch(cursor_pos1){
    | OnText(pos1) => {
      if(pos1!=0) {
        switch(cursor_pos2){
          | OnText(pos2) => pos2==0
          | OnDelim(_, side) =>
            switch(side){
            | Before => true
            | After => failwith("impossible jump")
            }
          | OnOp(side) =>
            switch(side){
              | Before => true
              | After => failwith("impossible jump")
              } 
        }
      } else false;
    }
    | OnDelim(_, side) =>
      switch(side){
      | Before => false
      | After => true
      }
    | OnOp(side) => {
      switch(side){
        | Before => false
        | After => true
        }
    }
  }
};
let cursor_jump_after_backspace = (cursor_pos1:CursorPosition.t, cursor_pos2:CursorPosition.t):bool => {
  switch(cursor_pos1){
    | OnText(pos1) => {
      if(pos1==0) {
        switch(cursor_pos2){
          | OnText(_) => true
          | OnDelim(_, side) =>
            switch(side){
            | Before => failwith("impossible jump")
            | After => true
            }
          | OnOp(side) =>
            switch(side){
              | Before => failwith("impossible jump")
              | After => true
              } 
        }
      } else false;
    }
    | OnDelim(_, side) =>
      switch(side){
      | Before => true
      | After => false
      }
    | OnOp(side) => {
      switch(side){
        | Before => true
        | After => false
        }
    }
  }
};
/* let single_hole_removed = (prev_entry_info:info, action: Action.t): option(int) => {
  switch(CursorInfo.is_hole(prev_entry_info.current_cursor_term)){
  | None => None
  | Some(hole_id) => {
    let prev_cursor_pos = get_cursor_pos(prev_entry_info.current_cursor_term);
    switch(prev_cursor_pos){
    | OnDelim(_, side) => 
      switch(side){
      | Before => if(action == Backspace) Some(hole_id) else None; 
      | After => if(action == Delete) Some(hole_id) else None; 
      }
    | OnText(_)
    | OnOp(_) => failwith("Impossible, hole only has delim cursor position")
    }
  }
  }
} */

type group_result =
  | Success(undo_history_group)
  | Fail(undo_history_group,undo_history_entry)

let join_group = (prev_group: undo_history_group, new_entry:undo_history_entry): group_result => {
  let prev_last_entry = get_last_history_entry(prev_group);
  switch(prev_last_entry.info, new_entry.info){
    | (None, _)
    | (_, None) => {
      let prev_group' = {
        ...prev_group,new_entry
        is_complete: true,
      };
      Fail(prev_group', new_entry);
    }
    | (Some(prev_entry_info),Some(new_entry_info)) => {
      switch(prev_entry_info.previous_action, new_entry_info.previous_action) {
        | (Delete, Delete) => {
          let prev_cursor_pos = get_cursor_pos(prev_entry_info.current_cursor_term);
          let new_cursor_pos = get_cursor_pos(new_entry_info.current_cursor_term);
          switch(prev_cursor_pos){
            | OnText(_) => {
              if(cursor_jump_after_delete(prev_cursor_pos,new_cursor_pos)){
                /* jump to next term */
                let prev_group' = {
                  ...prev_group,
                  is_complete: true,
                };
                let new_entry' = {
                  ...new_entry,
                  info: None,
                };
                Success(push_history_entry(prev_group',new_entry'));
              } else {
                /* normal edit */
                let new_entry' = set_edit_action_of_entry(new_entry,DeleteEdit(new_entry_info.current_cursor_term));
                Success(push_history_entry(prev_group,new_entry'));
              }
            }
            | OnDelim(num, side) =>
              switch (side) {
              | Before => {
                switch(CursorInfo.is_hole(prev_entry_info.current_cursor_term)){
                | Some(_) => {
                  /* move cursor in the hole */
                  let new_entry' = {
                    ...new_entry,
                    info: None,
                  };
                  Success(push_history_entry(prev_group,new_entry'));
                }
                | None => {
                  if (num == 1 && can_delete_typ_ann(prev_entry_info.current_cursor_term)) {
                    /* num==1 is the position of ':' in an expression */
                  
                      let prev_group' = {
                        ...prev_group,
                        is_complete: true,
                      };
                      let new_entry' = set_edit_action_of_entry(new_entry, DeleteTypeAnn);
                      Fail(prev_group', new_entry');
                  } else {
                    switch(CursorInfo.is_hole(new_entry_info.current_cursor_term)){
                      | Some(hole_id) => {
                        /* delete and reach a hole */
                        let new_entry' = set_edit_action_of_entry(new_entry,DeleteToHole(hole_id));
                        Success(push_history_entry(prev_group,new_entry'));
                      }
                      | None => {
                        /* delete and not reach a hole */
                        let prev_group' = {
                          ...prev_group,
                          is_complete: true,
                        };
                        let new_entry' = set_edit_action_of_entry(new_entry,DeleteToNotHole);
                        Success(push_history_entry(prev_group',new_entry'));
                      }
                      }
                  }

                }
                }                   
              }
              | After =>
                switch(CursorInfo.is_hole(prev_entry_info.current_cursor_term)){
                | Some(hole_id) => {
                  let prev_group' = {
                    ...prev_group,
                    is_complete: true,
                  };
                  let new_entry' = set_edit_action_of_entry(new_entry, DeleteHole(hole_id));
                  Fail(prev_group', new_entry');
                }
                | None => {
                  /* move cursor to next term, just ignore this move */
                  let prev_group' = {
                    ...prev_group,
                    is_complete: true,
                  };
                  let new_entry' = {
                    ...new_entry,
                    info: None,
                  };
                  Success(push_history_entry(prev_group',new_entry'));
                }
                }
              
              }
            | OnOp(side) =>
              switch (side) {
              | Before => {
                switch(CursorInfo.is_hole(new_entry_info.current_cursor_term)){
                  | Some(hole_id) => {
                    /* delete and reach a hole */
                    let new_entry' = set_edit_action_of_entry(new_entry,DeleteToHole(hole_id));
                    Success(push_history_entry(prev_group,new_entry'));
                  }
                  | None => {
                    /* delete and not reach a hole */
                    let prev_group' = {
                      ...prev_group,
                      is_complete: true,
                    };
                    let new_entry' = set_edit_action_of_entry(new_entry,DeleteToNotHole);
                    Success(push_history_entry(prev_group',new_entry'));
                  }
                }
              }
              | After =>{
                /* move cursor to next term, just ignore this move */
                let prev_group' = {
                  ...prev_group,
                  is_complete: true,
                };
                let new_entry' = {
                  ...new_entry,
                  info: None,
                };
                Success(push_history_entry(prev_group',new_entry'));
              } 
              }
          }
        }
        | (Backspace, Backspace) => {
          let prev_cursor_pos = get_cursor_pos(prev_entry_info.current_cursor_term);
          switch(prev_cursor_pos){
            | OnText(_) => {
              if(cursor_jump_after_backspace(prev_cursor_pos,new_cursor_pos)){
                /* jump to next term */
                let prev_group' = {
                  ...prev_group,
                  is_complete: true,
                };
                let new_entry' = {
                  ...new_entry,
                  info: None,
                };
                Success(push_history_entry(prev_group',new_entry'));
              } else {
                /* normal edit */
                let new_entry' = set_edit_action_of_entry(new_entry,DeleteEdit(new_entry_info.current_cursor_term));
                Success(push_history_entry(prev_group,new_entry'));
              }
            }
            | OnDelim(num, side) =>
              switch (side) {
              | Before =>
                switch(CursorInfo.is_hole(prev_entry_info.current_cursor_term)){
                  | Some(hole_id) => {
                    if(prev_entry_info.prev_is_empty_line){
                      /* whether delete the previous empty line */
                      switch(prev_entry_info.edit_action){
                        | DeleteEmptyLine => {
                          let new_entry' = set_edit_action_of_entry(new_entry,DeleteEmptyLine);
                          Success(push_history_entry(prev_group',new_entry'));
                        }
                        | DeleteToHole(_)
                        | InsertToHole(_)
                        | DeleteToNotHole
                        | DeleteHole(_)
                        | DeleteEdit(_)
                        | InsertEdit(_)
                        | Construct(_)
                        | DeleteStructure(_)
                        | DeleteTypeAnn
                        | MoveCursor
                        | NotSet  => {
                            let prev_group' = {
                              ...prev_group,
                              is_complete: true,
                            };
                            let new_entry' = set_edit_action_of_entry(new_entry,DeleteEmptyLine);
                            Fail(prev_group',new_entry');
                          }
                        
                      }
                    } else {
                      let prev_group' = {
                        ...prev_group,
                        is_complete: true,
                      };
                      let new_entry' = set_edit_action_of_entry(new_entry, DeleteHole(hole_id));
                      Fail(prev_group', new_entry');
                    }
                  }
                  | None => {
                    /* move cursor to next term, just ignore this move */
                    let prev_group' = {
                      ...prev_group,
                      is_complete: true,
                    };
                    let new_entry' = {
                      ...new_entry,
                      info: None,
                    };
                    Success(push_history_entry(prev_group',new_entry'));
                  }
                  }
              
              | After =>{
                switch(CursorInfo.is_hole(prev_entry_info.current_cursor_term)){
                | Some(_) => {
                  /* move cursor in the hole */
                  let new_entry' = {
                    ...new_entry,
                    info: None,
                  };
                  Success(push_history_entry(prev_group,new_entry'));
                }
                | None => {
                  if (num == 1 && can_delete_typ_ann(prev_entry_info.current_cursor_term)) {
                    /* num==1 is the position of ':' in an expression */
                  
                      let prev_group' = {
                        ...prev_group,
                        is_complete: true,
                      };
                      let new_entry' = set_edit_action_of_entry(new_entry, DeleteTypeAnn);
                      Fail(prev_group', new_entry');
                  } else {
                    switch(CursorInfo.is_hole(new_entry_info.current_cursor_term)){
                      | Some(hole_id) => {
                        /* delete and reach a hole */
                        let new_entry' = set_edit_action_of_entry(new_entry,DeleteToHole(hole_id));
                        Success(push_history_entry(prev_group,new_entry'));
                      }
                      | None => {
                        /* delete and not reach a hole */
                        let prev_group' = {
                          ...prev_group,
                          is_complete: true,
                        };
                        let new_entry' = set_edit_action_of_entry(new_entry,DeleteToNotHole);
                        Success(push_history_entry(prev_group',new_entry'));
                      }
                      }
                  }

                }
                }                   
              }
                
              
              }
            | OnOp(side) =>
              switch (side) {
              | Before => {
                /* move cursor to next term, just ignore this move */
                let prev_group' = {
                  ...prev_group,
                  is_complete: true,
                };
                let new_entry' = {
                  ...new_entry,
                  info: None,
                };
                Success(push_history_entry(prev_group',new_entry'));
              } 
              | After => {
                switch(CursorInfo.is_hole(new_entry_info.current_cursor_term)){
                  | Some(hole_id) => {
                    /* delete and reach a hole */
                    let new_entry' = set_edit_action_of_entry(new_entry,DeleteToHole(hole_id));
                    Success(push_history_entry(prev_group,new_entry'));
                  }
                  | None => {
                    /* delete and not reach a hole */
                    let prev_group' = {
                      ...prev_group,
                      is_complete: true,
                    };
                    let new_entry' = set_edit_action_of_entry(new_entry,DeleteToNotHole);
                    Success(push_history_entry(prev_group',new_entry'));
                  }
                }
              }
              }
          }
        }
        | (Construct(shape_1), Construct(shape_2)) => {
          switch
        }
        | (UpdateApPalette(_), _) =>
          failwith("ApPalette is not implemented in undo_history")
        | (Delete, _)
        | (Backspace, _)
        | (Construct(_), _) => false
        | (MoveTo(_), _)
        | (MoveToBefore(_), _)
        | (MoveLeft, _)
        | (MoveRight, _)
        | (MoveToNextHole, _)
        | (MoveToPrevHole, _) =>
          failwith(
            "Impossible match. Not undoable actions will not be added into history",
          ) 
      }
    }
  }

}
let in_same_history_group =
    (~prev_entry: undo_history_entry, ~cur_entry: undo_history_entry): bool => {
  switch (prev_entry.previous_action, cur_entry.previous_action) {
  | (None, _)
  | (_, None) => false
  | (Some(detail_action_1), Some(detail_action_2)) =>
    switch (detail_action_1, detail_action_2) {
    | (Delete, Delete)
    | (Backspace, Backspace) =>
      CursorInfo.can_group_cursor_term(
        prev_entry.current_cursor_term,
        cur_entry.current_cursor_term,
      )
    | (Construct(shape_1), Construct(shape_2)) =>
      /* if shapes are similar, then continue to check if they have similar cursor_term */
      if (Action.can_group_shape(shape_1, shape_2)) {
        CursorInfo.can_group_cursor_term(
          prev_entry.current_cursor_term,
          cur_entry.current_cursor_term,
        );
      } else {
        false;
      }
    | (UpdateApPalette(_), _) =>
      failwith("ApPalette is not implemented in undo_history")
    | (Delete, _)
    | (Backspace, _)
    | (Construct(_), _) => false
    | (MoveTo(_), _)
    | (MoveToBefore(_), _)
    | (MoveLeft, _)
    | (MoveRight, _)
    | (MoveToNextHole, _)
    | (MoveToPrevHole, _) =>
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
      action: option(Action.t),
    )
    : t => {
  let prev_group = ZList.prj_z(undo_history);
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  if (undoable_action(action)) {
    let (prev_cursor_term, _) = get_cursor_info(prev_cardstacks);
    let (cur_cursor_term, prev_is_empty_line) =
      get_cursor_info(cur_cardstacks);
    let cur_entry = {
      cardstacks: cur_cardstacks,
      previous_action: action,
      previous_cursor_term: prev_cursor_term,
      current_cursor_term: cur_cursor_term,
      prev_is_empty_line,
    };
    if (!prev_group.is_complete
        && in_same_history_group(~prev_entry, ~cur_entry)) {
      /* group the new entry into the current group */
      let group_entries_after_push = (
        [],
        cur_entry,
        [
          ZList.prj_z(prev_group.group_entries),
          ...ZList.prj_suffix(prev_group.group_entries),
        ],
      );
      (
        [],
        {
          group_entries: group_entries_after_push,
          is_expanded: false,
          is_complete: false,
        }, /* initial expanded-state of a group should be folded*/
        ZList.prj_suffix(undo_history),
      );
    } else {
      /* start a new group */
      let new_group = {
        group_entries: ([], cur_entry, []),
        is_expanded: false,
        is_complete: false,
      };
      (
        [],
        new_group,
        [ZList.prj_z(undo_history), ...ZList.prj_suffix(undo_history)],
      );
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
