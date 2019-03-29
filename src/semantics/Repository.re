/* A repository stores the history of a codebase, represented as
 * lists of actions and (as an optimization) historical snapshots of code.
 */

type actionHistory = list(Action.t);

type t = {
  // Because we have undo/redo functionality, we have a notion of an "active" history,
  // i.e. the history corresponding to the current undo state. When an undo operation
  // is performed, an older version of the history is created, which must be allocated
  // separately so that a redo operation can later be performed if necessary.
  // We therefore store a stack of histories, with the active history at the top.
  // There must always be one (possibly empty) history in this stack.
  redo_stack: list(actionHistory),
  // We store snapshots of the code every X actions to enable rapid undo.
  code_snapshots: list(ZExp.t),
};

let empty: t = {redo_stack: [[]], code_snapshots: []};

// Add a new action to the active history.
let add = (action: Action.t, repository: t): t => {
  ...repository,
  redo_stack:
    switch (repository.redo_stack) {
    | [history, ...histories] =>
      if (Action.is_move(action)) {
        [
          // Preserve the redo stack when move actions are performed.
          [action, ...history],
          ...histories,
        ];
      } else {
        [
          // Erase the redo stack when edit actions are performed.
          [action, ...history],
        ];
      }
    | [] => assert(false)
    },
};

// Uses the given function to execute all the actions in the active history.
let execute_actions = (do_action: Action.t => unit, repository: t) => {
  GeneralUtil.rev_iter(
    do_action,
    switch (repository.redo_stack) {
    | [history, ..._] => history
    | [] => assert(false)
    },
  );
};

// Find the most recent edit action performed.
let rewind_to_last_edit = actions => {
  let rec rewind = actions => {
    switch (actions) {
    | [head, ...tail] => Action.is_move(head) ? rewind(tail) : tail
    | [] => []
    };
  };
  rewind(actions);
};

// Undoes the last performed action, returning the updated repository.
let undo = (repository: t): option(t) => {
  switch (repository.redo_stack) {
  | [history, ..._] =>
    if (history == []) {
      None;
    } else {
      let new_history = rewind_to_last_edit(history);
      Some({
        ...repository,
        redo_stack: [new_history, ...repository.redo_stack],
      });
    }
  | [] => assert(false)
  };
};

// Redoes the next action, returning the updated repository.
let redo = (repository: t): option(t) =>
  switch (repository.redo_stack) {
  | [_, ...histories] =>
    if (histories == []) {
      None;
          // There is only one history on the stack; there is nothing to redo.
    } else {
      Some({...repository, redo_stack: histories});
    }
  | [] => assert(false)
  };
