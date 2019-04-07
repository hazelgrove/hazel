/* A repository stores the history of a codebase, represented as
 * lists of actions (branches) and (as an optimization) historical snapshots of code.
 */

// Represents the state of a Hazel codebase
type edit_state = (ZExp.t, HTyp.t, MetaVarGen.t);

// A branch consists of the raw actions performed, plus periodic snapshots of the state.
type branch = {
  actions: list(Action.t),
  action_count: int,
  snapshots: list(edit_state),
};

type t = {
  // Because we have undo/redo functionality, we need to track multiple points in a history.
  // We therefore borrow Git's notion of branches, and we call the history corresponding to
  // the current undo state the "current branch". Although it may seem that basic undo/redo
  // functionality doesn't require true branches, it turns out that we actually do need branches
  // to preserve the redo stack whilst recording move actions.
  // Whenever an undo operation is performed, an older revision of the history is checked
  // out (branched), and the branch is added to the redo stack so that a redo operation can
  // later be performed if necessary. Future move actions are prepended to this branch.
  // We therefore store a stack of branches, with the current branch at the top.
  // There must always be one (possibly empty) branch in this stack.
  redo_stack: list(branch),
};

let empty: t = {
  redo_stack: [{actions: [], action_count: 0, snapshots: []}],
};

// How many edit actions between snapshots
let snapshot_freq = 100;

// Add a new action to the current branch.
let add = (action: Action.t, edit_state: edit_state, repository: t): t => {
  redo_stack:
    switch (repository.redo_stack) {
    | [branch, ...branches] =>
      let new_count = branch.action_count + 1;
      let updated_branch = {
        actions: [action, ...branch.actions],
        action_count: new_count,
        snapshots:
          if (new_count mod snapshot_freq == 0) {
            let () = print_endline("Made new snapshot");
            [edit_state, ...branch.snapshots];
          } else {
            branch.snapshots;
          },
      };
      if (Action.is_move(action)) {
        [
          // Preserve the redo stack when move actions are performed.
          updated_branch,
          ...branches,
        ];
      } else {
        [
          // Erase the redo stack when edit actions are performed.
          updated_branch,
        ];
      };
    | [] => assert(false)
    },
};

// Uses the given function to execute all the actions in the current branch.
let inform_state =
    (
      set_edit_state: edit_state => unit,
      do_action: Action.t => unit,
      repository: t,
    )
    : unit => {
  switch (repository.redo_stack) {
  | [branch, ..._] =>
    let () =
      switch (branch.snapshots) {
      | [snapshot, ..._] => set_edit_state(snapshot)
      | [] => ()
      };
    let unshapshotted =
      GeneralUtil.take(branch.action_count mod snapshot_freq, branch.actions);
    GeneralUtil.rev_iter(do_action, unshapshotted);
  | [] => ()
  };
};

// Find the revision before the last edit action performed.
let revision_before_last_edit = (branch: branch) => {
  let rec rewind = branch => {
    switch (branch.actions) {
    | [head, ...tail] =>
      let b = {
        actions: tail,
        action_count: branch.action_count - 1,
        snapshots:
          if (branch.action_count mod snapshot_freq == 0) {
            let () = print_endline("Rewound snapshot");
            List.tl(branch.snapshots);
          } else {
            branch.snapshots;
          },
      };
      Action.is_move(head) ? rewind(b) : b;
    | [] => branch
    };
  };
  rewind(branch);
};

// Undoes the last performed edit action, returning the updated repository.
let undo = (repository: t): option(t) => {
  switch (repository.redo_stack) {
  | [branch, ..._] =>
    if (branch.action_count == 0) {
      None;
    } else {
      let new_branch = revision_before_last_edit(branch);
      Some({redo_stack: [new_branch, ...repository.redo_stack]});
    }
  | [] => assert(false)
  };
};

// Redoes the next edit action, returning the updated repository.
let redo = (repository: t): option(t) =>
  switch (repository.redo_stack) {
  | [_, ...branches] =>
    if (branches == []) {
      None;
          // There is only one branch on the stack; there is nothing to redo.
    } else {
      Some({redo_stack: branches});
    }
  | [] => assert(false)
  };
