let u_gen0: MetaVarGen.t = (MetaVarGen.init: MetaVar.t);
let (u, u_gen1) = MetaVarGen.next(u_gen0);
let empty_ze =
  ZExp.CursorE(Before, UHExp.Tm(NotInHole, UHExp.EmptyHole(u)));
let empty_erasure = ZExp.erase(empty_ze);

// Represents the state of a Hazel codebase
type edit_state = (ZExp.t, HTyp.t, MetaVarGen.t);
type edit_state_rs = React.signal(edit_state);
let initial_edit_state: edit_state = (empty_ze, HTyp.Hole, u_gen1);

/* The model contains a repository that stores the history of the codebase, represented as
 * lists of actions (branches) and (as an optimization) historical snapshots of code.
 */

// A branch consists of the raw actions performed, plus periodic snapshots of the state.
type branch = {
  actions: list(Action.t),
  action_count: int,
  snapshots: list(edit_state),
};

// How many edit actions between snapshots
let snapshot_freq = 100;

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
type redo_stack = list(branch);
type redo_stack_rs = React.signal(redo_stack);
let empty_redo_stack: redo_stack = [
  {actions: [], action_count: 0, snapshots: []},
];

type e_rs = React.signal(UHExp.t);
type cursor_info_rs = React.signal(CursorInfo.t);
open Dynamics;
type result_rs =
  React.signal((DHExp.t, DHExp.HoleInstanceInfo.t, Evaluator.result));
type hole_instance_info_rs = React.signal(DHExp.HoleInstanceInfo.t);
module UserSelectedInstances = {
  type t = MetaVarMap.t(inst_num);
  type rs = React.signal(t);
  type rf = (~step: React.step=?, MetaVarMap.t(inst_num)) => unit;
  let update = (usi, inst) => MetaVarMap.insert_or_update(usi, inst);
};
type instance_click_fn = DHExp.HoleInstance.t => unit;
type user_selected_instances_rs = UserSelectedInstances.rs;
type user_selected_instances_rf = UserSelectedInstances.rf;
type selected_instance_rs = React.signal(option(DHExp.HoleInstance.t));
type selected_instance_rf =
  (~step: React.step=?, option(DHExp.HoleInstance.t)) => unit;
type monitors = list(React.signal(unit));
type do_action_t = Action.t => unit;
type replace_e = UHExp.t => unit;

exception InvalidAction;
exception MissingCursorInfo;
exception DoesNotExpand;
exception InvalidInput;

type t = {
  edit_state_rs,
  redo_stack_rs,
  cursor_info_rs,
  e_rs,
  result_rs,
  user_selected_instances_rs,
  user_selected_instances_rf,
  selected_instance_rs,
  selected_instance_rf,
  monitors,
  do_action: do_action_t,
  undo: unit => unit,
  redo: unit => unit,
  replace_e,
};

let new_model = (): t => {
  let (edit_state_rs, edit_state_rf) = React.S.create(initial_edit_state);
  let (redo_stack_rs, redo_stack_rf) = React.S.create(empty_redo_stack);
  let (e_rs, e_rf) = React.S.create(empty_erasure);

  let cursor_info_rs =
    React.S.l1(
      ~eq=(_, _) => false, /* palette contexts have functions in them! */
      ((ze, _, _)) =>
        switch (
          CursorInfo.syn_cursor_info(
            (VarCtx.empty, Palettes.initial_palette_ctx),
            ze,
          )
        ) {
        | Some(cursor_info) => cursor_info
        | None => raise(MissingCursorInfo)
        },
      edit_state_rs,
    );

  let result_rs =
    React.S.l1(
      e => {
        let expanded =
          DHExp.syn_expand(
            (VarCtx.empty, Palettes.initial_palette_ctx),
            Delta.empty,
            e,
          );
        switch (expanded) {
        | DHExp.DoesNotExpand => raise(DoesNotExpand)
        | DHExp.Expands(d, _, _) =>
          switch (Evaluator.evaluate(d)) {
          | Evaluator.InvalidInput(n) =>
            JSUtil.log("InvalidInput " ++ string_of_int(n));
            raise(InvalidInput);
          | Evaluator.BoxedValue(d) =>
            let (d_renumbered, hii) =
              DHExp.renumber([], DHExp.HoleInstanceInfo.empty, d);
            (d_renumbered, hii, Evaluator.BoxedValue(d_renumbered));
          | Evaluator.Indet(d) =>
            let (d_renumbered, hii) =
              DHExp.renumber([], DHExp.HoleInstanceInfo.empty, d);
            (d_renumbered, hii, Evaluator.Indet(d_renumbered));
          }
        };
      },
      e_rs,
    );

  let (user_selected_instances_rs, user_selected_instances_rf) =
    React.S.create(MetaVarMap.empty);

  let usi_monitor =
    React.S.l1(_ => user_selected_instances_rf(MetaVarMap.empty), result_rs);

  let (selected_instance_rs, selected_instance_rf) = React.S.create(None);

  let instance_at_cursor_monitor =
    React.S.l2(
      (
        {
          CursorInfo.mode: _,
          CursorInfo.sort,
          CursorInfo.ctx: _,
          CursorInfo.side: _,
        },
        (_, hii, _),
      ) => {
        let new_path =
          switch (sort) {
          | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.EmptyHole(u))) =>
            let usi = React.S.value(user_selected_instances_rs);
            switch (MetaVarMap.lookup(usi, u)) {
            | Some(i) => Some((u, i))
            | None =>
              switch (DHExp.HoleInstanceInfo.default_instance(hii, u)) {
              | Some(_) as inst => inst
              | None => None
              }
            };
          | _ => None
          };
        selected_instance_rf(new_path);
      },
      cursor_info_rs,
      result_rs,
    );

  let monitors = [instance_at_cursor_monitor, usi_monitor];

  let do_action_unrecorded = action =>
    switch (
      Action.perform_syn(
        (VarCtx.empty, Palettes.initial_palette_ctx),
        action,
        React.S.value(edit_state_rs),
      )
    ) {
    | Some((ze, ty, ugen)) =>
      edit_state_rf((ze, ty, ugen));
      if (!Action.is_move(action)) {
        e_rf(ZExp.erase(ze));
      };
    | None => raise(InvalidAction)
    };

  // Do the specified action, and record it in the current branch.
  let do_action = action => {
    do_action_unrecorded(action);

    // Add a new action to the current branch.
    let add_to_branch =
        (action: Action.t, edit_state: edit_state, redo_stack: redo_stack)
        : redo_stack =>
      switch (redo_stack) {
      | [branch, ...branches] =>
        let new_count = branch.action_count + 1;
        let updated_branch = {
          actions: [action, ...branch.actions],
          action_count: new_count,
          snapshots:
            if (new_count mod snapshot_freq == 0) {
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
      };

    let redo_stack = React.S.value(redo_stack_rs);
    let edit_state = React.S.value(edit_state_rs);
    redo_stack_rf(add_to_branch(action, edit_state, redo_stack));
  };

  let set_edit_state = edit_state => {
    let (ze, _, _) = edit_state;
    edit_state_rf(edit_state);
    e_rf(ZExp.erase(ze));
  };

  let replace_e = new_uhexp => {
    let new_edit_state =
      Action.zexp_syn_fix_holes(
        (VarCtx.empty, PaletteCtx.empty),
        MetaVarGen.init,
        ZExp.CursorE(Before, new_uhexp),
      );
    edit_state_rf(new_edit_state);
    e_rf(new_uhexp);
  };

  // Update the edit state to match all the actions performed in the current branch.
  let update_edit_state = (redo_stack: redo_stack): unit => {
    replace_e(empty_erasure);

    switch (redo_stack) {
    | [branch, ..._] =>
      let () =
        switch (branch.snapshots) {
        | [snapshot, ..._] => set_edit_state(snapshot)
        | [] => ()
        };
      let unshapshotted =
        GeneralUtil.take(
          branch.action_count mod snapshot_freq,
          branch.actions,
        );
      GeneralUtil.rev_iter(do_action_unrecorded, unshapshotted);
    | [] => ()
    };
  };

  // Undoes the last performed edit action, updating the redo_stack.
  let undo = () => {
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

    let redo_stack = React.S.value(redo_stack_rs);

    switch (redo_stack) {
    | [branch, ..._] =>
      if (branch.action_count > 0) {
        let new_branch = revision_before_last_edit(branch);
        let new_stack = [new_branch, ...redo_stack];

        redo_stack_rf(new_stack);
        update_edit_state(new_stack);
      }
    | [] => assert(false)
    };
  };

  // Redoes the next edit action, updating the redo stack.
  let redo = () => {
    let redo_stack = React.S.value(redo_stack_rs);

    switch (redo_stack) {
    | [_, ...branches] =>
      if (branches != []) {
        // If there's not more than one branch, then there's nothing to redo
        let new_stack = branches;

        redo_stack_rf(new_stack);
        update_edit_state(new_stack);
      }
    | [] => assert(false)
    };
  };

  {
    edit_state_rs,
    redo_stack_rs,
    cursor_info_rs,
    e_rs,
    result_rs,
    user_selected_instances_rs,
    user_selected_instances_rf,
    selected_instance_rs,
    selected_instance_rf,
    monitors,
    do_action,
    undo,
    redo,
    replace_e,
  };
};
