/* Patchwork sync for undo/redo operations.

   Undo/redo are handled at the History level (swapping Page.Model snapshots),
   which bypasses the normal Editor.Update path where sync_to_parent is called.
   This module bridges that gap by extracting zippers from Page.Model and
   calling the appropriate sync functions. */

open Haz3lcore;

/* Extract the current editor's zipper from Page.Model.
   Only supports Scratch/Documentation modes (used by Patchwork). */
let get_scratch_zipper = (page: Page.Model.t): option(Zipper.t) =>
  switch (page.editors) {
  | Scratch(model)
  | Documentation(model) =>
    let (_, cell) = List.nth(model.scratchpads, model.current);
    Some(cell.editor.editor.state.zipper);
  | Tutorial(_)
  | Exercises(_) => None
  };

/* Sync state and caret to Patchwork after undo/redo.
   Call this after History swaps the Page.Model snapshot. */
let sync = (old_page: Page.Model.t, new_page: Page.Model.t): unit =>
  if (PatchworkComm.is_in_iframe()) {
    switch (get_scratch_zipper(old_page), get_scratch_zipper(new_page)) {
    | (Some(old_z), Some(new_z)) =>
      SyncReplace.sync_for_undo(~old_zipper=old_z, ~new_zipper=new_z)
    | _ => ()
    };
  };
