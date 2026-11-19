open Util;

/* When to scroll the editor to the caret after an action. IfCaretVisible
   scrolls only if the caret was in view before the action: for re-measures
   that move the caret without the user moving it, which should keep a
   visible caret visible but not pull the view back to a caret the user has
   scrolled away from. */
[@deriving (show({with_path: false}), sexp, yojson)]
type scroll =
  | Never
  | Always
  | IfCaretVisible;

/* The stronger of two requests, for merging several actions' results */
let merge_scroll = (a: scroll, b: scroll): scroll =>
  switch (a, b) {
  | (Always, _)
  | (_, Always) => Always
  | (IfCaretVisible, _)
  | (_, IfCaretVisible) => IfCaretVisible
  | (Never, Never) => Never
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = {
  model: 'a,
  is_edit: bool, // Should the editor autosave after this action?
  recalculate: bool, // Should the editor recalculate after this action?
  scroll, // When should the editor scroll to the cursor after this action?
  logged: bool, // Should this action be logged?
  historic: bool // Should this action be undoable? (consumed by History.re to gate undo-stack pushes)
};

let ( let* ) = (updated: t('a), f) => {
  {
    ...updated,
    model: f(updated.model),
  };
};

let return =
    (
      ~is_edit=true,
      ~recalculate=true,
      ~scroll=Always,
      ~logged=true,
      ~historic=true,
      model: 'a,
    ) => {
  {
    model,
    is_edit,
    recalculate,
    scroll,
    logged,
    historic,
  };
};

let return_quiet =
    (
      ~is_edit=false,
      ~recalculate=false,
      ~scroll=Never,
      ~logged=false,
      ~historic=false,
      model: 'a,
    ) => {
  {
    model,
    is_edit,
    recalculate,
    scroll,
    logged,
    historic,
  };
};

exception InvalidAction;

let raise_invalid_action = _ => {
  raise(InvalidAction);
};
