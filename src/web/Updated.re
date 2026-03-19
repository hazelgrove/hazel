open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = {
  model: 'a,
  is_edit: bool, // Should the editor autosave after this action?
  recalculate: bool, // Should the editor recalculate after this action?
  scroll_active: bool, // Should the editor scroll to the cursor after this action?
  logged: bool, // Should this action be logged?
  historic: bool, // Should this action be undoable?
  eval_completed: bool // Did an eval result arrive? (triggers EvalComplete in log)
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
      ~scroll_active=true,
      ~logged=true,
      ~historic=true,
      ~eval_completed=false,
      model: 'a,
    ) => {
  {
    model,
    is_edit,
    recalculate,
    scroll_active,
    logged,
    historic,
    eval_completed,
  };
};

let return_quiet =
    (
      ~is_edit=false,
      ~recalculate=false,
      ~scroll_active=false,
      ~logged=false,
      ~historic=false,
      ~eval_completed=false,
      model: 'a,
    ) => {
  {
    model,
    is_edit,
    recalculate,
    scroll_active,
    logged,
    historic,
    eval_completed,
  };
};

exception InvalidAction;

let raise_invalid_action = _ => {
  raise(InvalidAction);
};
