open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = {
  model: 'a,
  is_edit: bool, // Did this action change program semantics? Triggers statics/eval recompute
  save: bool, // Should the editor autosave after this action? (defaults to is_edit)
  recalculate: bool, // Should the editor recalculate after this action?
  scroll_active: bool, // Should the editor scroll to the cursor after this action?
  logged: bool, // Should this action be logged?
  historic: bool, // Should this action be undoable? (consumed by History.re to gate undo-stack pushes)
  /* a historic step that a later update may retract: a new definition's
     insertion, dropped if it's never named */
  tentative: bool,
  /* takes back the tentative step on top of the undo history, as if it
     had never been made */
  retract: bool,
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
      ~save=?,
      ~recalculate=true,
      ~scroll_active=true,
      ~logged=true,
      ~historic=true,
      ~tentative=false,
      ~retract=false,
      model: 'a,
    ) => {
  {
    model,
    is_edit,
    save: Option.value(~default=is_edit, save),
    recalculate,
    scroll_active,
    logged,
    historic,
    tentative,
    retract,
  };
};

let return_quiet =
    (
      ~is_edit=false,
      ~save=?,
      ~recalculate=false,
      ~scroll_active=false,
      ~logged=false,
      ~historic=false,
      model: 'a,
    ) => {
  {
    model,
    is_edit,
    save: Option.value(~default=is_edit, save),
    recalculate,
    scroll_active,
    logged,
    historic,
    tentative: false,
    retract: false,
  };
};

exception InvalidAction;

let raise_invalid_action = _ => {
  raise(InvalidAction);
};
