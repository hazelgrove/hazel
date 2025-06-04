type t('a) = {
  model: 'a,
  is_edit: bool, // Should the editor autosave after this action?
  recalculate: bool, // Should the editor recalculate after this action?
  scroll_active: bool, // Should the editor scroll to the cursor after this action?
  logged: bool // Should this action be logged?
};

let ( let* ) = (updated: t('a), f) => {
  {...updated, model: f(updated.model)};
};

let return =
    (
      ~is_edit=true,
      ~recalculate=true,
      ~scroll_active=true,
      ~logged=true,
      model: 'a,
    ) => {
  {model, is_edit, recalculate, scroll_active, logged};
};

let return_quiet =
    (
      ~is_edit=false,
      ~recalculate=false,
      ~scroll_active=false,
      ~logged=false,
      model: 'a,
    ) => {
  {model, is_edit, recalculate, scroll_active, logged};
};
