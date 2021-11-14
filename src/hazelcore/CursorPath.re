open Sexplib.Std;

[@deriving sexp]
type steps = list(ChildIndex.t);

[@deriving sexp]
type t = (steps, CursorPosition.t);

[@deriving sexp]
type rev_steps = steps;

[@deriving sexp]
type rev_t = (CursorPosition.t, rev_steps);

[@deriving sexp]
type hole_shape =
  | TypeErr
  | VarErr
  | Empty;

[@deriving sexp]
type hook =
  | TypHole
  | PatHole(MetaVar.t, hole_shape)
  | ExpHole(MetaVar.t, hole_shape)
  | KeywordHook(KeywordID.t);

[@deriving sexp]
type hook_info = {
  hook,
  ap_steps: steps,
  steps,
};

[@deriving sexp]
type hook_list = list(hook_info);

/* two hook lists, one for before the cursor, one for after */
[@deriving sexp]
type zhook_list = {
  hooks_before: hook_list,
  hook_selected: option(hook_info),
  hooks_after: hook_list,
};

let mk_hook = (hook, steps): hook_info => {
  hook,
  ap_steps: steps, /* if this is an ap hole, steps to function pos; otherwise == steps */
  steps,
};

let mk_hook_ap = (hook, steps, ~ap_steps): hook_info => {
  hook,
  ap_steps,
  steps,
};

/* If to_fpos_for_aps is true, then the steps to application errors
   will lead to the function position as opposed to the ap itself.
   This is turned on for cursor movement to holes, as there is no
   cursor position on an ap, but off to draw error hole decorations */
let get_steps =
    (~to_fpos_for_aps: bool=false, {steps, ap_steps, _}: hook_info) =>
  to_fpos_for_aps ? ap_steps : steps;

let get_hook = ({hook, _}: hook_info): hook => hook;

let is_hole: hook_info => bool =
  hi =>
    switch (get_hook(hi)) {
    | TypHole
    | PatHole(_)
    | ExpHole(_) => true
    | KeywordHook(_) => false
    };

let filter_holes_z: zhook_list => zhook_list =
  ({hooks_before, hook_selected, hooks_after}) => {
    hooks_before: List.filter(is_hole, hooks_before),
    hook_selected:
      switch (hook_selected) {
      | None => None
      | Some(h) => is_hole(h) ? Some(h) : None
      },
    hooks_after: List.filter(is_hole, hooks_after),
  };
