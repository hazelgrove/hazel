type t;

let mk: Statics.edit_state => t;

let get_edit_state: t => Statics.edit_state;
let put_edit_state: (Statics.edit_state, t) => t;

let get_zexp: t => ZExp.t;
let get_uhexp: t => UHExp.t;

let get_path: t => CursorPath.t;
let get_steps: t => CursorPath.steps;

let get_u_gen: t => MetaVarGen.t;

exception MissingCursorInfo;
let get_cursor_info: t => CursorInfo.t;

exception DoesNotExpand;
let get_expansion: t => DHExp.t;

exception InvalidInput;
let get_result: t => Result.t;

exception FailedAction;
exception CursorEscaped;
let perform_edit_action: (Action.t, t) => t;

exception HoleNotFound;
let move_to_hole: (MetaVar.t, t) => t;

let get_doc: t => UHDoc.t;

let cursor_on_exp_hole: t => option(MetaVar.t);
