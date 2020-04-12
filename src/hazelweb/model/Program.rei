type t;

let mk: (~width: int, Statics.edit_state) => t;

let get_width: t => int;

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
let perform_move_action: (JSUtil.MoveKey.t, t) => t;

exception NodeNotFound;
let move_to_node: (TaggedNodeInstance.kind, MetaVar.t, t) => t;

let get_doc: t => UHDoc.with_splices;
let get_layout: t => UHLayout.with_splices;
let get_decorated_layout: t => UHLayout.with_splices;
let get_cursor_map: t => CursorMap.with_splices;

let cursor_on_inst: t => option((TaggedNodeInstance.kind, MetaVar.t));
