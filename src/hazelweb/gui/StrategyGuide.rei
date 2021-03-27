open Virtual_dom;

/**
 * Strategy Guide at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    Settings.CursorInspector.t,
    CursorInfo.t
  ) =>
  Vdom.Node.t;

let extract_vars: (Contexts.t, HTyp.t) => list((Var.t, HTyp.t));

let fun_vars: (Contexts.t, HTyp.t) => list((Var.t, HTyp.t));

let get_type: CursorInfo.t => option(HTyp.t);

let type_to_str: option(HTyp.t) => string;

let list_vars_view: VarCtx.t => list(Vdom.Node.t);
