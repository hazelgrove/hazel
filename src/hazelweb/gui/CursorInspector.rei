open Virtual_dom;

/**
 * Typing information at the cursor.
 */
let view:
  (~inject: ModelAction.t => Vdom.Event.t, ~loc: (float, float), Model.t) =>
  Vdom.Node.t;

let ci_control_pane:
  (
    option(Model.cursor_inspector_mode),
    ~inject: ModelAction.t => Vdom.Event.t
  ) =>
  Vdom.Node.t;
