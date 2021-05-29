open Virtual_dom;

/**
 * Strategy Guide at the cursor for pattern holes in case expressions.
 */
let pat_hole_view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    Settings.CursorInspector.t,
    CursorInfo.t
  ) =>
  Vdom.Node.t;

/**
 * Strategy Guide at the cursor for variable holes (patterns holes in lets and lambdas).
 */
let var_hole_view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    Settings.CursorInspector.t,
    CursorInfo.t
  ) =>
  Vdom.Node.t;
