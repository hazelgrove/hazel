open Virtual_dom;

/**
 * Strategy Guide at the cursor for expression holes.
 */
let exp_hole_view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    Settings.CursorInspector.t,
    CursorInfo.t
  ) =>
  Vdom.Node.t;

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

/**
 * Strategy Guide at the cursor for rules.
 */
let rules_view: CursorInfo.t => option(Vdom.Node.t);

/**
 * Strategy Guide at the cursor for lines.
 */
let lines_view: unit => Vdom.Node.t;
