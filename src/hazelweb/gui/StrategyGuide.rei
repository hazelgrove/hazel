open Virtual_dom;

/**
 * Strategy Guide at the cursor for expression holes.
 */
let exp_hole_view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~selected_tag_hole: option(MetaVar.t),
    CursorInspectorModel.t,
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
let lines_view: bool => Vdom.Node.t;
