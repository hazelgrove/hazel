open Virtual_dom;

/**
 * Typing information at the cursor.
 */
let view:
  (~inject: ModelAction.t => Vdom.Event.t, Model.t, (float, float)) =>
  //Settings.CursorInspector.t,
  //CursorInfo.t,
  //MetaVarGen.t
  Vdom.Node.t;
