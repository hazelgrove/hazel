open Virtual_dom;

let root_id: string;
let focus: unit => unit;

/**
 * Code representation of UHExp.
 */
let view:
  (~font_metrics: FontMetrics.t, ~settings: Settings.t, Program.exp) =>
  (Base.list(Vdom.Node.t), list(Vdom.Node.t));

let view_of_box: UHBox.t => Base.list(Vdom.Node.t);
let decoration_views:
  (~font_metrics: FontMetrics.t, UHDecorationPaths.t, UHLayout.t) =>
  list(Vdom.Node.t);
let key_handlers:
  (
    ~settings: Settings.t,
    ~u_gen: MetaVarGen.t,
    ~inject: ModelAction.t => Vdom.Event.t,
    ~is_mac: bool,
    ~cursor_info: CursorInfo.t,
    ~assistant_active: bool
  ) =>
  list(Vdom.Attr.t);

let codebox_view:
  (~font_metrics: FontMetrics.t, Program.Exp.t) => list(Vdom.Node.t);
let typebox_view:
  (
    ~inject: ModelAction.t => Ui_event.t,
    ~font_metrics: FontMetrics.t,
    ~settings: Settings.t,
    ~is_focused: bool=?,
    Program.Typ.t,
    CursorInfo.t,
    MetaVarGen.t
  ) =>
  list(Vdom.Node.t);
