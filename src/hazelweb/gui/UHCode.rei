open Virtual_dom;
module Js = Js_of_ocaml.Js;

let focus: Model.editor => unit;

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
let click_handler:
  (
    string,
    FontMetrics.t,
    ModelAction.t => Ui_event.t,
    {
      ..
      "clientX": Js.gen_prop({.. get: int}),
      "clientY": Js.gen_prop({.. get: int}),
    }
  ) =>
  Ui_event.t;

let codebox_view:
  (
    ~settings: Settings.t,
    ~font_metrics: FontMetrics.t,
    ~is_focused: bool,
    Program.exp
  ) =>
  list(Vdom.Node.t);
let typebox:
  (
    ~inject: ModelAction.t => Ui_event.t,
    ~font_metrics: FontMetrics.t,
    ~is_mac: bool,
    ~settings: Settings.t,
    ~is_focused: bool,
    Program.Typ.t,
    MetaVarGen.t
  ) =>
  list(Vdom.Node.t);
