open Virtual_dom;
module Js = Js_of_ocaml.Js;

let focus: Model.editor => unit;

let view_of_box: UHBox.t => Base.list(Vdom.Node.t);
let decoration_views:
  (~font_metrics: FontMetrics.t, UHDecorationPaths.t, UHLayout.t) =>
  list(Vdom.Node.t);
let key_handlers:
  (~inject: ModelAction.t => Vdom.Event.t, ~model: Model.t) =>
  list(Vdom.Attr.t);
let click_to_move:
  (
    string,
    FontMetrics.t,
    {
      ..
      "clientX": Js.gen_prop({.. get: int}),
      "clientY": Js.gen_prop({.. get: int}),
    }
  ) =>
  ModelAction.t;

let view_syntax:
  (
    ~settings: Settings.t,
    ~font_metrics: FontMetrics.t,
    ~is_focused: bool,
    TermSort.syntax
  ) =>
  list(Vdom.Node.t);

let exp_view:
  (
    ~settings: Settings.t,
    ~font_metrics: FontMetrics.t,
    ~is_focused: bool,
    Editor.exp
  ) =>
  list(Vdom.Node.t);

let filter_typ_view:
  (
    ~inject: ModelAction.t => Ui_event.t,
    ~font_metrics: FontMetrics.t,
    ~settings: Settings.t,
    ~model: Model.t,
    ~is_focused: bool,
    Editor.Typ.t,
    MetaVarGen.t
  ) =>
  list(Vdom.Node.t);
