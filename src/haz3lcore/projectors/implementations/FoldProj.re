open Util;
open ProjectorBase;
open ProjectorInterface;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed_m) = {
  [@default "⋱"]
  text: string,
  ed: 'ed_m,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  |;

[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

let hover_view =
    (
      view_ed: (~sort: Sort.t, ~background: bool=?, 'ed_m) => Node.t,
      sort,
      ed: 'ed_m,
    ) =>
  div(
    ~attrs=[Attr.class_("hover-view")],
    [view_ed(~sort, ~background=true, ed)],
  );

let view =
    (
      ~common as _,
      ~ed_str as _,
      ~view_ed: (~sort: Sort.t, ~background: bool=?, 'ed_m) => Node.t,
      ~view_editable as _,
      ~enter_ed as _,
      ~mk_ed as _,
      ~mk_term_ed as _,
      ~calculate_ed as _,
      ~local as _,
      ~parent,
      ~focus as _,
      ~focussed as _,
      m,
      info,
    )
    : View.t =>
  View.mk(
    div(
      ~attrs=[Attr.on_double_click(_ => parent(Remove))],
      [text(m.text), hover_view(view_ed, info.sort, m.ed)],
    ),
  );

module M =
       (Editor: ProjectorInterface.EDITOR)

         : (
           ProjectorInterface.PROJECTOR with
             type model' = model(Editor.model) and
             type action' = action(Editor.action) and
             type focus' = focus(Editor.focus) and
             type editor_model = Editor.model
       ) => {
  type editor_model = Editor.model;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model' = model(Editor.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action' = action(Editor.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus' = focus(Editor.focus);

  let init = (_any: Language.Term.Any.t, ed) => {
    open OptUtil.Syntax;
    let+ ed = ed();
    {
      text: "⋱",
      ed,
    };
  };

  let dynamics = false;

  let placeholder = (m: model', _) =>
    ProjectorShape.inline(m.text == "⋱" ? 2 : String.length(m.text));

  let update = (~common as _, ~sort as _, _info, m, _) => m;

  let mk_term = (~sort, ~prev, {text, ed}) => {
    let (ed, t) = Editor.Update.make_term(~sort, ed);
    (
      {
        text,
        ed,
      },
      Calc.update(t, Fun.id, prev),
    );
  };

  let calculate = (~common, {text, ed}) => {
    text,
    ed: Editor.Update.calculate(~common, ed),
  };

  let get_cursor_info =
    CursorInfo.default(~get_cursor_info_ed=Editor.Focus.get_cursor_info);

  let view = (~common) =>
    view(
      ~common,
      ~ed_str=Editor.View.print_string,
      ~mk_ed=Editor.Model.mk,
      ~mk_term_ed=Editor.Update.make_term,
      ~calculate_ed=Editor.Update.calculate,
      ~view_ed=
        Editor.View.view(
          ~font_metrics=common.font_metrics,
          ~secondary_icons=common.secondary_icons,
        ),
      ~view_editable=Editor.View.view_editable,
      ~enter_ed=Editor.Focus.enter,
    );
};

let methods = {
  init: (~copy_ed as _, _any: Language.Term.Any.t, ed) => {
    open OptUtil.Syntax;
    let+ ed = ed();
    {
      text: "⋱",
      ed,
    };
  },
  dynamics: false,
  placeholder: (~ed_size as _, m: model('ed), _) =>
    ProjectorShape.inline(m.text == "⋱" ? 2 : String.length(m.text)),
  update: (~update_ed as _, ~common as _, ~sort as _, _, m, _) => m,
  mk_term: (~mk_term_ed, ~sort, ~prev, {text, ed}) => {
    let (ed, t) = mk_term_ed(~sort, ed);
    (
      {
        text,
        ed,
      },
      Calc.update(t, Fun.id, prev),
    );
  },
  view,
  calculate: (~calculate_ed, ~common, {text, ed}) => {
    text,
    ed: calculate_ed(~common, ed),
  },
  get_cursor_info: CursorInfo.default,
  sexp_of_model,
  model_of_sexp,
  yojson_of_model,
  model_of_yojson,
  sexp_of_action,
  action_of_sexp,
  yojson_of_action,
  action_of_yojson,
  sexp_of_focus,
  focus_of_sexp,
  yojson_of_focus,
  focus_of_yojson,
};
