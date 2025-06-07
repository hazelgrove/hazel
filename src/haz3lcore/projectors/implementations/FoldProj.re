open Util;
open ProjectorBase;
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

let hover_view = (view_ed, ed: 'ed_m) => {
  //TODO(andrew): hardcoded sort below
  //TODO(andrew): add background deco to view_ed below
  div(
    ~attrs=[Attr.class_("hover-view")],
    [view_ed(~sort=Sort.Exp, ed)],
  );
};

let view =
    (
      ~common as _,
      ~ed_str as _,
      ~view_ed,
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
      _info,
    )
    : View.t =>
  View.mk(
    div(
      ~attrs=[Attr.on_double_click(_ => parent(Remove))],
      [text(m.text), hover_view(view_ed, m.ed)],
    ),
  );

let methods = {
  init: (~copy_ed as _, _any: Term.Any.t, ed) => {
    //TODO(andrew): this doesn't init on nonconvex tiles
    open OptUtil.Syntax;
    let+ ed = ed();
    {
      text: "⋱",
      ed,
    };
  },
  focusable: Focusable.non,
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
