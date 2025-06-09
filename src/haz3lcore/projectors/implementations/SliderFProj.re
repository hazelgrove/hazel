open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = float;
[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | Set(float);
[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

let init = (~copy_ed as _, any: Term.Any.t, _ed) =>
  switch (any) {
  | Exp({term: Atom(Float(f)), _}) => Some(f)
  | _ => None
  };

let mk_term =
    (~mk_term_ed as _, ~sort as _, ~prev as _, m)
    : (model('a), Calc.t(Any.t)) => (
  m,
  NewValue(Exp(Atom(Float(m)) |> Exp.fresh)),
);

let view =
    (
      ~common as _,
      ~ed_str as _,
      ~view_ed as _,
      ~view_editable as _,
      ~enter_ed as _,
      ~mk_ed as _,
      ~mk_term_ed as _,
      ~calculate_ed as _,
      ~local,
      ~parent as _,
      ~focus as _,
      ~focussed as _,
      model,
      _info,
    )
    : View.t =>
  View.mk(
    Util.Web.range(
      ~attrs=[Attr.on_input((_, v) => local(Set(float_of_string(v))))],
      model |> Printf.sprintf("%.2f"),
    ),
  );

let methods = {
  init,
  dynamics: false,
  placeholder: (~ed_size as _, _, _) => ProjectorShape.inline(10),
  update: (~update_ed as _, ~common as _, ~sort as _, _, _, Set(f)) => f,
  calculate: Calculate.default,
  view,
  mk_term,
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
