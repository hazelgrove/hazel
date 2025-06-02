open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = Bigint.t;
[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | Set(Bigint.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

let int_of = (any: Any.t): option(Bigint.t) =>
  switch (any) {
  | Exp({term: Atom(Int(i)), _}) => Some(i)
  | _ => None
  };

let init = (any: Term.Any.t, _ed) =>
  switch (int_of(any)) {
  | Some(i) => Some(i)
  | None => None
  };

let focusable = Focusable.non;
let dynamics = false;
let placeholder = (~ed_str as _, _, _) => ProjectorShape.inline(10);
let update = (~update_ed as _, ~common as _, ~sort as _, _, _, Set(n)) => n;

let view =
    (
      ~common as _,
      ~ed_str as _,
      ~view_ed as _,
      ~view_editable as _,
      ~mk_ed as _,
      ~local,
      ~parent as _,
      ~focus as _,
      ~focussed as _,
      model,
      _info,
    ) =>
  View.mk(
    Util.Web.range(
      ~attrs=[Attr.on_input((_, v) => local(Set(Bigint.of_string(v))))],
      model |> Bigint.to_string,
    ),
  );

let mk_term =
    (~mk_term_ed as _, ~sort as _, ~prev as _, m)
    : (model('a), Calc.t(Any.t)) => (
  m,
  NewValue(Exp(Atom(Int(m)) |> Exp.fresh)),
);

let get_cursor_info =
    (
      ~get_cursor_info_ed as _,
      ~common as _,
      ~inject as _: action('a) => Ui_effect.t(unit),
      ~read_only as _,
      _model,
      _focus,
    ) => Cursor.empty;

let methods = {
  init,
  focusable,
  dynamics,
  placeholder,
  view,
  update,
  calculate: (~calculate_ed as _, ~common as _, m) => m,
  mk_term,
  get_cursor_info,
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
