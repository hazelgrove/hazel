open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed_m) = bool;

[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | Toggle;

[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

let init = (~copy_ed as _, any: Term.Any.t, _ed) =>
  switch (any) {
  | Exp({term: Atom(Bool(b)), _}) => Some(b)
  | _ => None
  };

let mk_term =
    (~mk_term_ed as _, ~sort as _, ~prev as _, m)
    : (model('a), Calc.t(Any.t)) => (
  m,
  NewValue(Exp(Atom(Bool(m)) |> Exp.fresh)),
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
    Node.input(
      ~attrs=
        [
          Attr.create("type", "checkbox"),
          Attr.on_input((_, _) => local(Toggle)),
        ]
        @ (model ? [Attr.checked] : []),
      (),
    ),
  );

let methods = {
  init,
  dynamics: false,
  placeholder: (~ed_size as _, _, _) => ProjectorShape.inline(2),
  update: (~update_ed as _, ~common as _, ~sort as _, _, b, Toggle) => !b,
  mk_term,
  view,
  calculate: Calculate.default,
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
