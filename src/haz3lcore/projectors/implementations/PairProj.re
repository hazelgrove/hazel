open Util;
open ProjectorBase;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = ('ed, 'ed);

[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | Left('ed_a)
  | Right('ed_a);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  | Left('ed_f)
  | Right('ed_f);

let methods:
  methods(model('ed_m), action('ed_a), focus('ed_f), 'ed_m, 'ed_a, 'ed_f) = {
  init: (_any, ed) => {
    let* ed = ed();
    Some((ed, ed));
  },
  focusable: Focusable.non, // TODO: Internal focus handling
  dynamics: false,
  view:
    (
      ~ed_str as _,
      ~view_ed,
      ~mk_ed as _,
      (ed1, ed2),
      _info,
      ~local as _,
      ~parent as _,
    ) =>
    View.{
      inline:
        Web.div_c(
          "main",
          [
            Web.Node.text("("),
            view_ed(~sort=Exp, ed1),
            Web.Node.text(","),
            view_ed(~sort=Exp, ed2),
            Web.Node.text(")"),
          ],
        ),
      offside: None,
      overlay: None,
    },
  placeholder: (~ed_str, (ed1, ed2), _info) =>
    ProjectorShape.inline(
      7 + String.length(ed_str(ed1)) + String.length(ed_str(ed2)),
    ),
  update:
    (~update_ed, ~common as _, ~sort, _info, (left: 'ed, right: 'ed), action) => {
    switch (action) {
    | Left(ed_ac) =>
      let l_ed = update_ed(~sort, ed_ac, left);
      (l_ed, right);
    | Right(ed_ac) =>
      let r_ed = update_ed(~sort, ed_ac, right);
      (left, r_ed);
    };
  },
  mk_term: (~term_of_ed, _sort, (ed1, ed2)) =>
    Exp(
      Exp.fresh(
        Tuple([
          term_of_ed(Exp, ed1) |> Any.is_exp |> Option.get,
          term_of_ed(Exp, ed2) |> Any.is_exp |> Option.get,
        ]),
      ),
    ),
  handle_key_event:
    (~handle_key_ed, ~focus: focus('ed_f), ~key, (ed1, ed2)) =>
    switch (focus) {
    | Left(focus) =>
      handle_key_ed(~focus, ~key, ed1)
      |> Option.map((x): action('ed_a) => Left(x))
    | Right(focus) =>
      handle_key_ed(~focus, ~key, ed2)
      |> Option.map((x): action('ed_a) => Right(x))
    },
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
