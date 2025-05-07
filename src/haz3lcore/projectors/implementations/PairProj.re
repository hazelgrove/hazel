open Util;
open ProjectorBase;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = ('ed, 'ed);

type action('ed_a) =
  | Left('ed_a)
  | Right('ed_a);

let methods: methods(model('ed), action('ed_a), 'ed, 'ed_a) = {
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
    (~sort, ~update_ed, ~statics, (left: 'ed, right: 'ed), info, action) => {
    switch (action) {
    | Left(ed_ac) =>
      let l_ed = update_ed(~sort, statics, ed_ac, left);
      (l_ed, right);
    | Right(ed_ac) =>
      let r_ed = update_ed(~sort, statics, ed_ac, right);
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
  persist: sexp_of_model,
  unpersist: model_of_sexp,
};
