open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = Bigint.t;
[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | Set(Bigint.t);

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
let update = (model, _, _) => model;

let view =
    (
      ~ed_str as _,
      ~view_ed as _,
      ~mk_ed as _,
      model,
      _info,
      ~local,
      ~parent as _,
    ) =>
  View.mk(
    Util.Web.range(
      ~attrs=[Attr.on_input((_, v) => local(Set(Bigint.of_string(v))))],
      model |> Bigint.to_string,
    ),
  );

let mk_term = (~term_of_ed as _, _, m): Any.t =>
  Exp(Atom(Int(m)) |> Exp.fresh);

let methods = {
  init,
  focusable,
  dynamics,
  placeholder,
  view,
  update,
  mk_term,
};
