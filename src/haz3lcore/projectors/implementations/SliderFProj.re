open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type model = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type action = unit;

let float_of = (any: Any.t): option(float) =>
  switch (any) {
  | Exp({term: Atom(Float(f)), _}) => Some(f)
  | _ => None
  };

let init = (any: Term.Any.t) =>
  switch (float_of(any)) {
  | Some(_) => Some()
  | None => None
  };

let get = (info: info('p)): float =>
  switch (
    info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(float_of)
  ) {
  | Some(f) => f
  | None => failwith("SliderF: Get: not float literal")
  };

let put = (info: info('p), v: string): Base.segment('p) =>
  switch (
    info.utility.lift_syntax(
      fun
      | Exp(t) =>
        Exp({
          ...t,
          term: Atom(Float(float_of_string(v))),
        })
      | _ => failwith("SliderF: Put: not float literal"),
      info.syntax,
    )
  ) {
  | Some(s) => s
  | None => failwith("SliderF: Put: lift failed")
  };

let focusable = Focusable.non;
let dynamics = false;
let placeholder = (_, _) => ProjectorShape.inline(10);
let update = (model, _, _) => model;

let view =
    (
      _,
      info,
      ~local as _,
      ~parent: external_action('p) => Ui_effect.t(unit),
      ~view_seg as _,
    ) =>
  View.mk(
    Util.Web.range(
      ~attrs=[Attr.on_input((_, v) => parent(SetSyntax(put(info, v))))],
      info |> get |> Printf.sprintf("%.2f"),
    ),
  );

let mk_term = mk_term_default;

let methods = {
  init,
  focusable,
  dynamics,
  placeholder,
  view,
  update,
  mk_term,
};
