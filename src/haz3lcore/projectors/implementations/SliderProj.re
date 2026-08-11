open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let int_of = (any: Language.Any.t): option(Bigint.t) =>
    switch (any) {
    | Exp({term: Atom(Int(i)), _}) => Some(i)
    | _ => None
    };

  let init = (any: Language.Any.t, _) =>
    switch (int_of(any)) {
    | Some(_) => Some(((), None))
    | None => None
    };

  let get = (info: info): Bigint.t =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(int_of)
    ) {
    | Some(i) => i
    | None => failwith("Slider: Get: not integer literal")
    };

  let put = (info: info, v: string): Language.Any.t =>
    switch (
      info.utility.lift_term(
        fun
        | Exp(t) =>
          Exp({
            ...t,
            term: Atom(Int(Bigint.of_string(v))),
          })
        | _ => failwith("Slider: Put: not integer literal"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("Slider: Put: lift failed")
    };

  let focusable = Focusable.non;
  let dynamics = false;
  let elaborate_syntax = false;
  let placeholder = (_, _, _) => ProjectorCore.Shape.inline(10);
  let splice_rows = (_, _, _) => Id.Map.empty;
  let update = (model, _, _) => model;
  let error = (_, _): option(ProjectorBase.error) => None;
  let context_actions = (_, _, ~splice as _) => [];

  let view = ({info, parent, _}: View.args(model, action)) =>
    View.mk(
      Util.WebUtil.range(
        ~attrs=[
          Attr.on_input((_, v) => parent(SetTerm(put(info, v), false))),
        ],
        info |> get |> Bigint.to_string,
      ),
    );
};
