open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* Pure helpers are exposed at file level (outside the sealed module
   below) so that alternative view backends (e.g. the TUI) can reuse
   the projector's semantics without going through the Vdom view. */

let int_of = (any: Language.Any.t): option(Bigint.t) =>
  switch (any) {
  | Exp({term: Atom(Int(i)), _}) => Some(i)
  | _ => None
  };

let get = (info: info): Bigint.t =>
  switch (info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(int_of)) {
  | Some(i) => i
  | None => failwith("Slider: Get: not integer literal")
  };

let put = (info: info, v: string): Base.segment =>
  switch (
    info.utility.lift_syntax(
      ~inline=true,
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

let shape = ProjectorCore.Shape.inline(10);

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Language.Any.t) =>
    switch (int_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let focusable = Focusable.non;
  let dynamics = false;
  let elaborate_syntax = false;
  let placeholder = (_, _) => shape;
  let update = (model, _, _) => model;
  let error = (_, _): option(ProjectorBase.error) => None;

  let view = ({info, parent, _}: View.args(model, action)) =>
    View.mk(
      Util.WebUtil.range(
        ~attrs=[Attr.on_input((_, v) => parent(SetSyntax(put(info, v))))],
        info |> get |> Bigint.to_string,
      ),
    );
};
