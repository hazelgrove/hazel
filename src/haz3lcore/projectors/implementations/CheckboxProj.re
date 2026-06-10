open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

/* Pure helpers are exposed at file level (outside the sealed module
   below) so that alternative view backends (e.g. the TUI) can reuse
   the projector's semantics without going through the Vdom view. */

let bool_of = (any: Language.Any.t): option(bool) =>
  switch (any) {
  | Exp({term: Atom(Bool(b)), _}) => Some(b)
  | _ => None
  };

let get = (info: info): bool =>
  switch (
    info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(bool_of)
  ) {
  | Some(b) => b
  | None => failwith("Checkbox: Get: not boolean literal")
  };

let toggle = (info: info): Base.segment =>
  switch (
    info.utility.lift_syntax(
      ~inline=true,
      fun
      | Exp({term: Atom(Bool(b)), _} as t) =>
        Exp({
          ...t,
          term: Atom(Bool(!b)),
        })
      | _ => failwith("Checkbox: Toggle: not boolean literal"),
      info.syntax,
    )
  ) {
  | Some(s) => s
  | None => failwith("Checkbox: Toggle: lift failed")
  };

let shape = ProjectorCore.Shape.inline(2);

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Language.Any.t) =>
    switch (bool_of(any)) {
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
      Node.input(
        ~attrs=
          [
            Attr.create("type", "checkbox"),
            Attr.on_input((_, _) => parent(SetSyntax(toggle(info)))),
          ]
          @ (info |> get ? [Attr.checked] : []),
        (),
      ),
    );
};
