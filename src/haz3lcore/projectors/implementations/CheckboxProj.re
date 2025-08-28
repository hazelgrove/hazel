open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let bool_of = (any: Language.Any.t): option(bool) =>
    switch (any) {
    | Exp({term: Atom(Bool(b)), _}) => Some(b)
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (bool_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let get = (info: info): bool =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(bool_of)
    ) {
    | Some(b) => b
    | None => failwith("Checkbox: Get: not boolean literal")
    };

  let toggle = (info): Base.segment =>
    switch (
      info.utility.lift_syntax(
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

  let focusable = Focusable.non;
  let dynamics = false;
  let placeholder = (_, _) => ProjectorCore.Shape.inline(2);
  let update = (model, _, _) => model;

  let view =
      (
        _,
        info,
        ~local as _,
        ~parent: external_action => Ui_effect.t(unit),
        ~parent_global as _,
        ~view_seg as _,
      ) =>
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
