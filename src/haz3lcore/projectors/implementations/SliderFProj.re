open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let float_of = (any: Language.Any.t): option(float) =>
    switch (any) {
    | Exp({term: Atom(Float(f)), _}) => Some(f)
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (float_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let get = (info: info): float =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(float_of)
    ) {
    | Some(f) => f
    | None => failwith("SliderF: Get: not float literal")
    };

  let put = (info: info, v: string): Base.segment =>
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
  let placeholder = (_, _) => ProjectorCore.Shape.inline(10);
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
      Util.WebUtil.range(
        ~attrs=[Attr.on_input((_, v) => parent(SetSyntax(put(info, v))))],
        info |> get |> Printf.sprintf("%.2f"),
      ),
    );
};
