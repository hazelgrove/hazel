open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

module Make: Projector =
  (
    Syntax: {
      //  type segment;
    },
  ) => {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type model = unit;
    let kind = ProjectorCore.Kind.Slider;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type action = unit;

    let int_of = (any: Semantics.Any.t): option(Bigint.t) =>
      switch (any) {
      | Exp({term: Atom(Int(i)), _}) => Some(i)
      | _ => None
      };

    let init = (any: Semantics.Any.t) =>
      switch (int_of(any)) {
      | Some(_) => Some()
      | None => None
      };

    let get = (info: info('s)): Bigint.t =>
      switch (
        info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(int_of)
      ) {
      | Some(i) => i
      | None => failwith("Slider: Get: not integer literal")
      };

    let put = (info: info('s), v: string): 's =>
      switch (
        info.utility.lift_syntax(
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
    let placeholder = (_, _) => ProjectorCore.Shape.inline(10);
    let update = (model, _, _) => model;

    let view =
        (
          _,
          info,
          ~local as _,
          ~parent: external_action('s) => Ui_effect.t(unit),
          ~view_seg as _,
        ) =>
      View.mk(
        Util.Web.range(
          ~attrs=[
            Attr.on_input((_, v) => parent(SetSyntax(put(info, v)))),
          ],
          info |> get |> Bigint.to_string,
        ),
      );
  };
