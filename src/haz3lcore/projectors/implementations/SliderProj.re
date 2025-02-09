open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = ();

  let int_of = (any: Any.t): option(int) =>
    switch (any) {
    | Exp({term: Int(i), _}) => Some(i)
    | _ => None
    };

  let get = (info: info): int =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(int_of)
    ) {
    | Some(i) => i
    | None => failwith("Slider: Get: not integer literal")
    };

  let put = (info: info, v: string): Base.segment =>
    switch (
      info.utility.lift_syntax(
        fun
        | Exp(t) => Exp({...t, term: Int(int_of_string(v))})
        | _ => failwith("Slider: Put: not integer literal"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("Slider: Put: lift failed")
    };

  let can_project = any => int_of(any) != None;
  let can_focus = false;
  let dynamics = false;
  let placeholder = (_, _) => ProjectorCore.Shape.inline(10);
  let update = (model, _, _) => model;

  let view =
      (
        _,
        info,
        ~local as _,
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) =>
    View.mk(
      Util.Web.range(
        ~attrs=[Attr.on_input((_, v) => parent(SetSyntax(put(info, v))))],
        info |> get |> string_of_int,
      ),
    );

  let focus = _ => ();
};
