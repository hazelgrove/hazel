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

  let get = (utility: utility, syntax: syntax): int =>
    switch ([syntax] |> utility.seg_to_term |> int_of) {
    | Some(i) => i
    | None => failwith("Slider: not integer literal")
    };

  let can_project = (_, any) => int_of(any) != None;
  let can_focus = false;
  let dynamics = false;
  let placeholder = (_, _) => ProjectorCore.inline(10);
  let update = (model, _, _) => model;

  let view =
      (
        _,
        info,
        ~local as _,
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) => {
    let put_syntax = (v: string): syntax =>
      info.utility.lift_syntax(
        fun
        | Exp(any) => Exp({...any, term: Int(int_of_string(v))})
        | any => any,
        info.syntax,
      );
    Util.Web.range(
      ~attrs=[Attr.on_input((_, v) => parent(SetSyntax(put_syntax(v))))],
      get(info.utility, info.syntax) |> string_of_int,
    );
  };

  let offside_view = Option.None;
  let overlay_view = Option.None;
  let underlay_view = Option.None;
  let focus = _ => ();
};
