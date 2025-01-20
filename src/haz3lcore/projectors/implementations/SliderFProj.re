open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = ();

  let float_of = (any: Any.t): option(float) =>
    switch (any) {
    | Exp({term: Float(f), _}) => Some(f)
    | _ => None
    };

  let get = (utility: utility, syntax: syntax): float =>
    switch ([syntax] |> utility.seg_to_term |> float_of) {
    | Some(f) => f
    | None => failwith("SliderF: not float literal")
    };

  let can_project = (_, any) => float_of(any) != None;
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
        | Exp(any) => Exp({...any, term: Float(float_of_string(v))})
        | any => any,
        info.syntax,
      );
    Util.Web.range(
      ~attrs=[Attr.on_input((_, v) => parent(SetSyntax(put_syntax(v))))],
      get(info.utility, info.syntax) |> Printf.sprintf("%.2f"),
    );
  };

  let offside_view = Option.None;
  let overlay_view = Option.None;
  let underlay_view = Option.None;
  let focus = _ => ();
};
