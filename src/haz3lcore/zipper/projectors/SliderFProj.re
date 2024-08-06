open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* Some decimal places necessary to avoid becoming an int */
let float_of_float = s => s |> float_of_string |> Printf.sprintf("%.2f");

let put = (s: string): Segment.t => [
  s |> float_of_float |> Piece.mk_mono(Exp),
];

let get_opt = (seg: Segment.t): option(float) =>
  switch (seg) {
  | [p] => p |> Piece.of_mono |> Util.OptUtil.and_then(float_of_string_opt)
  | _ => None
  };

let get = (seg: Segment.t): string =>
  switch (get_opt(seg)) {
  | None => failwith("ERROR: Slider: not float literal")
  | Some(s) => Printf.sprintf("%.2f", s)
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = ();
  let can_project = p => get_opt(p) != None;
  let can_focus = false;
  let placeholder = (_, _) => Inline(10);
  let update = (model, _) => model;
  let view =
      (_, ~info, ~local as _, ~parent: external_action => Ui_effect.t(unit)) =>
    Util.Web.range(
      ~attrs=[Attr.on_input((_, v) => parent(SetSyntax(put(v))))],
      get(info.syntax),
    );
  let focus = _ => ();
};
