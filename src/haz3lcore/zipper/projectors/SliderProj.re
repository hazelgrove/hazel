open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

let put = (s: string): Segment.t => [Piece.mk_mono(Exp, s)];

let get_opt = (seg: Segment.t): option(int) =>
  switch (seg) {
  | [p] => p |> Piece.of_mono |> Util.OptUtil.and_then(int_of_string_opt)
  | _ => None
  };

let get = (seg: Segment.t): string =>
  switch (get_opt(seg)) {
  | None => failwith("ERROR: Slider: not integer literal")
  | Some(s) => string_of_int(s)
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
