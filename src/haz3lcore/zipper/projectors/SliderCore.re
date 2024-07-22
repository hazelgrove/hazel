open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

let put: string => Piece.t = Piece.mk_mono(Exp);

let get_opt = (piece: Piece.t): option(int) =>
  piece |> Piece.of_mono |> Util.OptUtil.and_then(int_of_string_opt);

let get = (piece: Piece.t): string =>
  switch (get_opt(piece)) {
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
      (
        _,
        ~info,
        ~go as _,
        ~inject: ProjectorBase.action => Ui_effect.t(unit),
      ) =>
    Util.Web.range(
      ~attrs=[Attr.on_input((_, v) => inject(SetSyntax(put(v))))],
      get(info.syntax),
    );
  let focus = _ => ();
};
