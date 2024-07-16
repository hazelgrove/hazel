open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
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

module M: CoreInner = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  let init = ();
  let can_project = p => get_opt(p) != None;
  let placeholder = (_, _) => Inline(10);
  let update = (model, _) => model;
  let view = (_, ~info, ~inject: ProjectorBase.action => Ui_effect.t(unit)) =>
    Util.Web.range(
      ~attrs=[Attr.on_input((_, v) => inject(SetSyntax(put(v))))],
      get(info.syntax),
    );
  let keymap = (_, _, _) => None;
};
