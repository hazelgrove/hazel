open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

let put: string => Piece.t = Piece.mk_mono(Exp);

let get_opt = (piece: Piece.t): option(string) =>
  piece |> Piece.of_mono |> Option.map(Form.parse_livelit);

let get = (piece: Piece.t): string =>
  switch (get_opt(piece)) {
  | None => failwith("ERROR: Slider: not integer literal")
  | Some(s) => s
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
      (_, ~info, ~local as _, ~parent: external_action => Ui_effect.t(unit)) => {
    let llname = get(info.syntax);
    print_endline("Livelit: " ++ llname);
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [Node.text("Livelit: " ++ llname)],
    );
  };
  let focus = _ => ();
};
