open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

let of_mono = (syntax: Piece.t): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(l)
  | _ => None
  };

let mk_mono = (sort: Sort.t, string: string): Piece.t =>
  string |> Form.mk_atomic(sort) |> Piece.mk_tile(_, []);

let state_of = (piece: Piece.t): option(bool) =>
  piece |> of_mono |> Option.map(bool_of_string);

let get = (piece: Piece.t): bool =>
  switch (piece |> of_mono |> Util.OptUtil.and_then(bool_of_string_opt)) {
  | None => failwith("Checkbox: not boolean literal")
  | Some(s) => s
  };

let put = (bool: bool): Piece.t => bool |> string_of_bool |> mk_mono(Exp);

let toggle = (piece: Piece.t) => put(!get(piece));

let view =
    (
      _,
      ~info,
      ~go as _,
      ~inject: ProjectorBase.external_action => Ui_effect.t(unit),
    ) =>
  Node.input(
    ~attrs=
      [
        Attr.create("type", "checkbox"),
        Attr.on_input((_, _) =>
          inject(SetSyntax(put(!get(info.syntax))))
        ),
      ]
      @ (get(info.syntax) ? [Attr.checked] : []),
    (),
  );

let keymap = (_, _, key: Key.t): option(ProjectorBase.external_action) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = ();
  let can_project = p => state_of(p) != None;
  let can_focus = false;
  let placeholder = (_, _) => Inline(2);
  let update = (model, _) => model;
  let view = view;
  let focus = _ => ();
};
