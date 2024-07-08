open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
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
  switch (piece |> of_mono |> Option.map(bool_of_string)) {
  | None => failwith("Checkbox: not boolean literal")
  | Some(s) => s
  };

let put = (bool: bool): Piece.t => bool |> string_of_bool |> mk_mono(Exp);

let toggle = (piece: Piece.t) => put(!get(piece));

let view = (~inject: ProjectorsUpdate.t => Ui_effect.t(unit), ~syntax, _) =>
  Node.input(
    ~attrs=
      [
        Attr.create("type", "checkbox"),
        Attr.on_input((_, _) => inject(UpdateSyntax(toggle))),
        //JsUtil.stop_mousedown_propagation,
      ]
      @ (get(syntax) ? [Attr.checked] : []),
    (),
  );

let keymap = (_, key: Key.t): option(ProjectorsUpdate.t) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk = (model, ~syntax): core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = unit;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = unit;
     let model = model;
     let projector: projector = Checkbox(model);
     let can_project = p => state_of(p) != None;
     let placeholder = () => Inline(2);
     let auto_update = _: projector => Checkbox();
     let update = (_action): projector => Checkbox();
     let view = view(~syntax);
     let keymap = keymap;
   });
