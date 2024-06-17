open Haz3lcore;
open Virtual_dom.Vdom;

// let of_mono = (syntax: Piece.t): option(string) =>
//   switch (syntax) {
//   | Tile({label: [l], _}) => Some(l)
//   | _ => None
//   };

// let mk_mono = (sort: Sort.t, string: string): Piece.t =>
//   string |> Form.mk_atomic(sort) |> Piece.mk_tile(_, []);

// let state_of = (piece: Piece.t): option(int) =>
//   piece |> of_mono |> Option.map(int_of_string);

// let get = (piece: Piece.t): int =>
//   switch (piece |> of_mono |> Option.map(int_of_string)) {
//   | None => failwith("Slider: not integer literal")
//   | Some(s) => s
//   };

// let put = (int: int): Piece.t => int |> string_of_int |> mk_mono(Exp);

let view = (~inject: Projector.action(_) => Ui_effect.t(unit), value: int) =>
  Node.input(
    ~attr=
      Attr.many([
        Attr.create("type", "range"),
        Attr.create("value", string_of_int(value)),
        Attr.on_input((_evt, new_val) =>
          inject(
            UpdateModel(SliderCore.serialize(Set(int_of_string(new_val)))),
          )
        ),
        // => inject(UpdateSyntax(_ => put(int_of_string(new_val))))
      ]),
    [],
  );

let keymap = (key: Key.t): option(Projector.action(string)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk =
    (_syntax: Piece.t, ~inject, _model: ZipperBase.slider)
    : ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.slider_action;

     //let value = get(_syntax);
     let value = _model.value;
     let view = view(value, ~inject);
     let keymap = keymap;
   });
