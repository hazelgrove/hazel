// open Sexplib.Std;
// open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Virtual_dom.Vdom;
open ProjectorBase;

let of_mono = (syntax: Piece.t): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(l)
  | _ => None
  };

let mk_mono = (sort: Sort.t, string: string): Piece.t =>
  string |> Form.mk_atomic(sort) |> Piece.mk_tile(_, []);

let state_of = (piece: Piece.t): option(int) =>
  piece |> of_mono |> Util.OptUtil.and_then(int_of_string_opt);

let put = (new_val: string): Piece.t => mk_mono(Exp, new_val);

let get = (piece: Piece.t): int =>
  switch (state_of(piece)) {
  | None =>
    //TODO(andrew): fix this bug (moving caret to right of slider crashes)
    switch (of_mono(piece)) {
    | Some(p) =>
      print_endline("ERROR: Slider: not integer literal: " ++ p);
      0;
    | None =>
      print_endline("ERROR: Slider: not integer literal: No piece");
      0;
    }
  | Some(s) => s
  };

let view = (~inject: ProjectorBase.action => Ui_effect.t(unit), value: int) =>
  Node.input(
    ~attrs=[
      Attr.create("type", "range"),
      Attr.create("value", string_of_int(value)),
      Attr.on_input((_evt, new_val) =>
        inject(UpdateSyntax(_ => put(new_val)))
      ),
    ],
    (),
  );

let keymap = (_, key: Key.t): option(ProjectorBase.action) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk = (model): core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = slider;
     let model = model;
     let can_project = p => state_of(p) != None;
     let placeholder = _ => Inline(10);
     let auto_update = _ => Slider(model);
     let update = _ => Slider(model);
     let view = (~status as _, ~syntax, ~info as _) => view(get(syntax));
     let keymap = keymap;
   });
