open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Virtual_dom.Vdom;
open ProjectorBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type textarea = unit; //{inside: bool};

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//TODO(andrew): unhardcode element !!!!!!!!!!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

let selector = ".projector.text textarea";
// let serialize = a => a |> sexp_of_action_ |> Sexplib.Sexp.to_string;
// let deserialize = a => a |> Sexplib.Sexp.of_string |> action__of_sexp;

let escape_linebreaks: string => string =
  Re.Str.global_replace(Re.Str.regexp("\n"), "\\n");

let unescape_linebreaks: string => string =
  Re.Str.global_replace(Re.Str.regexp("\\\\n"), "\n");

let of_mono = (syntax: Piece.t): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(unescape_linebreaks(l))
  | _ => None
  };

let mk_mono = (sort: Sort.t, string: string): Piece.t =>
  string |> escape_linebreaks |> Form.mk_atomic(sort) |> Piece.mk_tile(_, []);

let state_of = (piece: Piece.t): option(string) => piece |> of_mono;

let get = (piece: Piece.t): string =>
  switch (piece |> of_mono) {
  | None => failwith("TextArea: not string literal")
  | Some(s) => s
  };

let put = (s: string): Piece.t => s |> mk_mono(Exp);

let put = (str: string): ProjectorBase.action =>
  SetSyntax(str |> Form.string_quote |> put);

let key_handler = (~inject, ~selector, direction: Util.Direction.t, evt) => {
  print_endline("HANDLER: TextAreaCore");
  let key = Key.mk(KeyDown, evt);
  let textarea = JsUtil.TextArea.get(selector);
  //TODO(andrew): clean
  //IE query focus state FocusInternal side?
  // but what if gets unfocussed due to eg refresh?
  let pos = JsUtil.TextArea.caret_rel_pos(textarea);
  let is_last_pos = pos.rows == Last && pos.cols == Last;
  let is_first_pos = pos.rows == First && pos.cols == First;
  // print_endline("is_focus:" ++ string_of_bool(is_focus));
  // let rel_pos = JsUtil.TextArea.caret_rel_pos(textarea);
  // let pos = JsUtil.TextArea.caret_pos(textarea);
  // print_endline("pos: " ++ JsUtil.TextArea.show_rel_pos(rel_pos));
  // print_endline("pos': " ++ JsUtil.TextArea.show_pos(pos));
  switch (key.key, direction) {
  | (D("ArrowRight" | "ArrowDown"), _) when is_last_pos =>
    Effect.Many([
      inject(Escape(selector, Right)),
      Effect.Prevent_default,
      Effect.Stop_propagation,
    ])
  | (D("ArrowLeft" | "ArrowUp"), _) when is_first_pos =>
    Effect.Many([
      inject(Escape(selector, Left)),
      Effect.Prevent_default,
      Effect.Stop_propagation,
    ])
  | _ =>
    print_endline("Warning: Not focussed");
    Effect.Stop_propagation;
  };
};

let textarea =
    (
      ~selector,
      ~inject: ProjectorBase.action => Ui_effect.t(unit),
      text: string,
    ) =>
  Node.textarea(
    ~attrs=[
      Attr.id("sdfsdf"),
      Attr.on_keydown(key_handler(~selector, Left, ~inject)),
      //Attr.on_mousedown(_ => {Effect.Ignore}),
      Attr.on_input((_, new_text) => inject(put(new_text))),
    ],
    [Node.text(text)],
  );

let n_of = (n: int) =>
  //·•⬤
  [Node.text("·")]
  @ (List.init(n, _ => [Node.br(), Node.text("·")]) |> List.flatten);

let _key_handler = (~inject as _, ~dir: Key.dir, evt): Effect.t(unit) => {
  open Effect;
  let _key = Key.mk(dir, evt);
  print_endline("LALALLA TExtcoree....");
  //Ignore;
  Prevent_default;
};

let view = (_model: textarea, ~selector, ~info, ~inject) => {
  let text = info.syntax |> get |> Form.strip_quotes;
  Node.div(
    ~attrs=[
      Attr.classes(["cols" /*@ (model.inside ? ["inside"] : [])*/]),
      // Attr.classes(["cols"] @ (model.inside ? [] : cls(info.status))),
    ],
    n_of(Util.StringUtil.num_linebreaks(text))
    @ [textarea(~inject, ~selector, text)],
  );
};

let keymap =
    (
      ~selector as _,
      _model: textarea,
      _direction: Util.Direction.t,
      _key: Key.t,
    )
    : option(ProjectorBase.action) => {
  None;
};
let line_lengths = syntax =>
  List.map(String.length, Re.Str.split(Re.Str.regexp("\n"), get(syntax)));
let num_lines = syntax => List.fold_left(max, 0, line_lengths(syntax));
let placeholder = (_, info) =>
  Block({
    row: Util.StringUtil.num_linebreaks(get(info.syntax)),
    col: 2 + num_lines(info.syntax) /* +2 for left and right padding */
  });

module M: CoreInner = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = textarea;
  let init = (); //{inside: false};
  let can_project = _ => true; //TODO(andrew): restrict
  let placeholder = placeholder;
  //  let auto_update = _ => TextArea(model);
  let update = (model, _) => model;
  let view = view(~selector);
  let keymap = keymap(~selector);
};
