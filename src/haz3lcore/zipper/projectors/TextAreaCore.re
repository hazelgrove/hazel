// open Sexplib.Std;
// open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Virtual_dom.Vdom;
open ProjectorBase;

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//TODO(andrew): unhardcode element !!!!!!!!!!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

//TODO(andrew): reinstate
// [@deriving (show({with_path: false}), sexp, yojson)]
// type inner_action +=
//   | SetInside(bool);

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
  UpdateSyntax(_ => str |> Form.string_quote |> put);

let textarea =
    (
      ~selector,
      ~inject: ProjectorBase.action => Ui_effect.t(unit),
      text: string,
    ) =>
  Node.textarea(
    ~attrs=[
      Attr.on_blur(_ => inject(UpdateModel(SetInside(false)))),
      Attr.on_focus(_ => inject(UpdateModel(SetInside(true)))),
      Attr.on_click(_ => inject(FocusInternal(selector))),
      Attr.on_mousedown(_ => inject(FocusInternal(selector))),
      Attr.on_input((_, new_text) => inject(put(new_text))),
    ],
    [Node.text(text)],
  );

let n_of = (n: int) =>
  [Node.text("·")]
  @ (List.init(n, _ => [Node.br(), Node.text("·")]) |> List.flatten);

let view = (model: textarea, ~selector, ~info, ~inject) => {
  let text = info.syntax |> get |> Form.strip_quotes;
  Node.div(
    ~attrs=[
      Attr.classes(["cols"] @ (model.inside ? [] : cls(info.status))),
    ],
    n_of(1 + Util.StringUtil.num_linebreaks(text))
    @ [textarea(~inject, ~selector, text)],
  );
};

let keymap =
    (~selector, model: textarea, direction: Util.Direction.t, key: Key.t)
    : option(ProjectorBase.action) => {
  let textarea = JsUtil.TextArea.get(selector);
  let focussed = model.inside;
  //TODO(andrew): make actual focus king?
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
  | (D("ArrowRight"), Right) when !focussed =>
    Some(FocusInternal(selector))
  | (D("ArrowLeft"), Left) when !focussed =>
    JsUtil.TextArea.set_caret_to_end(textarea);
    Some(FocusInternal(selector));
  | (D("ArrowRight" | "ArrowDown"), _) when focussed && is_last_pos =>
    Some(Escape(selector, Left))
  | (D("ArrowLeft" | "ArrowUp"), _) when focussed && is_first_pos =>
    Some(Escape(selector, Right))
  | _ when focussed => Some(Default)
  | _ =>
    print_endline("Warning: Not focussed");
    None;
  };
};

let line_lengths = syntax =>
  List.map(String.length, Re.Str.split(Re.Str.regexp("\n"), get(syntax)));

let num_lines = syntax => List.fold_left(max, 0, line_lengths(syntax));

let placeholder = info =>
  Block({
    row: Util.StringUtil.num_linebreaks(get(info.syntax)),
    col: 2 + num_lines(info.syntax) /* +2 for left and right padding */
  });

let mk = (model): core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = textarea;
     let model = model;
     let can_project = _ => true;
     let placeholder = placeholder;
     //  let auto_update = _ => TextArea(model);
     let update = a =>
       switch (a) {
       | SetInside(b) =>
         print_endline("setting inside:" ++ string_of_bool(b));
         TextArea({inside: b});
       | _ => TextArea(model)
       };
     let view = view(model, ~selector);
     let keymap = keymap(~selector, model);
   });
