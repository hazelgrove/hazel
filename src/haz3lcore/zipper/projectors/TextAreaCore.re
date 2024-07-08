open ZipperBase;
open Virtual_dom.Vdom;

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//TODO(andrew): unhardcode element !!!!!!!!!!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
let selector = ".projector.text textarea";

let serialize = a =>
  a |> ZipperBase.sexp_of_textarea_action |> Sexplib.Sexp.to_string;

let deserialize = a =>
  a |> Sexplib.Sexp.of_string |> ZipperBase.textarea_action_of_sexp;

/* Function to escape linebreaks */
let escape_linebreaks = (str: string): string => {
  Re.Str.global_replace(Re.Str.regexp("\n"), "\\n", str);
};

/* Function to unescape linebreaks */
let unescape_linebreaks = (str: string): string => {
  Re.Str.global_replace(Re.Str.regexp("\\\\n"), "\n", str);
};

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

let put = (str: string): ProjectorsUpdate.t =>
  UpdateSyntax(_ => str |> Form.string_quote |> put);

let textarea =
    (
      ~selector,
      ~inject: ProjectorsUpdate.t => Ui_effect.t(unit),
      text: string,
    ) =>
  Node.textarea(
    ~attrs=[
      // Attr.create("rows", "4"),
      // Attr.create("cols", "21"),
      Attr.on_blur(_ => inject(UpdateModel(serialize(SetInside(false))))),
      Attr.on_click(_ => inject(FocusInternal(selector))),
      Attr.on_input((_, new_text) => inject(put(new_text))),
    ],
    [Node.text(text)],
  );

let n_of = (n: int) =>
  [Node.text("·")]
  @ (List.init(n, _ => [Node.br(), Node.text("·")]) |> List.flatten);

let view =
    (
      ~inject,
      ~selector,
      model: ZipperBase.textarea,
      text: string,
      indicated: option(ZipperBase.accent),
    ) =>
  Node.div(
    ~attrs=[
      Attr.classes(
        ["cols"] @ (model.inside ? [] : ZipperBase.cls(indicated)),
      ),
    ],
    n_of(1 + Util.StringUtil.num_linebreaks(text))
    @ [textarea(~inject, ~selector, text)],
  );

let keymap =
    (
      ~selector,
      model: ZipperBase.textarea,
      direction: Util.Direction.t,
      key: Key.t,
    )
    : option(ProjectorsUpdate.t) => {
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

let mk = (~syntax, model): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = ZipperBase.textarea;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.textarea_action;
     let model = model;
     let projector: projector = TextArea(model);
     let can_project = _ => true;
     //TODO(andrew): cleanup
     let row = Util.StringUtil.num_linebreaks(get(syntax));
     /* +2 for left and right padding */
     let col =
       2
       + List.fold_left(
           max,
           0,
           List.map(
             String.length,
             Re.Str.split(Re.Str.regexp("\n"), get(syntax)),
           ),
         );
     let placeholder = () => Block({row, col});
     let auto_update = _: projector => TextArea(model);
     let update = (a: string): projector =>
       switch (deserialize(a)) {
       | SetInside(b) => TextArea({inside: b})
       };
     let value = syntax |> get |> Form.strip_quotes;
     let view = view(model, value, ~selector);
     let keymap = keymap(~selector, model);
   });
