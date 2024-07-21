open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

let of_id = (id: Id.t) =>
  "id" ++ (id |> Id.to_string |> String.sub(_, 0, 8));

let of_mono = (syntax: Piece.t): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(StringUtil.unescape_linebreaks(l))
  | _ => None
  };

let mk_mono = (sort: Sort.t, string: string): Piece.t =>
  string
  |> StringUtil.escape_linebreaks
  |> Form.mk_atomic(sort)
  |> Piece.mk_tile(_, []);

let get = (piece: Piece.t): string =>
  switch (piece |> of_mono) {
  | None => failwith("TextArea: not string literal")
  | Some(s) => s
  };

let put = (s: string): Piece.t => s |> mk_mono(Exp);

let put = (str: string): ProjectorBase.action =>
  SetSyntax(str |> Form.string_quote |> put);

let key_handler = (id, ~inject, evt) => {
  print_endline("textarea: keydown");
  open Effect;
  let key = Key.mk(KeyDown, evt);
  let is_last_pos =
    JsUtil.TextArea.caret_at_end(JsUtil.TextArea.get(of_id(id)));
  let is_first_pos =
    JsUtil.TextArea.caret_at_start(JsUtil.TextArea.get(of_id(id)));
  switch (key.key) {
  // | D("Enter") =>
  //   print_endline("textarea: enter");
  //   Many([Stop_propagation]);
  | D("ArrowRight" | "ArrowDown") when is_last_pos =>
    JsUtil.get_elem_by_id(of_id(id))##blur;
    Many([inject(Escape(Right)), Stop_propagation]);
  | D("ArrowLeft" | "ArrowUp") when is_first_pos =>
    JsUtil.get_elem_by_id(of_id(id))##blur;
    Many([inject(Escape(Left)), Stop_propagation]);
  | _ => Stop_propagation
  };
};

let textarea =
    (id, ~inject: ProjectorBase.action => Ui_effect.t(unit), text: string) =>
  Node.textarea(
    ~attrs=[
      Attr.id(of_id(id)),
      Attr.on_keydown(key_handler(id, ~inject)),
      // Attr.on_keyup(_ => {
      //   print_endline("textarea: on_keyup");
      //   Effect.(Many([Stop_propagation]));
      // }),
      // Attr.on_keyup(_ => {
      //   print_endline("textarea: on_keyup");
      //   Effect.(Many([Stop_propagation]));
      // }),
      Attr.on_input((_, new_text) => {
        print_endline("textarea: on_input");
        Effect.(Many([inject(put(new_text))]));
      }),
      // Attr.on_change((_, _) => {
      //   print_endline("textarea: on_change");
      //   Effect.(Many([Stop_propagation, Prevent_default]));
      // }),
    ],
    [Node.text(text)],
  );

let n_of = (n: int) =>
  [Node.text("·")]  //·•⬤
  @ (List.init(n, _ => [Node.br(), Node.text("·")]) |> List.flatten);

let view = (_, ~info, ~go as _, ~inject) => {
  let text = info.syntax |> get |> Form.strip_quotes;
  Node.div(
    //TODO(andrew): rm wrapper?
    ~attrs=[Attr.classes(["projector-wrapper"])],
    [
      Node.div(
        ~attrs=[Attr.classes(["cols"])],
        n_of(StringUtil.num_linebreaks(text))
        @ [textarea(info.id, ~inject, text)],
      ),
    ],
  );
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = ();
  let can_project = _ => true; //TODO(andrew): restrict somehow
  let can_focus = true;
  let placeholder = (_, info) => {
    let str = Form.strip_quotes(get(info.syntax));
    // print_endline(
    //   "num-rows:" ++ (str |> StringUtil.num_lines |> string_of_int),
    // );
    Block({
      row: StringUtil.num_lines(str),
      /* +2 for left and right padding */
      col: 2 + StringUtil.max_line_width(str),
    });
  };
  let update = (model, _) => model;
  let view = view;
  let activate = ((id: Id.t, d: Direction.t)) => {
    JsUtil.get_elem_by_id(of_id(id))##focus;
    switch (d) {
    | Left => ()
    | Right =>
      JsUtil.TextArea.set_caret_to_end(JsUtil.TextArea.get(of_id(id)))
    };
  };
};
