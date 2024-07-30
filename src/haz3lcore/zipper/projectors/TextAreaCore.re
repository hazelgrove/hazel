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

let put = (str: string): external_action =>
  SetSyntax(str |> Form.string_quote |> put);

let is_last_pos = id =>
  JsUtil.TextArea.caret_at_end(JsUtil.TextArea.get(of_id(id)));
let is_first_pos = id =>
  JsUtil.TextArea.caret_at_start(JsUtil.TextArea.get(of_id(id)));

let key_handler = (id, ~parent, evt) => {
  open Effect;
  let key = Key.mk(KeyDown, evt);

  switch (key.key) {
  | D("ArrowRight" | "ArrowDown") when is_last_pos(id) =>
    JsUtil.get_elem_by_id(of_id(id))##blur;
    Many([parent(Escape(Right)), Stop_propagation]);
  | D("ArrowLeft" | "ArrowUp") when is_first_pos(id) =>
    JsUtil.get_elem_by_id(of_id(id))##blur;
    Many([parent(Escape(Left)), Stop_propagation]);
  | _ => Stop_propagation
  };
};

let textarea =
    (id, ~parent: external_action => Ui_effect.t(unit), text: string) =>
  Node.textarea(
    ~attrs=[
      Attr.id(of_id(id)),
      Attr.on_keydown(key_handler(id, ~parent)),
      Attr.on_input((_, new_text) =>
        Effect.(Many([parent(put(new_text))]))
      ),
      /* Note: adding these handlers below because
       * currently these are handled on page level.
       * unnecesary maybe if we move handling down */
      Attr.on_copy(_ => Effect.Stop_propagation),
      Attr.on_cut(_ => Effect.Stop_propagation),
      Attr.on_paste(_ => Effect.Stop_propagation),
    ],
    [Node.text(text)],
  );

let view = (_, ~info, ~local as _, ~parent) => {
  let text = info.syntax |> get |> Form.strip_quotes;
  Node.div(
    ~attrs=[Attr.classes(["wrapper"])],
    [
      Node.div(
        ~attrs=[Attr.classes(["cols", "code"])],
        [Node.text("·")] @ [textarea(info.id, ~parent, text)],
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
    Block({
      row: StringUtil.num_lines(str),
      /* +2 for left and right padding */
      col: 2 + StringUtil.max_line_width(str),
    });
  };
  let update = (model, _) => model;
  let view = view;
  let focus = ((id: Id.t, d: option(Direction.t))) => {
    JsUtil.get_elem_by_id(of_id(id))##focus;
    switch (d) {
    | None
    | Some(Left) => ()
    | Some(Right) =>
      JsUtil.TextArea.set_caret_to_end(JsUtil.TextArea.get(of_id(id)))
    };
  };
};
