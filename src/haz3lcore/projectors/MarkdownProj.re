open ProjectorBase;
open Virtual_dom.Vdom;
open Util;
open Util.WebUtil;

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
let string_of = (any: Language.Any.t): option(string) =>
  switch (any) {
  | Exp({term: Atom(String(s)), _}) =>
    Some(StringUtil.unescape_linebreaks(s))
  | _ => None
  };

let get = (info: info): string =>
  switch (info.syntax |> info.utility.seg_to_term) {
  | Some(s) =>
    switch (string_of(s)) {
    | Some(s) => s
    | None => failwith("TextArea: get: Not string literal")
    }
  | None => failwith("TextArea: get: Not string literal")
  };
let put = (s: string): Piece.t => s |> mk_mono(Exp);

let put = (str: string): external_action =>
  SetSyntax([str |> Form.string_quote |> put]);

let is_last_pos = id =>
  WebUtil.TextArea.caret_at_end(WebUtil.TextArea.get(of_id(id)));
let is_first_pos = id =>
  WebUtil.TextArea.caret_at_start(WebUtil.TextArea.get(of_id(id)));

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
  /* Defer to parent editor undo for now */
  | D("z" | "Z" | "y" | "Y") when Key.ctrl_held(evt) || Key.meta_held(evt) =>
    Many([Prevent_default])
  | D("z" | "Z")
      when Key.shift_held(evt) && (Key.ctrl_held(evt) || Key.meta_held(evt)) =>
    Many([Prevent_default])
  | D("\"") =>
    /* Hide quotes from both the textarea and parent editor */
    Many([Prevent_default, Stop_propagation])
  | _ => Stop_propagation
  };
};

let safe_html_to_node = (html_string: string): Node.t =>
  Node.div(~attrs=[Attr.create("innerHTML", html_string)], []);
let textarea =
    (id, ~parent as _: external_action => Ui_effect.t(unit), text: string) => {
  let foo = Omd.of_string(text);
  let bar = Omd.to_html(foo);
  let size =
    Css_gen.concat([
      Css_gen.overflow(`Auto),
      // Css_gen.height(`Px(int_of_float(30. *. font_metrics.row_height))),
      // Css_gen.width(`Px(int_of_float(150. *. font_metrics.col_width))),
    ]);
  // Node.innerHtml(bar);
  let foo =
    Node.inner_html(
      ~attrs=[Attr.id(of_id(id)), Attr.style(size)],
      ~this_html_is_sanitized_and_is_totally_safe_trust_me=bar, // ;)
      ~tag="div",
    );
  foo();
};

let view = (~info, ~local as _, ~parent) => {
  let text = info |> get |> Form.strip_quotes;
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
  let init = (any: Language.Any.t) => {
    print_endline("MarkdownProj.init");
    print_endline(
      "String_of: " ++ Option.value(~default="None", string_of(any)),
    );
    switch (string_of(any)) {
    | Some(_) => Some()
    | None => None
    };
  };
  let dynamics = false;
  let focusable =
    Focusable.{
      pointer: None,
      keyboard: None,
    };
  let placeholder = (_, info) => {
    let str = info |> get;
    ProjectorCore.Shape.{
      vertical: Block(StringUtil.num_linebreaks(str) * 2),
      /* +2 for left and right padding */
      horizontal: 2 + StringUtil.max_line_width(str),
    };
  };
  let update = (model, _, _) => model;
  let view = view;

  let view =
      (
        _,
        info,
        ~local: action => Ui_effect.t(unit),
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _: View.seg,
      ) =>
    View.mk(view(~info, ~local, ~parent));
};
