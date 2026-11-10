open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

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

let put = (info, s: string): Base.segment =>
  switch (
    info.utility.lift_syntax(
      ~inline=true,
      fun
      | Exp(any) =>
        Exp({
          ...any,
          term: Atom(String(StringUtil.escape_linebreaks(s))),
        })
      | _any => failwith("TextArea: put: not string literal"),
      info.syntax,
    )
  ) {
  | Some(s) => s
  | None => failwith("TextArea: put: lift failed")
  };

let focus_parent_editor = (id): unit => {
  FocusEffect.expect_blur();
  let el = JsUtil.get_elem_by_id(Id.cls(id));
  el##blur;
  /* After blur, give DOM focus to the parent code-editor so it
   * receives subsequent key events. Without this, focus goes to
   * <body> and the editor stops responding to keys. */
  switch (JsUtil.find_ancestor_with_class(el, "code-editor")) {
  | Some(editor_el) => editor_el##focus
  | None => JsUtil.focus_page()
  };
};

/* Only the deletion that makes multiline text single-line changes its
 * presentation. Existing single-line projectors and ordinary edits stay put.
 * Browser selection offsets are UTF-16; the resulting code caret is graphemes. */
let joined_string = (text: string, start: int, finish: int, d: Direction.t) => {
  open Js_of_ocaml;
  let value = Js.string(text);
  let len = value##.length;
  let (left, right) =
    start == finish
      ? switch (d) {
        | Left => (start - 1, finish)
        | Right => (start, finish + 1)
        }
      : (start, finish);
  if (left < 0 || right > len || left >= right) {
    None;
  } else {
    let removed = Js.to_string(value##substring(left, right));
    let joined =
      Js.to_string(value##substring(0, left))
      ++ Js.to_string(value##substring(right, len));
    if (String.contains(removed, '\n')
        && !String.contains(joined, '\n')
        && Token.is_string(Token.string_quote(joined))
        && StringUtil.unescape_linebreaks(joined) == joined) {
      let caret = HazelString.grapheme_of_utf16(joined, left);
      /* Joining can form a new cluster (e.g. a + combining acute). Snap to
       * its end rather than leaving a caret inside the resulting grapheme. */
      let caret =
        HazelString.utf16_of_grapheme(joined, caret) < left
          ? caret + 1 : caret;
      Some((joined, caret));
    } else {
      None;
    };
  };
};

let key_handler = (id, ~parent, evt) => {
  open Effect;
  let key = Key.mk(KeyDown, evt);

  switch (key.key) {
  | D(("Backspace" | "Delete") as deletion)
      when key.meta == Up && key.ctrl == Up && key.alt == Up =>
    let area = WebUtil.TextArea.get(Id.cls(id));
    let start = area##.selectionStart;
    let finish = area##.selectionEnd;
    let d = deletion == "Backspace" ? Direction.Left : Right;
    switch (
      joined_string(Js_of_ocaml.Js.to_string(area##.value), start, finish, d)
    ) {
    | Some(_) =>
      Many([
        Prevent_default,
        Stop_propagation,
        parent(JoinTextLines(start, finish, d)),
      ])
    | None => Stop_propagation
    };
  | D("ArrowRight" | "ArrowDown")
      when
        key.shift == Up
        && key.alt == Up
        && key.ctrl == Up
        && key.meta == Up
        && WebUtil.TextArea.is_last_pos(Id.cls(id)) =>
    focus_parent_editor(id);
    Many([parent(Escape(Right)), Stop_propagation]);
  | D("ArrowLeft" | "ArrowUp")
      when
        key.shift == Up
        && key.alt == Up
        && key.ctrl == Up
        && key.meta == Up
        && WebUtil.TextArea.is_first_pos(Id.cls(id)) =>
    focus_parent_editor(id);
    Many([parent(Escape(Left)), Stop_propagation]);
  /* Defer to parent editor undo for now */
  | D("z" | "Z" | "y" | "Y") when Key.ctrl_held(evt) || Key.meta_held(evt) =>
    FocusEffect.schedule_editor_if_missing(id);
    Many([Prevent_default]);
  | D("z" | "Z")
      when Key.shift_held(evt) && (Key.ctrl_held(evt) || Key.meta_held(evt)) =>
    Many([Prevent_default])
  | D("\"") =>
    /* Hide quotes from both the textarea and parent editor */
    Many([Prevent_default, Stop_propagation])
  | _ => Stop_propagation
  };
};

let textarea =
    (info, ~parent: external_action => Ui_effect.t(unit), text: string) =>
  Node.textarea(
    ~attrs=[
      Attr.id(Id.cls(info.id)),
      Attr.classes(["text-projector-input"]),
      /* Keep pointer and focus events inside the textarea. */
      Attr.on_pointerdown(_ => Effect.Stop_propagation),
      Attr.on_focus(_ => Effect.Stop_propagation),
      Attr.on_blur(_ => Effect.Stop_propagation),
      Attr.on_keydown(key_handler(info.id, ~parent)),
      /* Capture the pre-edit native selection for undo, including IME,
       * paste and cut (which need not originate from a keydown). */
      Attr.property(
        "onbeforeinput",
        Js_of_ocaml.Js.Unsafe.inject(
          Js_of_ocaml.Js.wrap_callback(_ =>
            FocusEffect.before_textarea_input(info.id)
          ),
        ),
      ),
      Attr.on_input((_, str) => {
        FocusEffect.after_textarea_input();
        FocusEffect.remember_textarea(info.id);
        Effect.(Many([parent(SetSyntax(str |> put(info)))]));
      }),
      /* Note: adding these handlers below because
       * currently these are handled on page level.
       * unnecesary maybe if we move handling down */
      Attr.on_copy(_ => Effect.Stop_propagation),
      Attr.on_cut(_ => Effect.Stop_propagation),
      Attr.on_paste(_ => Effect.Stop_propagation),
      Attr.string_property("value", text),
    ],
    [],
  );

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Language.Any.t) =>
    switch (string_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let focus_keyboard = (id: Id.t, d: Direction.t) => {
    JsUtil.get_elem_by_id(Id.cls(id))##focus;
    switch (d) {
    | Left =>
      WebUtil.TextArea.set_caret_to_start(WebUtil.TextArea.get(Id.cls(id)))
    | Right =>
      WebUtil.TextArea.set_caret_to_end(WebUtil.TextArea.get(Id.cls(id)))
    };
  };

  let focus_pointer = (id: Id.t) => {
    JsUtil.get_elem_by_id(Id.cls(id))##focus;
  };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: Some(focus_keyboard),
    };
  let elaborate_syntax = false;
  let placeholder = (_, info) => {
    let str = info |> get;
    /* Rows and widest line in display columns (StringUtil.max_line_width
     * counts grapheme clusters, which undercounts wide glyphs). */
    let (rows, cols) = Unicode.Width.bounding_box_for(str);
    ProjectorCore.Shape.{
      vertical: Block(rows),
      /* +2 for left and right padding */
      horizontal: 2 + cols,
    };
  };
  let update = (model, _, _) => model;
  let error = (_, _): option(ProjectorBase.error) => None;

  let view = ({info, parent, _}: View.args(model, action)) =>
    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["wrapper"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["cols", "code"])],
            [Node.text("·")] @ [textarea(info, ~parent, info |> get)],
          ),
        ],
      ),
    );
};
