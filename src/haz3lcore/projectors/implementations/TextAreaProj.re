open Util_web;
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
  let el = JsUtil.get_elem_by_id(Id.cls(id));
  el##blur;
  /* After blur, give DOM focus to the parent code-editor so it
   * receives subsequent key events. Without this, focus goes to
   * <body> and the editor stops responding to keys. */
  switch (JsUtil.find_ancestor_with_class(el, "code-editor")) {
  | Some(editor_el) => editor_el##focus
  | None => JsUtil.focus_clipboard_shim()
  };
};

let key_handler = (id, ~parent, evt) => {
  open Effect;
  let key = Key.mk(KeyDown, evt);

  switch (key.key) {
  | D("ArrowRight" | "ArrowDown")
      when WebUtil.TextArea.is_last_pos(Id.cls(id)) =>
    focus_parent_editor(id);
    Many([parent(Escape(Right)), Stop_propagation]);
  | D("ArrowLeft" | "ArrowUp")
      when WebUtil.TextArea.is_first_pos(Id.cls(id)) =>
    focus_parent_editor(id);
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

let textarea =
    (info, ~parent: external_action => Ui_effect.t(unit), text: string) =>
  Node.textarea(
    ~attrs=[
      Attr.id(Id.cls(info.id)),
      Attr.on_keydown(key_handler(info.id, ~parent)),
      Attr.on_input((_, str) =>
        Effect.(Many([parent(SetSyntax(str |> put(info)))]))
      ),
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
  let dynamics = false;
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
