open Util;
open Virtual_dom.Vdom;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;

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
        Effect.(Many([parent(SetSyntax(str |> TextAreaProj.put(info)))]))
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

module V: ProjectorView = {
  module L = TextAreaProj.M;

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

  let view = ({info, parent, _}: View.args(L.model, L.action)) =>
    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["wrapper"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["cols", "code"])],
            [Node.text("·")]
            @ [textarea(info, ~parent, info |> TextAreaProj.get)],
          ),
        ],
      ),
    );
};
