open Util;
open Virtual_dom.Vdom;
open ProjectorInterface;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = string;
[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | SetString(string);
[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) = unit;

let key_handler = (id, ~parent, evt) => {
  open Effect;
  let key = Key.mk(KeyDown, evt);
  switch (key.key) {
  | D("ArrowRight" | "ArrowDown")
      when WebUtil.TextArea.is_last_pos(Id.cls(id)) =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([parent(Escape(Right)), Stop_propagation, Prevent_default]);
  /* Prevent_default above and below is necessary to prevent scrolling */
  | D("ArrowLeft" | "ArrowUp")
      when WebUtil.TextArea.is_first_pos(Id.cls(id)) =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([parent(Escape(Left)), Stop_propagation, Prevent_default]);
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
    (
      id,
      ~inject: action('a) => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      text: string,
    ) =>
  Node.textarea(
    ~attrs=[
      Attr.id(Id.cls(id)),
      Attr.on_keydown(key_handler(id, ~parent)),
      Attr.on_input((_, str) => Effect.(Many([inject(SetString(str))]))),
      Attr.string_property("value", text),
    ],
    [],
  );

module M =
       (Editor: EditorInterface.EDITOR)

         : (
           ProjectorInterface.PROJECTOR with
             type model' = model(Editor.model) and
             type action' = action(Editor.action) and
             type focus' = focus(Editor.focus) and
             type editor_model = Editor.model
       ) => {
  let kind = ProjectorKind.TextArea;
  type editor_model = Editor.model;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model' = model(Editor.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action' = action(Editor.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus' = focus(Editor.focus);

  let mk =
      (any: Language.Any.t, _ed: unit => option(Editor.model))
      : option(model') =>
    switch (any) {
    | Exp({term: Atom(String(str)), _}) =>
      Some(StringUtil.unescape_linebreaks(str))
    | _ => None
    };

  let dynamics = false;

  let placeholder = (~common as _, ~id as _, model: model') =>
    ProjectorShape.{
      vertical: Block(StringUtil.num_linebreaks(model)),
      /* +2 for left and right padding */
      horizontal: 2 + StringUtil.max_line_width(model),
    };

  let update = (~common as _, ~sort as _, ~id as _, _model, SetString(s)) => s;

  let mk_term = (~sort as _, ~prev, m: model'): (model', Calc.t(Any.t)) => {
    (
      m,
      Calc.set(
        ~eq=Language.Any.fast_equal,
        Exp(
          Atom(String(StringUtil.escape_linebreaks(m)))
          |> Language.Exp.fresh,
        ),
        prev,
      ),
    );
  };

  let calculate = Defaults.calculate;

  let get_cursor_info = Defaults.get_cursor_info;

  let view =
      (
        ~common as _,
        ~inject,
        ~escape,
        ~take_focus,
        ~focus as _,
        ~info,
        model: model',
      ) => {
    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["wrapper"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["cols", "code"])],
            [Node.text("·")]
            @ [textarea(info.id, ~inject, ~parent=escape, model)],
          ),
        ],
      ),
      ~enter_left=
        Ui_effect.Many([
          take_focus(),
          Ui_effect.of_sync_fun(
            () =>
              WebUtil.TextArea.set_caret_to_start(
                WebUtil.TextArea.get(Id.cls(info.id)),
              ),
            (),
          ),
          Effect.Prevent_default,
        ]),
      ~enter_right=
        Ui_effect.Many([
          take_focus(),
          Ui_effect.of_sync_fun(
            () =>
              WebUtil.TextArea.set_caret_to_end(
                WebUtil.TextArea.get(Id.cls(info.id)),
              ),
            (),
          ),
          Effect.Prevent_default,
        ]),
    );
  };
};
