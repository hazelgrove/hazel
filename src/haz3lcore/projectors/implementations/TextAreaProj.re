open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

// let string_of = (any: Any.t): option(string) =>
//   switch (any) {
//   | Exp({term: Atom(String(s)), _}) =>
//     Some(StringUtil.unescape_linebreaks(s))
//   | _ => None
//   };
// let get = (info: info): string =>
//   switch (info.syntax |> info.utility.seg_to_term) {
//   | Some(s) =>
//     switch (string_of(s)) {
//     | Some(s) => s
//     | None => failwith("TextArea: get: Not string literal")
//     }
//   | None => failwith("TextArea: get: Not string literal")
//   };
// let put = (info, s: string): Base.segment('p) =>
//   switch (
//     info.utility.lift_syntax(
//       fun
//       | Exp(any) =>
//         Exp({
//           ...any,
//           term: Atom(String(StringUtil.escape_linebreaks(s))),
//         })
//       | _any => failwith("TextArea: put: not string literal"),
//       info.syntax,
//     )
//   ) {
//   | Some(s) => s
//   | None => failwith("TextArea: put: lift failed")
//   };
// let key_handler = (id, ~parent, evt) => {
//   open Effect;
//   let key = Key.mk(KeyDown, evt);
//   switch (key.key) {
//   | D("ArrowRight" | "ArrowDown") when Web.TextArea.is_last_pos(Id.cls(id)) =>
//     JsUtil.get_elem_by_id(Id.cls(id))##blur;
//     Many([parent(Escape(Right)), Stop_propagation]);
//   | D("ArrowLeft" | "ArrowUp") when Web.TextArea.is_first_pos(Id.cls(id)) =>
//     JsUtil.get_elem_by_id(Id.cls(id))##blur;
//     Many([parent(Escape(Left)), Stop_propagation]);
//   /* Defer to parent editor undo for now */
//   | D("z" | "Z" | "y" | "Y") when Key.ctrl_held(evt) || Key.meta_held(evt) =>
//     Many([Prevent_default])
//   | D("z" | "Z")
//       when Key.shift_held(evt) && (Key.ctrl_held(evt) || Key.meta_held(evt)) =>
//     Many([Prevent_default])
//   | D("\"") =>
//     /* Hide quotes from both the textarea and parent editor */
//     Many([Prevent_default, Stop_propagation])
//   | _ => Stop_propagation
//   };
// };
// let textarea =
//     (info, ~parent: external_action('p) => Ui_effect.t(unit), text: string) =>
//   Node.textarea(
//     ~attrs=[
//       Attr.id(Id.cls(info.id)),
//       Attr.on_keydown(key_handler(info.id, ~parent)),
//       Attr.on_input((_, str) =>
//         Effect.(Many([parent(SetSyntax(str |> put(info)))]))
//       ),
//       /* Note: adding these handlers below because
//        * currently these are handled on page level.
//        * unnecesary maybe if we move handling down */
//       Attr.on_copy(_ => Effect.Stop_propagation),
//       Attr.on_cut(_ => Effect.Stop_propagation),
//       Attr.on_paste(_ => Effect.Stop_propagation),
//       Attr.string_property("value", text),
//     ],
//     [],
//   );
// [@deriving (show({with_path: false}), sexp, yojson)]
// type model('ed) = unit;
// [@deriving (show({with_path: false}), sexp, yojson)]
// type action = unit;
// let init = (any: Term.Any.t) =>
//   switch (string_of(any)) {
//   | Some(_) => Some()
//   | None => None
//   };
// let focus_keyboard = (id: Id.t, d: Direction.t) => {
//   JsUtil.get_elem_by_id(Id.cls(id))##focus;
//   switch (d) {
//   | Left => Web.TextArea.set_caret_to_start(Web.TextArea.get(Id.cls(id)))
//   | Right => Web.TextArea.set_caret_to_end(Web.TextArea.get(Id.cls(id)))
//   };
// };
// let focus_pointer = (id: Id.t) => {
//   JsUtil.get_elem_by_id(Id.cls(id))##focus;
// };
// let focusable =
//   Focusable.{
//     pointer: Some(focus_pointer),
//     keyboard: Some(focus_keyboard),
//   };
// let dynamics = false;
// let placeholder = (_, info) => {
//   let str = info |> get;
//   ProjectorShape.{
//     vertical: Block(StringUtil.num_linebreaks(str)),
//     /* +2 for left and right padding */
//     horizontal: 2 + StringUtil.max_line_width(str),
//   };
// };
// let update = (model, _, _) => model;
// let view = (_, info, ~local as _, ~parent, ~view_seg as _) =>
//   View.mk(
//     Node.div(
//       ~attrs=[Attr.classes(["wrapper"])],
//       [
//         Node.div(
//           ~attrs=[Attr.classes(["cols", "code"])],
//           [Node.text("·")] @ [textarea(info, ~parent, info |> get)],
//         ),
//       ],
//     ),
//   );
// let mk_term = mk_term_default;
// let methods = {
//   init,
//   focusable,
//   dynamics,
//   placeholder,
//   view,
//   update,
//   mk_term,
// };

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
  | D("ArrowRight" | "ArrowDown") when Web.TextArea.is_last_pos(Id.cls(id)) =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([parent(Escape(Right)), Stop_propagation]);
  | D("ArrowLeft" | "ArrowUp") when Web.TextArea.is_first_pos(Id.cls(id)) =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
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
    (
      info,
      ~inject: action('a) => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      text: string,
    ) =>
  Node.textarea(
    ~attrs=[
      Attr.id(Id.cls(info.id)),
      Attr.on_keydown(key_handler(info.id, ~parent)),
      Attr.on_input((_, str) => Effect.(Many([inject(SetString(str))]))),
      Attr.string_property("value", text),
    ],
    [],
  );

let methods:
  ProjectorBase.methods(
    model('ed),
    action('ed_a),
    focus('ed_f),
    'ed,
    'ed_a,
    'ed_f,
  ) = {
  init: (any: Term.Any.t, _ed) =>
    switch (any) {
    | Exp({term: Atom(String(str)), _}) => Some(str)
    | _ => None
    },
  focusable: Focusable.non,
  dynamics: false,
  placeholder: (~ed_str as _, str, _) => {
    ProjectorShape.{
      vertical: Block(StringUtil.num_linebreaks(str)),
      /* +2 for left and right padding */
      horizontal: 2 + StringUtil.max_line_width(str),
    };
  },
  update: (~update_ed as _, ~common as _, ~sort as _, _, _, SetString(s)) => s,
  mk_term: (~mk_term_ed as _, ~sort, ~prev, m) => {
    (
      m,
      Calc.set(
        ~eq=Any.fast_equal,
        switch (sort) {
        | Sort.Exp => Exp(Atom(String(m)) |> Exp.fresh)
        | Sort.Pat => Pat(Atom(String(m)) |> Pat.fresh)
        | _ => Any()
        },
        prev,
      ),
    );
  },
  calculate: (~calculate_ed as _, ~common as _, model) => model,
  view:
    (
      ~common as _,
      ~ed_str as _,
      ~view_ed as _,
      ~view_editable as _,
      ~mk_ed as _,
      ~local,
      ~parent,
      ~focus as _,
      ~focussed as _,
      model,
      info,
    ) =>
    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["wrapper"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["cols", "code"])],
            [Node.text("·")]
            @ [textarea(info, ~inject=local, ~parent, model)],
          ),
        ],
      ),
    ),
  sexp_of_model,
  model_of_sexp,
  yojson_of_model,
  model_of_yojson,
  sexp_of_action,
  action_of_sexp,
  yojson_of_action,
  action_of_yojson,
  sexp_of_focus,
  focus_of_sexp,
  yojson_of_focus,
  focus_of_yojson,
};
