open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

let string_of = (any: Any.t): option(string) =>
  switch (any) {
  | Exp({term: String(s), _}) => Some(StringUtil.unescape_linebreaks(s))
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
      fun
      | Exp(any) =>
        Exp({...any, term: String(StringUtil.escape_linebreaks(s))})
      | _any => failwith("TextArea: put: not string literal"),
      info.syntax,
    )
  ) {
  | Some(s) => s
  | None => failwith("TextArea: put: lift failed")
  };

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
  let init = ();
  let can_project = any => string_of(any) != None;
  let can_focus = true;
  let dynamics = false;
  let placeholder = (_, info) => {
    let str = info |> get;
    ProjectorCore.Shape.{
      vertical: Block(StringUtil.num_linebreaks(str)),
      /* +2 for left and right padding */
      horizontal: 2 + StringUtil.max_line_width(str),
    };
  };
  let update = (model, _, _) => model;

  let view = (_, info, ~local as _, ~parent, ~view_seg as _) =>
    Node.div(
      ~attrs=[Attr.classes(["wrapper"])],
      [
        Node.div(
          ~attrs=[Attr.classes(["cols", "code"])],
          [Node.text("·")] @ [textarea(info, ~parent, info |> get)],
        ),
      ],
    );

  let offside_view = Option.None;
  let overlay_view = Option.None;
  let underlay_view = Option.None; //TODO

  let focus = ((id: Id.t, d: option(Direction.t))) => {
    JsUtil.get_elem_by_id(Id.cls(id))##focus;
    switch (d) {
    | None => ()
    | Some(Left) =>
      Web.TextArea.set_caret_to_start(Web.TextArea.get(Id.cls(id)))
    | Some(Right) =>
      Web.TextArea.set_caret_to_end(Web.TextArea.get(Id.cls(id)))
    };
  };
};
