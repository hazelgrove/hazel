open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Js_of_ocaml;
open Language;

/* AutomergeWriteBack Projector: reads an evaluated Hazel value
   (via dynamics = true), converts it to JSON, and writes it back
   to an automerge document. Paired with AutomergeProj (which reads
   automerge → Hazel), this creates a reactive loop:
   AutomergeProj reads doc → Hazel augment function → AutomergeWriteBackProj writes to doc */

/* --- Write state management (module-level, persistent across re-renders) --- */

type write_status =
  | Idle
  | Wrote
  | Waiting
  | Error(string);

type write_state = {
  mutable last_json: option(string),
  mutable status: write_status,
};

let write_states: ref(Id.Map.t(write_state)) = ref(Id.Map.empty);

let get_write_state = (id: Id.t): write_state =>
  switch (Id.Map.find_opt(id, write_states^)) {
  | Some(ws) => ws
  | None =>
    let ws = {
      last_json: None,
      status: Waiting,
    };
    write_states := Id.Map.add(id, ws, write_states^);
    ws;
  };

/* --- Projector module --- */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {url: string};

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetUrl(string);

  let init = (a: Language.Any.t): option(model) =>
    switch (a) {
    | Exp(_) => Some({url: ""})
    | _ => None
    };

  let input_id = (id: Id.t): string => Id.cls(id) ++ "-wb-input";

  let focus_pointer = (id: Id.t) => {
    JsUtil.get_elem_by_id(input_id(id))##focus;
  };

  let focus_keyboard = (id: Id.t, d: Direction.t) => {
    let el = JsUtil.get_elem_by_id(input_id(id));
    el##focus;
    switch (d) {
    | Left =>
      Js.Unsafe.set(el, "selectionStart", 0);
      Js.Unsafe.set(el, "selectionEnd", 0);
    | Right =>
      let len: int = Js.Unsafe.get(Js.Unsafe.get(el, "value"), "length");
      Js.Unsafe.set(el, "selectionStart", len);
      Js.Unsafe.set(el, "selectionEnd", len);
    };
  };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: Some(focus_keyboard),
    };
  let dynamics = true;

  let url_placeholder = "automerge:<doc-url>";

  let placeholder = (m, _info) => {
    let url_len = String.length(m.url);
    let display_len = max(String.length(url_placeholder), url_len);
    /* +1 focus dot; +2 arrow icon; +2 status dot + margins */
    ProjectorCore.Shape.inline(display_len + 6);
  };

  let update = (_m: model, _info: info, action: action): model =>
    switch (action) {
    | SetUrl(url) => {url: url}
    };

  let select_sample = (info: info): option(Sample.t) =>
    switch (info.dynamics) {
    | Some(x) =>
      switch (x.samples) {
      | [sample, ..._] => Some(sample)
      | _ => None
      }
    | _ => None
    };

  let view =
      ({model, info, local, parent, _}: View.args(model, action)): View.t => {
    let ws = get_write_state(info.id);

    /* Try to extract and write the evaluated value */
    if (String.length(model.url) > 0) {
      switch (select_sample(info)) {
      | Some(sample) =>
        switch (Automerge.exp_to_json_string(sample.value)) {
        | Ok(json_string) =>
          /* Only write if the JSON has changed (loop prevention) */
          let should_write =
            switch (ws.last_json) {
            | Some(prev) => prev != json_string
            | None => true
            };
          if (should_write) {
            Automerge.write_to_doc(model.url, json_string);
            ws.last_json = Some(json_string);
            ws.status = Wrote;
          } else {
            ws.status = Idle;
          };
        | Error(msg) => ws.status = Error(msg)
        }
      | None => ws.status = Waiting
      };
    };

    let status_cls =
      switch (ws.status) {
      | Idle => "write-idle"
      | Wrote => "write-active"
      | Waiting => "write-waiting"
      | Error(_) => "write-error"
      };

    let input_at_start = () => {
      let el = JsUtil.get_elem_by_id(input_id(info.id));
      let pos: int = Js.Unsafe.get(el, "selectionStart");
      pos == 0;
    };

    let input_at_end = () => {
      let el = JsUtil.get_elem_by_id(input_id(info.id));
      let pos: int = Js.Unsafe.get(el, "selectionStart");
      let len: int = Js.Unsafe.get(Js.Unsafe.get(el, "value"), "length");
      pos == len;
    };

    let key_handler = evt => {
      open Effect;
      let key = Key.mk(KeyDown, evt);
      switch (key.key) {
      | D("ArrowRight") when input_at_end() =>
        JsUtil.get_elem_by_id(input_id(info.id))##blur;
        Many([parent(Escape(Right)), Stop_propagation]);
      | D("ArrowLeft") when input_at_start() =>
        JsUtil.get_elem_by_id(input_id(info.id))##blur;
        Many([parent(Escape(Left)), Stop_propagation]);
      | D("Escape") =>
        JsUtil.get_elem_by_id(input_id(info.id))##blur;
        Many([parent(Escape(Right)), Stop_propagation]);
      | _ => Stop_propagation
      };
    };

    let url_input =
      Node.input(
        ~attrs=[
          Attr.id(input_id(info.id)),
          Attr.type_("text"),
          Attr.class_("automerge-wb-url-input"),
          Attr.placeholder(url_placeholder),
          Attr.string_property("value", model.url),
          Attr.on_input((_evt, value) => local(SetUrl(value))),
          Attr.on_keydown(key_handler),
          Attr.on_copy(_ => Effect.Stop_propagation),
          Attr.on_cut(_ => Effect.Stop_propagation),
          Attr.on_paste(_ => Effect.Stop_propagation),
          Attr.style(
            Css_gen.concat([
              Css_gen.create(~field="width", ~value="100%"),
              Css_gen.create(~field="font-size", ~value="inherit"),
              Css_gen.create(~field="font-family", ~value="inherit"),
            ]),
          ),
        ],
        (),
      );

    let status_title =
      switch (ws.status) {
      | Idle => "Idle (no change)"
      | Wrote => "Wrote to document"
      | Waiting => "Waiting for dynamics"
      | Error(msg) => "Error: " ++ msg
      };
    let status_dot =
      Node.span(
        ~attrs=[
          Attr.classes(["wb-status-dot", status_cls]),
          Attr.title(status_title),
        ],
        [],
      );

    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["wrapper"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["cols", "code"])],
            [
              Node.text({js|·|js}),
              Node.span(
                ~attrs=[Attr.class_("wb-arrow")],
                [Node.text({js|⬆|js})],
              ),
              url_input,
              status_dot,
            ],
          ),
        ],
      ),
    );
  };
};
