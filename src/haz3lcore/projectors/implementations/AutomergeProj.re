open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Js_of_ocaml;

/* Automerge Projector: subscribes to an automerge document URL,
   converts incoming data to the Hazel JSON ADT type, and updates
   the underlying syntax via SetSyntax. */

/* --- Subscription management (module-level, persistent across re-renders) --- */

type subscription = {
  url: string,
  mutable on_data: Language.Exp.t => unit,
  mutable cleanup: option(unit => unit),
};

let subscriptions: ref(Id.Map.t(subscription)) = ref(Id.Map.empty);

let subscribe_to_doc =
    (id: Id.t, url: string, on_data: Language.Exp.t => unit) => {
  let sub = {
    url,
    on_data,
    cleanup: None,
  };
  subscriptions := Id.Map.add(id, sub, subscriptions^);
  let repo = Js.Unsafe.get(Js.Unsafe.global, "repo");
  let promise =
    Js.Unsafe.meth_call(
      repo,
      "find",
      [|Js.Unsafe.inject(Js.string(url))|],
    );
  ignore(
    Js.Unsafe.meth_call(
      promise,
      "then",
      [|
        Js.Unsafe.inject(
          Js.wrap_callback(handle => {
            let callback =
              Js.wrap_callback(_ => {
                let doc = Js.Unsafe.meth_call(handle, "doc", [||]);
                let json_obj = Js.Unsafe.get(Js.Unsafe.global, "JSON");
                let json_str =
                  Js.to_string(
                    Js.Unsafe.meth_call(
                      json_obj,
                      "stringify",
                      [|Js.Unsafe.inject(doc)|],
                    ),
                  );
                let yojson = Yojson.Safe.from_string(json_str);
                switch (HazelProtocol.JsonADT.yojson_to_exp(yojson)) {
                | Ok(exp) =>
                  switch (Id.Map.find_opt(id, subscriptions^)) {
                  | Some(s) => s.on_data(exp)
                  | None => ()
                  }
                | Error(err) => prerr_endline("AutomergeProj: " ++ err)
                };
              });
            let cleanup_fn = () => {
              ignore(
                Js.Unsafe.meth_call(
                  handle,
                  "off",
                  [|
                    Js.Unsafe.inject(Js.string("change")),
                    Js.Unsafe.inject(callback),
                  |],
                ),
              );
            };
            switch (Id.Map.find_opt(id, subscriptions^)) {
            | Some(s) => s.cleanup = Some(cleanup_fn)
            | None => ()
            };
            ignore(
              Js.Unsafe.meth_call(
                handle,
                "on",
                [|
                  Js.Unsafe.inject(Js.string("change")),
                  Js.Unsafe.inject(callback),
                |],
              ),
            );
            /* Also read initial doc state */
            Js.Unsafe.fun_call(
              callback,
              [|Js.Unsafe.inject(Js.undefined)|],
            );
          }),
        ),
      |],
    ),
  );
};

let ensure_subscribed =
    (id: Id.t, url: string, on_data: Language.Exp.t => unit) =>
  if (String.length(url) > 0) {
    switch (Id.Map.find_opt(id, subscriptions^)) {
    | Some(sub) when sub.url == url => sub.on_data = on_data
    | Some(sub) =>
      Option.iter(f => f(), sub.cleanup);
      subscriptions := Id.Map.remove(id, subscriptions^);
      subscribe_to_doc(id, url, on_data);
    | None => subscribe_to_doc(id, url, on_data)
    };
  };

/* --- Projector module --- */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type connection_status =
    | Disconnected
    | Connecting
    | Connected
    | Error(string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    url: string,
    status: connection_status,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetUrl(string)
    | SetStatus(connection_status);

  let init = (a: Language.Any.t): option(model) =>
    switch (a) {
    | Exp({term: Constructor("Null", _), _}) =>
      Some({
        url: "",
        status: Disconnected,
      })
    | _ => None
    };

  let put = (info, exp: Language.Exp.t): Base.segment =>
    switch (
      info.utility.lift_syntax(
        fun
        | Exp(any) =>
          Exp({
            ...any,
            term: exp.term,
          })
        | _ => failwith("AutomergeProj: put: not expression"),
        Inline.Compound,
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("AutomergeProj: put: lift failed")
    };

  let input_id = (id: Id.t): string => Id.cls(id) ++ "-input";

  let focus_pointer = (id: Id.t) => {
    JsUtil.get_elem_by_id(input_id(id))##focus;
  };

  let focus_keyboard = (id: Id.t, _d: Direction.t) => {
    JsUtil.get_elem_by_id(input_id(id))##focus;
  };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: Some(focus_keyboard),
    };
  let dynamics = false;

  let placeholder = (_m, _info) => ProjectorCore.Shape.inline(40);

  let update = (m: model, _info: info, action: action): model =>
    switch (action) {
    | SetUrl(url) => {
        ...m,
        url,
      }
    | SetStatus(status) => {
        ...m,
        status,
      }
    };

  let view =
      ({model, info, local, parent, _}: View.args(model, action)): View.t => {
    let status_indicator = {
      let (color, title) =
        switch (model.status) {
        | Disconnected => ("#999", "Disconnected")
        | Connecting => ("#fa0", "Connecting...")
        | Connected => ("#0c0", "Connected")
        | Error(msg) => ("#f00", "Error: " ++ msg)
        };
      Node.span(
        ~attrs=[
          Attr.style(
            Css_gen.concat([
              Css_gen.create(~field="display", ~value="inline-block"),
              Css_gen.create(~field="width", ~value="8px"),
              Css_gen.create(~field="height", ~value="8px"),
              Css_gen.create(~field="border-radius", ~value="50%"),
              Css_gen.create(~field="background-color", ~value=color),
              Css_gen.create(~field="margin-right", ~value="4px"),
            ]),
          ),
          Attr.title(title),
        ],
        [],
      );
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
          Attr.class_("automerge-url-input"),
          Attr.placeholder("automerge:<doc-url>"),
          Attr.string_property("value", model.url),
          Attr.on_input((_evt, value) => {local(SetUrl(value))}),
          Attr.on_keydown(key_handler),
          Attr.on_copy(_ => Effect.Stop_propagation),
          Attr.on_cut(_ => Effect.Stop_propagation),
          Attr.on_paste(_ => Effect.Stop_propagation),
          Attr.style(
            Css_gen.concat([
              Css_gen.create(~field="width", ~value="280px"),
              Css_gen.create(~field="font-size", ~value="inherit"),
              Css_gen.create(~field="font-family", ~value="inherit"),
              Css_gen.create(~field="border", ~value="1px solid #ccc"),
              Css_gen.create(~field="padding", ~value="1px 4px"),
            ]),
          ),
        ],
        (),
      );

    let on_data = (exp: Language.Exp.t) => {
      let seg = put(info, exp);
      Bonsai.Effect.Expert.handle(
        Effect.Many([
          local(SetStatus(Connected)),
          parent(SetSyntax(seg)),
        ]),
      );
    };

    ensure_subscribed(info.id, model.url, on_data);

    if (String.length(model.url) > 0 && model.status == Disconnected) {
      Bonsai.Effect.Expert.handle(local(SetStatus(Connecting)));
    };

    View.mk(
      Node.span(
        ~attrs=[Attr.class_("automerge-projector")],
        [status_indicator, url_input],
      ),
    );
  };
};
