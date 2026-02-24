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
  mutable on_error: string => unit,
  mutable cleanup: option(unit => unit),
  mutable handle: option(Js.t(Automerge.handle)),
};

let subscriptions: ref(Id.Map.t(subscription)) = ref(Id.Map.empty);

let subscribe_to_doc =
    (
      id: Id.t,
      url: string,
      on_data: Language.Exp.t => unit,
      on_error: string => unit,
    ) => {
  let sub = {
    url,
    on_data,
    on_error,
    cleanup: None,
    handle: None,
  };
  subscriptions := Id.Map.add(id, sub, subscriptions^);

  /* Look up the document by URL via the global automerge repo. */
  let repo = Automerge.get_repo();
  let promise = repo##find(Js.string(url));

  /* Once the handle resolves, wire up the change listener. */
  let then_result =
    promise##then_(
      Js.wrap_callback((handle: Js.t(Automerge.handle)) => {
        /* Called on every "change" event (and once for the initial read).
           Converts the live JS document to a Hazel expression. */
        let callback =
          Js.wrap_callback(_ => {
            switch (Automerge.doc_to_exp(handle)) {
            | Ok(exp) =>
              switch (Id.Map.find_opt(id, subscriptions^)) {
              | Some(s) => s.on_data(exp)
              | None => ()
              }
            | Error(err) =>
              switch (Id.Map.find_opt(id, subscriptions^)) {
              | Some(s) => s.on_error(err)
              | None => ()
              }
            }
          });

        /* Store a cleanup function that unsubscribes from changes. */
        let cleanup_fn = () => {
          ignore(
            handle##off_(Js.string("change"), Js.Unsafe.inject(callback)),
          );
        };
        switch (Id.Map.find_opt(id, subscriptions^)) {
        | Some(s) =>
          s.cleanup = Some(cleanup_fn);
          s.handle = Some(handle);
        | None => ()
        };

        /* Subscribe to future changes. */
        ignore(
          handle##on_(Js.string("change"), Js.Unsafe.inject(callback)),
        );

        /* Fire the callback once immediately to read the initial doc state. */
        Js.Unsafe.fun_call(callback, [|Js.Unsafe.inject(Js.undefined)|]);
      }),
    );
  ignore(
    Js.Unsafe.meth_call(
      then_result,
      "catch",
      [|
        Js.Unsafe.inject(
          Js.wrap_callback((_err: Js.Unsafe.any) => {
            switch (Id.Map.find_opt(id, subscriptions^)) {
            | Some(s) => s.on_error("Failed to find document")
            | None => ()
            }
          }),
        ),
      |],
    ),
  );
};

let ensure_subscribed =
    (
      id: Id.t,
      url: string,
      on_data: Language.Exp.t => unit,
      on_error: string => unit,
    ) =>
  if (String.length(url) > 0) {
    switch (Id.Map.find_opt(id, subscriptions^)) {
    | Some(sub) when sub.url == url =>
      sub.on_data = on_data;
      sub.on_error = on_error;
    | Some(sub) =>
      Option.iter(f => f(), sub.cleanup);
      subscriptions := Id.Map.remove(id, subscriptions^);
      subscribe_to_doc(id, url, on_data, on_error);
    | None => subscribe_to_doc(id, url, on_data, on_error)
    };
  };

/* --- URL storage in syntax (mirrors TextArea pattern) --- */

let get_url = (info: info): string =>
  switch (info.syntax |> info.utility.seg_to_term) {
  | Some(Exp({term: Atom(String(s)), _})) => s
  | _ => ""
  };

let put_url = (info, url: string): Base.segment =>
  switch (
    info.utility.lift_syntax(
      fun
      | Exp(any) =>
        Exp({
          ...any,
          term: Atom(String(url)),
        })
      | _ => failwith("AutomergeProj: put_url: not expression"),
      Inline.Compound,
      info.syntax,
    )
  ) {
  | Some(s) => s
  | None => failwith("AutomergeProj: put_url: lift failed")
  };

/* --- Projector module --- */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type last_load =
    | Succeeded
    | Failed;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    url: string,
    last_load: option(last_load),
    hot_reload: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetUrl(string)
    | SetLastLoad(last_load)
    | ToggleHotReload;

  let init = (a: Language.Any.t): option(model) =>
    switch (a) {
    | Exp({term: Constructor("Null", _), _}) =>
      Some({
        url: "",
        last_load: None,
        hot_reload: true,
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
  let dynamics = false;

  let placeholder = (m, _info) => {
    let url_len = String.length(m.url);
    /* 19 = placeholder text length; +1 focus dot; +2 status indicator;
       +2 reload btn; +5 toggle + margins */
    let display_len = max(19, url_len);
    ProjectorCore.Shape.inline(display_len + 10);
  };

  let update = (m: model, _info: info, action: action): model =>
    switch (action) {
    | SetUrl(url) => {
        ...m,
        url,
        last_load: String.length(url) == 0 ? None : m.last_load,
      }
    | SetLastLoad(ll) => {
        ...m,
        last_load: Some(ll),
      }
    | ToggleHotReload => {
        ...m,
        hot_reload: !m.hot_reload,
      }
    };

  let view =
      ({model, info, local, parent, _}: View.args(model, action)): View.t => {
    let status_indicator = {
      let (color, title) =
        switch (model.last_load) {
        | None => ("#999", "No data loaded")
        | Some(Succeeded) => ("#0c0", "Last load succeeded")
        | Some(Failed) => ("#f00", "Last load failed")
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
              Css_gen.create(~field="margin-left", ~value="4px"),
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
          Attr.on_input((_evt, value) =>
            Effect.(
              Many([
                local(SetUrl(value)),
                parent(SetSyntax(value |> put_url(info))),
              ])
            )
          ),
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

    let on_data = (exp: Language.Exp.t) => {
      let seg = put(info, exp);
      let effects =
        if (model.hot_reload) {
          [local(SetLastLoad(Succeeded)), parent(SetSyntax(seg))];
        } else {
          [local(SetLastLoad(Succeeded))];
        };
      Bonsai.Effect.Expert.handle(Effect.Many(effects));
    };

    let on_error = (_msg: string) => {
      Bonsai.Effect.Expert.handle(local(SetLastLoad(Failed)));
    };

    ensure_subscribed(info.id, model.url, on_data, on_error);

    let connected = model.last_load == Some(Succeeded);
    let hot_reload_toggle =
      Node.div(
        ~attrs=[
          Attr.classes(
            ["toggle-switch", "hot-reload-toggle"]
            @ (model.hot_reload ? ["active"] : [])
            @ (connected ? [] : ["disabled"]),
          ),
          Attr.title(
            connected
              ? model.hot_reload
                  ? "Live (click to pause)" : "Paused (click to resume)"
              : "Connect to enable",
          ),
          Attr.on_pointerdown(evt => {
            // Sending up Effect.Stop_propagation doesn't work here
            // for some reason, causing the caret to change position when
            // the toggle is clicked. Claude figured out calling the methods
            // on the js event directly does work.
            Js.Unsafe.meth_call(evt, "stopPropagation", [||]) |> ignore;
            Js.Unsafe.meth_call(evt, "preventDefault", [||]) |> ignore;
            if (connected) {
              local(ToggleHotReload);
            } else {
              Effect.Ignore;
            };
          }),
        ],
        [
          Node.div(
            ~attrs=[Attr.classes(["toggle-knob"])],
            [Node.text({js|🔥|js})],
          ),
        ],
      );

    let disabled = model.hot_reload || !connected;
    let reload_btn =
      Node.div(
        ~attrs=[
          Attr.classes(
            ["manual-reload-btn"] @ (disabled ? ["disabled"] : []),
          ),
          Attr.title(
            disabled ? "Disable hot reload to use" : "Reload document",
          ),
          Attr.on_pointerdown(evt => {
            Js.Unsafe.meth_call(evt, "stopPropagation", [||]) |> ignore;
            Js.Unsafe.meth_call(evt, "preventDefault", [||]) |> ignore;
            if (!disabled) {
              switch (Id.Map.find_opt(info.id, subscriptions^)) {
              | Some({handle: Some(h), _}) =>
                switch (Automerge.doc_to_exp(h)) {
                | Ok(exp) =>
                  let seg = put(info, exp);
                  Bonsai.Effect.Expert.handle(parent(SetSyntax(seg)));
                  Effect.Ignore;
                | Error(_) => Effect.Ignore
                }
              | _ => Effect.Ignore
              };
            } else {
              Effect.Ignore;
            };
          }),
        ],
        [Node.text({js|↻|js})],
      );

    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["wrapper"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["cols", "code"])],
            [
              Node.text({js|·|js}),
              url_input,
              status_indicator,
              reload_btn,
              hot_reload_toggle,
            ],
          ),
        ],
      ),
    );
  };
};
