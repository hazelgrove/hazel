---
schemaVersion: 1
prNumber: 2346
prOwner: hazelgrove
prRepo: hazel
baseSha: 63261b75c3d5ea05b55c78f7e8c1823cf14dc09e
headSha: d30895c9f65258697f913904e1e9d32796e81783
---
# Automerge read/write projectors

This PR introduces a pair of Automerge projectors that can read from and write to remotely stored Automerge documents.

Validating behavior here requires creating Automerge documents through tiny.patchwork.inkandswitch.com (or some other Automerge doc interface), copying and pasting Automerge URLs into the read/write projectors, and assessing the JSON values read into Hazel and written back to the Automerge doc.

The read projector loads in an Automerge document specified by URL and converts the raw JSON into a Hazel JSON value that can then be manipulated programmatically within Hazel. It includes buttons for hot-reloading and, if not hot-reloading, manual refreshing. These are instantiated by typing ^^Automerge(Null) (where Null is the initial JSON value getting projected over and will be replaced by whatever Automerge JSON doc is loaded in).
<img width="700" height="724" alt="image" src="https://github.com/user-attachments/assets/7699e74b-4550-491f-b44d-6c23969635da" />

<details open>
<summary><code>src/haz3lcore/projectors/implementations/AutomergeProj.re</code> · open Util;</summary>

<!-- changetour:hunk file=src/haz3lcore/projectors/implementations/AutomergeProj.re level=1 baseBlob=606fd6e96bc9051d9d3030a75053346b0e8576b0 -->

```diff
@@ -0,0 +1,467 @@
+open Util;
+open ProjectorBase;
+open Virtual_dom.Vdom;
+open Js_of_ocaml;
+
+/* Automerge Projector: subscribes to an automerge document URL,
+   converts incoming data to the Hazel JSON ADT type, and updates
+   the underlying syntax via SetSyntax. */
+
+/* --- Subscription management (module-level, persistent across re-renders) --- */
+
+type subscription = {
+  url: string,
+  mutable on_data: Language.Exp.t => unit,
+  mutable on_error: string => unit,
+  mutable cleanup: option(unit => unit),
+  mutable handle: option(Js.t(Automerge.handle)),
+  mutable last_json: option(string),
+  mutable failed: bool,
+};
+
+let subscriptions: ref(Id.Map.t(subscription)) = ref(Id.Map.empty);
+
+let subscribe_to_doc =
+    (
+      id: Id.t,
+      url: string,
+      on_data: Language.Exp.t => unit,
+      on_error: string => unit,
+    ) => {
+  let sub = {
+    url,
+    on_data,
+    on_error,
+    cleanup: None,
+    handle: None,
+    last_json: None,
+    failed: false,
+  };
+  subscriptions := Id.Map.add(id, sub, subscriptions^);
+
+  /* Look up the document by URL via the global automerge repo.
+     Bail out if the repo isn't loaded yet (race condition on reload).
+     Keep the subscription in the map so ensure_subscribed won't retry
+     on every render (which would cause an infinite loop). */
+  if (Automerge.repo_is_ready()) {
+    let repo = Automerge.get_repo();
+    let promise = repo##find(Js.string(url));
+
+    /* Once the handle resolves, wire up the change listener. */
+    let then_result =
+      promise##then_(
+        Js.wrap_callback((handle: Js.t(Automerge.handle)) => {
+          /* Called on every "change" event (and once for the initial read).
+             Converts the live JS document to a Hazel expression. */
+          let callback =
+            Js.wrap_callback(_ => {
+              /* Deduplicate: compare the raw JSON string before doing
+                 the expensive Yojson→Hazel conversion and SetSyntax.
+                 Automerge fires "change" on sync ACKs even when the
+                 document content is unchanged. */
+              let json_str =
+                try(Some(Automerge.json_stringify(handle##doc))) {
+                | _ => None
+                };
+              let changed =
+                switch (json_str, Id.Map.find_opt(id, subscriptions^)) {
+                | (Some(js), Some(s)) =>
+                  switch (s.last_json) {
+                  | Some(prev) when prev == js => false
+                  | _ =>
+                    s.last_json = Some(js);
+                    true;
+                  }
+                | _ => true
+                };
+              if (changed) {
+                switch (Automerge.doc_to_exp(handle)) {
+                | Ok(exp) =>
+                  switch (Id.Map.find_opt(id, subscriptions^)) {
+                  | Some(s) => s.on_data(exp)
+                  | None => ()
+                  }
+                | Error(err) =>
+                  switch (Id.Map.find_opt(id, subscriptions^)) {
+                  | Some(s) => s.on_error(err)
+                  | None => ()
+                  }
+                };
+              };
+            });
+
+          /* Store a cleanup function that unsubscribes from changes. */
+          let cleanup_fn = () => {
+            ignore(
+              handle##off_(Js.string("change"), Js.Unsafe.inject(callback)),
+            );
+          };
+          switch (Id.Map.find_opt(id, subscriptions^)) {
+          | Some(s) =>
+            s.cleanup = Some(cleanup_fn);
+            s.handle = Some(handle);
+          | None => ()
+          };
+
+          /* Subscribe to future changes. */
+          ignore(
+            handle##on_(Js.string("change"), Js.Unsafe.inject(callback)),
+          );
+
+          /* Fire the callback once immediately to read the initial doc state. */
+          Js.Unsafe.fun_call(callback, [|Js.Unsafe.inject(Js.undefined)|]);
+        }),
+      );
+    ignore(
+      Js.Unsafe.meth_call(
+        then_result,
+        "catch",
+        [|
+          Js.Unsafe.inject(
+            Js.wrap_callback((_err: Js.Unsafe.any) => {
+              switch (Id.Map.find_opt(id, subscriptions^)) {
+              | Some(s) =>
+                s.failed = true;
+                s.on_error("Failed to find document");
+              | None => ()
+              }
+            }),
+          ),
+        |],
+      ),
+    );
+  };
+};
+
+let ensure_subscribed =
+    (
+      id: Id.t,
+      url: string,
+      on_data: Language.Exp.t => unit,
+      on_error: string => unit,
+    ) =>
+  if (String.length(url) > 0) {
+    switch (Id.Map.find_opt(id, subscriptions^)) {
+    | Some(sub) when sub.url == url && (sub.handle != None || sub.failed) =>
+      /* Already connected, or already failed for this URL — just
+         update the callbacks. Don't retry failed subscriptions to
+         avoid an infinite render loop (reject → on_error → re-render
+         → retry → reject → …). A new attempt will happen if the
+         user changes the URL. */
+      sub.on_data = on_data;
+      sub.on_error = on_error;
+    | Some(sub) when sub.url == url =>
+      /* Subscription exists but handle is None and not failed — the
+         repo wasn't ready when we first tried. Retry now if the
+         repo is available. */
+      sub.on_data = on_data;
+      sub.on_error = on_error;
+      if (Automerge.repo_is_ready()) {
+        subscriptions := Id.Map.remove(id, subscriptions^);
+        subscribe_to_doc(id, url, on_data, on_error);
+      };
+    | Some(sub) =>
+      Option.iter(f => f(), sub.cleanup);
+      subscriptions := Id.Map.remove(id, subscriptions^);
+      subscribe_to_doc(id, url, on_data, on_error);
+    | None => subscribe_to_doc(id, url, on_data, on_error)
+    };
+  };
+
+/* --- Projector module --- */
+
+module M: Projector = {
+  [@deriving (show({with_path: false}), sexp, yojson)]
+  type last_load =
+    | Succeeded
+    | Failed;
+
+  [@deriving (show({with_path: false}), sexp, yojson)]
+  type model = {
+    url: string,
+    last_load: option(last_load),
+    hot_reload: bool,
+  };
+
+  [@deriving (show({with_path: false}), sexp, yojson)]
+  type action =
+    | SetUrl(string)
+    | SetLastLoad(last_load)
+    | ToggleHotReload;
+
+  let init = (a: Language.Any.t): option(model) =>
+    switch (a) {
+    | Exp({term: Constructor("Null", _), _}) =>
+      Some({
+        url: "",
+        last_load: None,
+        hot_reload: true,
+      })
+    | _ => None
+    };
+
+  let put = (info, exp: Language.Exp.t): Base.segment =>
+    switch (
+      info.utility.lift_syntax(
+        ~inline=true,
+        fun
+        | Exp(any) =>
+          Exp({
+            ...any,
+            term: exp.term,
+          })
+        | _ => failwith("AutomergeProj: put: not expression"),
+        info.syntax,
+      )
+    ) {
+    | Some(s) => s
+    | None => failwith("AutomergeProj: put: lift failed")
+    };
+
+  let input_id = (id: Id.t): string => Id.cls(id) ++ "-input";
+
+  let focus_pointer = (id: Id.t) => {
+    JsUtil.get_elem_by_id(input_id(id))##focus;
+  };
+
+  let focus_keyboard = (id: Id.t, d: Direction.t) => {
+    let el = JsUtil.get_elem_by_id(input_id(id));
+    el##focus;
+    switch (d) {
+    | Left =>
+      Js.Unsafe.set(el, "selectionStart", 0);
+      Js.Unsafe.set(el, "selectionEnd", 0);
+    | Right =>
+      let len: int = Js.Unsafe.get(Js.Unsafe.get(el, "value"), "length");
+      Js.Unsafe.set(el, "selectionStart", len);
+      Js.Unsafe.set(el, "selectionEnd", len);
+    };
+  };
+
+  let focusable =
+    Focusable.{
+      pointer: Some(focus_pointer),
+      keyboard: Some(focus_keyboard),
+    };
+  let dynamics = false;
+  let elaborate_syntax = false;
+  let error = (_, _): option(ProjectorBase.error) => None;
+
+  let url_placeholder = "automerge:<doc-url>";
+
+  let placeholder = (m, _info) => {
+    let url_len = String.length(m.url);
+    /* +1 focus dot; +2 reload btn; +3 toggle + margins */
+    let display_len = max(String.length(url_placeholder), url_len);
+    ProjectorCore.Shape.inline(display_len + 7);
+  };
+
+  let update = (m: model, _info: info, action: action): model =>
+    switch (action) {
+    | SetUrl(url) => {
+        ...m,
+        url,
+        last_load: String.length(url) == 0 ? None : m.last_load,
+      }
+    | SetLastLoad(ll) => {
+        ...m,
+        last_load: Some(ll),
+      }
+    | ToggleHotReload => {
+        ...m,
+        hot_reload: !m.hot_reload,
+      }
+    };
+
+  let view =
+      ({model, info, local, parent, _}: View.args(model, action)): View.t => {
+    let load_status_cls =
+      switch (model.last_load) {
+      | None when String.length(model.url) > 0 => "load-none"
+      | None => ""
+      | Some(Succeeded) => "load-succeeded"
+      | Some(Failed) => "load-failed"
+      };
+
+    let input_at_start = () => {
+      let el = JsUtil.get_elem_by_id(input_id(info.id));
+      let pos: int = Js.Unsafe.get(el, "selectionStart");
+      pos == 0;
+    };
+
+    let input_at_end = () => {
+      let el = JsUtil.get_elem_by_id(input_id(info.id));
+      let pos: int = Js.Unsafe.get(el, "selectionStart");
+      let len: int = Js.Unsafe.get(Js.Unsafe.get(el, "value"), "length");
+      pos == len;
+    };
+
+    let key_handler = evt => {
+      open Effect;
+      let key = Key.mk(KeyDown, evt);
+      switch (key.key) {
+      | D("ArrowRight") when input_at_end() =>
+        JsUtil.get_elem_by_id(input_id(info.id))##blur;
+        Many([parent(Escape(Right)), Stop_propagation]);
+      | D("ArrowLeft") when input_at_start() =>
+        JsUtil.get_elem_by_id(input_id(info.id))##blur;
+        Many([parent(Escape(Left)), Stop_propagation]);
+      | D("Escape") =>
+        JsUtil.get_elem_by_id(input_id(info.id))##blur;
+        Many([parent(Escape(Right)), Stop_propagation]);
+      | _ => Stop_propagation
+      };
+    };
+
+    let url_input =
+      Node.input(
+        ~attrs=[
+          Attr.id(input_id(info.id)),
+          Attr.type_("text"),
+          Attr.class_("automerge-url-input"),
+          Attr.placeholder(url_placeholder),
+          Attr.string_property("value", model.url),
+          Attr.on_input((_evt, value) => {
+            let null_exp =
+              Language.IdTagged.FreshGrammar.Exp.constructor("Null", None);
+            let seg = put(info, null_exp);
+            Effect.(Many([local(SetUrl(value)), parent(SetSyntax(seg))]));
+          }),
+          Attr.on_keydown(key_handler),
+          Attr.on_copy(_ => Effect.Stop_propagation),
+          Attr.on_cut(_ => Effect.Stop_propagation),
+          Attr.on_paste(_ => Effect.Stop_propagation),
+          Attr.style(
+            Css_gen.concat([
+              Css_gen.create(~field="width", ~value="100%"),
+              Css_gen.create(~field="font-size", ~value="inherit"),
+              Css_gen.create(~field="font-family", ~value="inherit"),
+            ]),
+          ),
+        ],
+        (),
+      );
+
+    let on_data = (exp: Language.Exp.t) => {
+      ProjectorCore.set_bypass(info.id, exp);
+      let null_exp =
+        Language.IdTagged.FreshGrammar.Exp.constructor("Null", None);
+      let seg = put(info, null_exp);
+      let effects =
+        if (model.hot_reload) {
+          [local(SetLastLoad(Succeeded)), parent(SetSyntax(seg))];
+        } else {
+          [local(SetLastLoad(Succeeded))];
+        };
+      Bonsai.Effect.Expert.handle(Effect.Many(effects));
+    };
+
+    let on_error = (_msg: string) => {
+      Bonsai.Effect.Expert.handle(local(SetLastLoad(Failed)));
+    };
+
+    ensure_subscribed(info.id, model.url, on_data, on_error);
+
+    let connected = model.last_load == Some(Succeeded);
+    let hot_reload_toggle =
+      Node.div(
+        ~attrs=[
+          Attr.classes(
+            ["toggle-switch", "hot-reload-toggle"]
+            @ (model.hot_reload ? ["active"] : [])
+            @ (connected ? [] : ["disabled"]),
+          ),
+          Attr.title(
+            connected
+              ? model.hot_reload
+                  ? "Live (click to pause)" : "Paused (click to resume)"
+              : "Connect to enable",
+          ),
+          Attr.on_pointerdown(evt => {
+            // Sending up Effect.Stop_propagation doesn't work here
+            // for some reason, causing the caret to change position when
+            // the toggle is clicked. Claude figured out calling the methods
+            // on the js event directly does work.
+            Js.Unsafe.meth_call(evt, "stopPropagation", [||]) |> ignore;
+            Js.Unsafe.meth_call(evt, "preventDefault", [||]) |> ignore;
+            if (connected) {
+              local(ToggleHotReload);
+            } else {
+              Effect.Ignore;
+            };
+          }),
+        ],
+        [
+          Node.div(
+            ~attrs=[Attr.classes(["toggle-knob"])],
+            [Node.text({js|🔥|js})],
+          ),
+        ],
+      );
+
+    let disabled = model.hot_reload || !connected;
+    let reload_btn =
+      Node.div(
+        ~attrs=[
+          Attr.classes(
+            ["manual-reload-btn"] @ (disabled ? ["disabled"] : []),
+          ),
+          Attr.title(
+            disabled ? "Disable hot reload to use" : "Reload document",
+          ),
+          Attr.on_pointerdown(evt => {
+            Js.Unsafe.meth_call(evt, "stopPropagation", [||]) |> ignore;
+            Js.Unsafe.meth_call(evt, "preventDefault", [||]) |> ignore;
+            if (!disabled) {
+              /* Spin animation feedback (remove+reflow+add to restart) */
+              let target = Js.Unsafe.get(evt, "currentTarget");
+              JsUtil.rm_cls(target, "spinning");
+              ignore(Js.Unsafe.get(target, "offsetWidth"));
+              JsUtil.add_cls(target, "spinning");
+              switch (Id.Map.find_opt(info.id, subscriptions^)) {
+              | Some({handle: Some(h), _}) =>
+                switch (Automerge.doc_to_exp(h)) {
+                | Ok(exp) =>
+                  ProjectorCore.set_bypass(info.id, exp);
+                  let null_exp =
+                    Language.IdTagged.FreshGrammar.Exp.constructor(
+                      "Null",
+                      None,
+                    );
+                  let seg = put(info, null_exp);
+                  Bonsai.Effect.Expert.handle(parent(SetSyntax(seg)));
+                  Effect.Ignore;
+                | Error(_) => Effect.Ignore
+                }
+              | _ => Effect.Ignore
+              };
+            } else {
+              Effect.Ignore;
+            };
+          }),
+          Attr.on_mouseleave(evt => {
+            JsUtil.rm_cls(Js.Unsafe.get(evt, "currentTarget"), "spinning");
+            Effect.Ignore;
+          }),
+        ],
+        [Node.text({js|🔄|js})],
+      );
+
+    View.mk(
+      Node.div(
+        ~attrs=[Attr.classes(["wrapper", load_status_cls])],
+        [
+          Node.div(
+            ~attrs=[Attr.classes(["cols", "code"])],
+            [
+              Node.text({js|·|js}),
+              url_input,
+              reload_btn,
+              hot_reload_toggle,
+            ],
+          ),
+        ],
+      ),
+    );
+  };
+};
```

</details>

<details open>
<summary><code>src/web/www/style/projectors/proj-automerge.css</code> · /* PROJECTOR: AUTOMERGE */</summary>

<!-- changetour:hunk file=src/web/www/style/projectors/proj-automerge.css level=1 baseBlob=666be48fb3358dbe3aab4eafbf6de94165d11127 -->

```diff
@@ -0,0 +1,135 @@
+/* PROJECTOR: AUTOMERGE */
+
+/* Turn off caret when automerge projector is focused */
+.code-deco:has(~ .projectors .projector.Automerge *:focus) #caret .caret-path {
+  fill: #0000;
+}
+
+.projector.Automerge {
+  cursor: default;
+}
+
+.projector.Automerge > svg {
+  stroke-width: 0.5px;
+  stroke: var(--BR2);
+}
+
+.projector.Automerge .wrapper {
+  position: relative;
+  height: 100%;
+  width: 100%;
+  border-radius: 0.1em;
+}
+
+.projector.Automerge .cols {
+  height: 100%;
+  margin-left: 2px;
+  margin-right: 2px;
+  display: flex;
+  align-items: center;
+  color: var(--SAND);
+}
+
+.projector.Automerge.indicated .cols,
+.projector.Automerge:has(input:focus) .cols {
+  color: var(--R1);
+}
+
+.projector.Automerge.indicated > svg {
+  fill: var(--textarea-indicated);
+}
+
+.projector.Automerge.selected > svg {
+  filter: drop-shadow(1px 1px 0 var(--R0));
+}
+
+.projector.Automerge input {
+  outline: none;
+  caret-color: var(--caret-color);
+  padding: 0;
+  margin: 0;
+  line-height: var(--line-height);
+  font-family: var(--code-font);
+  font-size: inherit;
+  border: none;
+  color: var(--textarea-text);
+  background: none;
+  overflow: hidden;
+}
+
+.projector.Automerge input::placeholder {
+  opacity: 0.5;
+}
+
+.projector.Automerge input::selection {
+  color: var(--BLACK);
+  background-color: var(--shard-selected);
+}
+
+.projector.Automerge .hot-reload-toggle {
+  flex-shrink: 0;
+  margin-left: 4px;
+  font-size: 0.7em;
+}
+
+.projector.Automerge .hot-reload-toggle.disabled {
+  opacity: 0.35;
+  cursor: default;
+  pointer-events: none;
+}
+
+.projector.Automerge .manual-reload-btn {
+  flex-shrink: 0;
+  cursor: pointer;
+  font-size: 0.8em;
+  margin-left: 4px;
+  user-select: none;
+  color: var(--GB1);
+  /* align vertically with hot-reload toggle */
+  position: relative;
+  top: 1px;
+}
+
+.projector.Automerge .manual-reload-btn.disabled {
+  opacity: 0.35;
+  cursor: default;
+  pointer-events: none;
+}
+
+.projector.Automerge .manual-reload-btn:hover {
+  animation: wobble 0.6s ease 0s 1 normal forwards;
+  filter: brightness(1.2);
+}
+
+@keyframes spin360 {
+  from { transform: rotate(0deg); }
+  to   { transform: rotate(-360deg); }
+}
+
+.projector.Automerge .manual-reload-btn.spinning {
+  animation: spin360 0.4s ease-in-out;
+}
+
+/* Last-load status superscript (mirrors inline test result styling) */
+.projector.Automerge .wrapper::after {
+  position: absolute;
+  top: -0.15em;
+  right: -1em;
+  font-size: 64%;
+  z-index: var(--projector-overlay-z);
+}
+
+.projector.Automerge .wrapper.load-succeeded::after {
+  content: "\2714";
+  color: var(--test-pass);
+}
+
+.projector.Automerge .wrapper.load-failed::after {
+  content: "\2718";
+  color: var(--test-fail);
+}
+
+.projector.Automerge .wrapper.load-none::after {
+  content: "?";
+  color: var(--test-indet);
+}
```

</details>

The write projector takes an Automerge URL and pushes whatever JSON value it is projecting over to that url. These are instantiated by typing `^^AutomergeWriteBack(<json>)` where `<json>` is whatever Hazel JSON value you want to write back - in the screenshots below, this is just the empty `Null` value.
<img width="468" height="220" alt="image" src="https://github.com/user-attachments/assets/41c29038-876c-45fb-86fa-99bf5ba14a87" />
<img width="500" height="220" alt="image" src="https://github.com/user-attachments/assets/4ea6349b-d891-41fc-9cc5-893cb0c76c06" />

<details open>
<summary><code>src/haz3lcore/projectors/implementations/AutomergeWriteBackProj.re</code> · open Util;</summary>

<!-- changetour:hunk file=src/haz3lcore/projectors/implementations/AutomergeWriteBackProj.re level=1 baseBlob=19e6b6901d1f62d6c79e9a5798c8ffa8bc4cc2c2 -->

```diff
@@ -0,0 +1,249 @@
+open Util;
+open ProjectorBase;
+open Virtual_dom.Vdom;
+open Js_of_ocaml;
+open Language;
+
+/* AutomergeWriteBack Projector: reads an evaluated Hazel value
+   (via dynamics = true), converts it to JSON, and writes it back
+   to an automerge document. Paired with AutomergeProj (which reads
+   automerge → Hazel), this creates a reactive loop:
+   AutomergeProj reads doc → Hazel augment function → AutomergeWriteBackProj writes to doc */
+
+/* --- Write state management (module-level, persistent across re-renders) --- */
+
+type write_status =
+  | Idle
+  | Wrote
+  | Waiting
+  | Error(string);
+
+type write_state = {
+  mutable last_json: option(string),
+  mutable last_sample: option(Language.Exp.t),
+  mutable status: write_status,
+};
+
+let write_states: ref(Id.Map.t(write_state)) = ref(Id.Map.empty);
+
+let get_write_state = (id: Id.t): write_state =>
+  switch (Id.Map.find_opt(id, write_states^)) {
+  | Some(ws) => ws
+  | None =>
+    let ws = {
+      last_json: None,
+      last_sample: None,
+      status: Waiting,
+    };
+    write_states := Id.Map.add(id, ws, write_states^);
+    ws;
+  };
+
+/* --- Projector module --- */
+
+module M: Projector = {
+  [@deriving (show({with_path: false}), sexp, yojson)]
+  type model = {url: string};
+
+  [@deriving (show({with_path: false}), sexp, yojson)]
+  type action =
+    | SetUrl(string);
+
+  let init = (a: Language.Any.t): option(model) =>
+    switch (a) {
+    | Exp(_) => Some({url: ""})
+    | _ => None
+    };
+
+  let input_id = (id: Id.t): string => Id.cls(id) ++ "-wb-input";
+
+  let focus_pointer = (id: Id.t) => {
+    JsUtil.get_elem_by_id(input_id(id))##focus;
+  };
+
+  let focus_keyboard = (id: Id.t, d: Direction.t) => {
+    let el = JsUtil.get_elem_by_id(input_id(id));
+    el##focus;
+    switch (d) {
+    | Left =>
+      Js.Unsafe.set(el, "selectionStart", 0);
+      Js.Unsafe.set(el, "selectionEnd", 0);
+    | Right =>
+      let len: int = Js.Unsafe.get(Js.Unsafe.get(el, "value"), "length");
+      Js.Unsafe.set(el, "selectionStart", len);
+      Js.Unsafe.set(el, "selectionEnd", len);
+    };
+  };
+
+  let focusable =
+    Focusable.{
+      pointer: Some(focus_pointer),
+      keyboard: Some(focus_keyboard),
+    };
+  let dynamics = true;
+  let elaborate_syntax = false;
+  let error = (_, _): option(ProjectorBase.error) => None;
+
+  let url_placeholder = "automerge:<doc-url>";
+
+  let placeholder = (m, _info) => {
+    let url_len = String.length(m.url);
+    let display_len = max(String.length(url_placeholder), url_len);
+    /* +1 focus dot; +2 arrow icon; +2 status dot + margins */
+    ProjectorCore.Shape.inline(display_len + 6);
+  };
+
+  let update = (_m: model, _info: info, action: action): model =>
+    switch (action) {
+    | SetUrl(url) => {url: url}
+    };
+
+  let select_sample = (info: info): option(Sample.t) =>
+    switch (info.dynamics) {
+    | Some(x) =>
+      switch (x.samples) {
+      | [sample, ..._] => Some(sample)
+      | _ => None
+      }
+    | _ => None
+    };
+
+  let view =
+      ({model, info, local, parent, _}: View.args(model, action)): View.t => {
+    let ws = get_write_state(info.id);
+
+    /* Try to extract and write the evaluated value.
+       Skip the expensive exp→JSON conversion when the sample expression
+       is physically identical to the last one we processed (common on
+       cursor moves where dynamics are cached). */
+    if (String.length(model.url) > 0) {
+      switch (select_sample(info)) {
+      | Some(sample) =>
+        let same_sample =
+          switch (ws.last_sample) {
+          | Some(prev) => prev === sample.value
+          | None => false
+          };
+        if (!same_sample) {
+          ws.last_sample = Some(sample.value);
+          switch (Automerge.exp_to_json_string(sample.value)) {
+          | Ok(json_string) =>
+            /* Only write if the JSON has changed (loop prevention) */
+            let should_write =
+              switch (ws.last_json) {
+              | Some(prev) => prev != json_string
+              | None => true
+              };
+            if (should_write) {
+              Automerge.write_to_doc(model.url, json_string);
+              ws.last_json = Some(json_string);
+              ws.status = Wrote;
+            } else {
+              ws.status = Idle;
+            };
+          | Error(msg) => ws.status = Error(msg)
+          };
+        };
+      | None => ws.status = Waiting
+      };
+    };
+
+    let status_cls =
+      switch (ws.status) {
+      | Idle => "write-idle"
+      | Wrote => "write-active"
+      | Waiting => "write-waiting"
+      | Error(_) => "write-error"
+      };
+
+    let input_at_start = () => {
+      let el = JsUtil.get_elem_by_id(input_id(info.id));
+      let pos: int = Js.Unsafe.get(el, "selectionStart");
+      pos == 0;
+    };
+
+    let input_at_end = () => {
+      let el = JsUtil.get_elem_by_id(input_id(info.id));
+      let pos: int = Js.Unsafe.get(el, "selectionStart");
+      let len: int = Js.Unsafe.get(Js.Unsafe.get(el, "value"), "length");
+      pos == len;
+    };
+
+    let key_handler = evt => {
+      open Effect;
+      let key = Key.mk(KeyDown, evt);
+      switch (key.key) {
+      | D("ArrowRight") when input_at_end() =>
+        JsUtil.get_elem_by_id(input_id(info.id))##blur;
+        Many([parent(Escape(Right)), Stop_propagation]);
+      | D("ArrowLeft") when input_at_start() =>
+        JsUtil.get_elem_by_id(input_id(info.id))##blur;
+        Many([parent(Escape(Left)), Stop_propagation]);
+      | D("Escape") =>
+        JsUtil.get_elem_by_id(input_id(info.id))##blur;
+        Many([parent(Escape(Right)), Stop_propagation]);
+      | _ => Stop_propagation
+      };
+    };
+
+    let url_input =
+      Node.input(
+        ~attrs=[
+          Attr.id(input_id(info.id)),
+          Attr.type_("text"),
+          Attr.class_("automerge-wb-url-input"),
+          Attr.placeholder(url_placeholder),
+          Attr.string_property("value", model.url),
+          Attr.on_input((_evt, value) => local(SetUrl(value))),
+          Attr.on_keydown(key_handler),
+          Attr.on_copy(_ => Effect.Stop_propagation),
+          Attr.on_cut(_ => Effect.Stop_propagation),
+          Attr.on_paste(_ => Effect.Stop_propagation),
+          Attr.style(
+            Css_gen.concat([
+              Css_gen.create(~field="width", ~value="100%"),
+              Css_gen.create(~field="font-size", ~value="inherit"),
+              Css_gen.create(~field="font-family", ~value="inherit"),
+            ]),
+          ),
+        ],
+        (),
+      );
+
+    let status_title =
+      switch (ws.status) {
+      | Idle => "Idle (no change)"
+      | Wrote => "Wrote to document"
+      | Waiting => "Waiting for dynamics"
+      | Error(msg) => "Error: " ++ msg
+      };
+    let status_dot =
+      Node.span(
+        ~attrs=[
+          Attr.classes(["wb-status-dot", status_cls]),
+          Attr.title(status_title),
+        ],
+        [],
+      );
+
+    View.mk(
+      Node.div(
+        ~attrs=[Attr.classes(["wrapper"])],
+        [
+          Node.div(
+            ~attrs=[Attr.classes(["cols", "code"])],
+            [
+              Node.text({js|·|js}),
+              Node.span(
+                ~attrs=[Attr.class_("wb-arrow")],
+                [Node.text({js|⬆|js})],
+              ),
+              url_input,
+              status_dot,
+            ],
+          ),
+        ],
+      ),
+    );
+  };
+};
```

</details>

<details open>
<summary><code>src/web/www/style/projectors/proj-automerge-writeback.css</code> · /* PROJECTOR: AUTOMERGE WRITE-BACK */</summary>

<!-- changetour:hunk file=src/web/www/style/projectors/proj-automerge-writeback.css level=1 baseBlob=fad4215b4eae81ec179a92ea8e1a6404c166657c -->

```diff
@@ -0,0 +1,98 @@
+/* PROJECTOR: AUTOMERGE WRITE-BACK */
+
+/* Turn off caret when write-back projector is focused */
+.code-deco:has(~ .projectors .projector.AutomergeWriteBack *:focus) #caret .caret-path {
+  fill: #0000;
+}
+
+.projector.AutomergeWriteBack {
+  cursor: default;
+}
+
+.projector.AutomergeWriteBack > svg {
+  stroke-width: 0.5px;
+  stroke: var(--BR2);
+}
+
+.projector.AutomergeWriteBack .wrapper {
+  position: relative;
+  height: 100%;
+  width: 100%;
+  border-radius: 0.1em;
+}
+
+.projector.AutomergeWriteBack .cols {
+  height: 100%;
+  margin-left: 2px;
+  margin-right: 2px;
+  display: flex;
+  align-items: center;
+  color: var(--SAND);
+}
+
+.projector.AutomergeWriteBack.indicated .cols,
+.projector.AutomergeWriteBack:has(input:focus) .cols {
+  color: var(--R1);
+}
+
+.projector.AutomergeWriteBack.indicated > svg {
+  fill: var(--textarea-indicated);
+}
+
+.projector.AutomergeWriteBack.selected > svg {
+  filter: drop-shadow(1px 1px 0 var(--R0));
+}
+
+.projector.AutomergeWriteBack input {
+  outline: none;
+  caret-color: var(--caret-color);
+  padding: 0;
+  margin: 0;
+  line-height: var(--line-height);
+  font-family: var(--code-font);
+  font-size: inherit;
+  border: none;
+  color: var(--textarea-text);
+  background: none;
+  overflow: hidden;
+}
+
+.projector.AutomergeWriteBack input::placeholder {
+  opacity: 0.5;
+}
+
+.projector.AutomergeWriteBack input::selection {
+  color: var(--BLACK);
+  background-color: var(--shard-selected);
+}
+
+.projector.AutomergeWriteBack .wb-arrow {
+  flex-shrink: 0;
+  font-size: 0.7em;
+  margin-right: 2px;
+}
+
+.projector.AutomergeWriteBack .wb-status-dot {
+  flex-shrink: 0;
+  width: 6px;
+  height: 6px;
+  border-radius: 50%;
+  margin-left: 4px;
+  display: inline-block;
+}
+
+.projector.AutomergeWriteBack .wb-status-dot.write-active {
+  background-color: var(--test-pass);
+}
+
+.projector.AutomergeWriteBack .wb-status-dot.write-idle {
+  background-color: #888;
+}
+
+.projector.AutomergeWriteBack .wb-status-dot.write-waiting {
+  background-color: #cc0;
+}
+
+.projector.AutomergeWriteBack .wb-status-dot.write-error {
+  background-color: var(--test-fail);
+}
```

</details>

Adding these new projectors involve a fair amount of plumbing/boilerplate.

<details open>
<summary><code>src/language/ProjectorKind.re</code> · | Csv;</summary>

<!-- changetour:hunk file=src/language/ProjectorKind.re level=1 baseBlob=e2ef917caf99fd794adcb30c6b95d4efbdebb6e8 -->

```diff
@@ -17,7 +17,9 @@ type t =
   | Livelit
   | TextArea
   | Table
-  | Csv;
+  | Csv
+  | Automerge
+  | AutomergeWriteBack;

 let livelit_projectors: list(t) = [
   Csv, /* Competes with Card for empty list */
```

</details>

<details open>
<summary><code>src/language/ProjectorKind.re</code> · let projectors: list(t) = livelit_projectors @ [Fold];</summary>

<!-- changetour:hunk file=src/language/ProjectorKind.re level=1 baseBlob=e2ef917caf99fd794adcb30c6b95d4efbdebb6e8 -->

```diff
@@ -32,7 +34,8 @@ let livelit_projectors: list(t) = [
 ];

 /* Note: Probe intentionally excluded - probes use separate action path */
-let projectors: list(t) = livelit_projectors @ [Fold];
+let projectors: list(t) =
+  livelit_projectors @ [Fold, Automerge, AutomergeWriteBack];

 /* Refractors are like probes - additive decorations, not syntax-replacing */
 let refractors: list(t) = [Probe, Statics];
```

</details>

<details open>
<summary><code>src/language/ProjectorKind.re</code> · | Automerge =&gt; "Automerge"</summary>

<!-- changetour:hunk file=src/language/ProjectorKind.re level=1 baseBlob=e2ef917caf99fd794adcb30c6b95d4efbdebb6e8 -->

```diff
@@ -54,6 +57,8 @@ let name = (p: t): string =>
   | TextArea => "text"
   | Table => "table"
   | Csv => "csv"
+  | Automerge => "Automerge"
+  | AutomergeWriteBack => "AutomergeWriteBack"
   };

 /* This must be updated and kept 1-to-1 with the above
```

</details>

<details open>
<summary><code>src/language/ProjectorKind.re</code> · | "Automerge" =&gt; Automerge</summary>

<!-- changetour:hunk file=src/language/ProjectorKind.re level=1 baseBlob=e2ef917caf99fd794adcb30c6b95d4efbdebb6e8 -->

```diff
@@ -72,6 +77,8 @@ let of_name = (p: string): t =>
   | "card" => Card
   | "table" => Table
   | "csv" => Csv
+  | "Automerge" => Automerge
+  | "AutomergeWriteBack" => AutomergeWriteBack
   | _ => failwith("Unknown projector kind")
   };

```

</details>

<details open>
<summary><code>src/haz3lcore/projectors/ProjectorInit.re</code> · | Automerge =&gt; (module Cook(AutomergeProj.M))</summary>

<!-- changetour:hunk file=src/haz3lcore/projectors/ProjectorInit.re level=1 baseBlob=346d0efe498606248ac00f1ba0fe9b758a4dd113 -->

```diff
@@ -17,6 +17,8 @@ let to_module = (kind: ProjectorCore.Kind.t): (module Cooked) =>
   | Card => (module Cook(CardProj.M))
   | Table => (module Cook(TableProj.M))
   | Csv => (module Cook(CSVProjector.M))
+  | Automerge => (module Cook(AutomergeProj.M))
+  | AutomergeWriteBack => (module Cook(AutomergeWriteBackProj.M))
   };

 let init =
```

</details>

<details open>
<summary><code>src/web/app/editors/code/ContextMenu.re</code> · | Automerge =&gt; "Automerge"</summary>

<!-- changetour:hunk file=src/web/app/editors/code/ContextMenu.re level=1 baseBlob=242b635e94fea3f68935e939197f6da7e4df8ade -->

```diff
@@ -281,6 +281,8 @@ module Projectors = {
     | Table => "Table"
     | Livelit => "Livelit"
     | Probe => "Probe" /* shouldn't appear in menu */
+    | Automerge => "Automerge"
+    | AutomergeWriteBack => "Automerge (Write-back)"
     };

   let applicable_kinds =
```

</details>

<details open>
<summary><code>src/web/www/style/projectors/proj-base.css</code> · @import "proj-automerge.css";</summary>

<!-- changetour:hunk file=src/web/www/style/projectors/proj-base.css level=1 baseBlob=16b8d28aa70046c0bf10cd9923e7e41d16b6b015 -->

```diff
@@ -12,6 +12,8 @@
 @import "proj-table.css";
 @import "proj-table-probe.css";
 @import "proj-csv.css";
+@import "proj-automerge.css";
+@import "proj-automerge-writeback.css";

 /* Default projector styles */

```

</details>

<details open>
<summary><code>src/util/JsUtil.re</code> · let add_cls = (el: Js.Unsafe.any, cls: string): unit =&gt;</summary>

<!-- changetour:hunk file=src/util/JsUtil.re level=1 baseBlob=d8a280d2952ef0003bdf65f4f649a4ebee1675af -->

```diff
@@ -241,6 +241,20 @@ let find_ancestor_with_class =
   loop(element_to_node(el));
 };

+let add_cls = (el: Js.Unsafe.any, cls: string): unit =>
+  Js.Unsafe.meth_call(
+    Js.Unsafe.get(el, "classList"),
+    "add",
+    [|Js.Unsafe.inject(Js.string(cls))|],
+  );
+
+let rm_cls = (el: Js.Unsafe.any, cls: string): unit =>
+  Js.Unsafe.meth_call(
+    Js.Unsafe.get(el, "classList"),
+    "remove",
+    [|Js.Unsafe.inject(Js.string(cls))|],
+  );
+
 let adjust_scroll = (container: Js.t(Dom_html.element), delta: float) =>
   if (delta != 0.) {
     let current = float_of_int(container##.scrollTop);
```

</details>

The standard approach to incorporating projector values into the Hazel runtime is to expand projector values into the underlying editor representation (ie `Segment.t`) and then let the regular parsing/typechecking/evaluating pipeline do its thing. This approach is inefficient in that typically the projector values start in AST form (ie `Term.t`) and get serialized to `Segment` only to be parsed back into `Term`. This is fine for small values but there's noticeable latency when parsing edit states encoding real Automerge docs. Hence, this PR introduces a bypass system by which the parser can skip parsing these large values. Projectors may store their expanded values (in already parsed `Term` form) in a table separate from the edit state, while leaving behind holes in their place. Later, the parser encounters these holes and puts the stored projector values in their place when constructing the overall term. See `ProjectorCore.re` and search for uses of `ProjectorCore.get_bypass` and `ProjectorCore.set_bypass` in `MakeTerm.re` and the projector implementations.

<details open>
<summary><code>src/haz3lcore/projectors/ProjectorCore.re</code> · /* Serialization bypass: projectors store their Exp.t here…</summary>

<!-- changetour:hunk file=src/haz3lcore/projectors/ProjectorCore.re level=1 baseBlob=76487755e7c5161696d97aef6395de4bd280a5ae -->

```diff
@@ -35,3 +35,18 @@ let mk = (~id=Id.mk(), kind, syntax, model) => {
 module Shape = Util.ProjectorShape;
 /* Projectors currently are all convex */
 let shapes = (_: t('a)): Nibs.shapes => Nib.Shape.(Convex, Convex);
+
+/* Serialization bypass: projectors store their Exp.t here instead of
+   round-tripping through segment/term serialization. Keyed by projector
+   piece ID. Used by the Automerge projectors to avoid serializing loaded
+   JSON docs through editor segment text. */
+let bypass_table: ref(Id.Map.t(Language.Exp.t)) = ref(Id.Map.empty);
+
+let set_bypass = (id: Id.t, exp: Language.Exp.t): unit =>
+  bypass_table := Id.Map.add(id, exp, bypass_table^);
+
+let get_bypass = (id: Id.t): option(Language.Exp.t) =>
+  Id.Map.find_opt(id, bypass_table^);
+
+let remove_bypass = (id: Id.t): unit =>
+  bypass_table := Id.Map.remove(id, bypass_table^);
```

</details>

<details open>
<summary><code>src/haz3lcore/lang/MakeTerm.re</code> · let sort = Piece.sort(syntax) |&gt; fst;</summary>

<!-- changetour:hunk file=src/haz3lcore/lang/MakeTerm.re level=1 baseBlob=0b8efd161f199442e01950c7d55066741f6931ae -->

```diff
@@ -1422,9 +1422,14 @@ and unsorted = (sort: Sort.t, skel: Skel.t, seg: Segment.t): unsorted => {
     | Grout(_) => []
     | Projector({id, kind, model, syntax, _} as pr) =>
       let _ = log_projector(pr);
-      let sort = Piece.sort(syntax) |> fst;
-      let seg = Piece.unparenthesize(syntax);
-      let inner = go_s(sort, Segment.skel(seg), seg);
+      let inner =
+        switch (ProjectorCore.get_bypass(id)) {
+        | Some(exp) => Grammar.Exp(exp)
+        | None =>
+          let sort = Piece.sort(syntax) |> fst;
+          let seg = Piece.unparenthesize(syntax);
+          go_s(sort, Segment.skel(seg), seg);
+        };
       /* Construct Projector term with proper annotation, preserving
        * projector metadata (kind, model) in the term for round-tripping */
       let projector_data: Grammar.projector_data = {
```

</details>

<details open>
<summary><code>src/haz3lcore/projectors/ProjectorPerform.re</code> · /* Clean up any serialization-bypass entry for this project…</summary>

<!-- changetour:hunk file=src/haz3lcore/projectors/ProjectorPerform.re level=1 baseBlob=fe8fc97019feb8adc6b9d455c90157a58c90631c -->

```diff
@@ -77,6 +77,11 @@ let replace_selection_and_unselect =
   |> Zipper.directional_unselect(focus);

 let remove = (piece: Base.piece, focus: Direction.t, z: Zipper.t): Zipper.t => {
+  /* Clean up any serialization-bypass entry for this projector. */
+  switch (piece) {
+  | Projector(pr) => ProjectorCore.remove_bypass(pr.id)
+  | _ => ()
+  };
   let seg = Piece.unparenthesize(piece);
   /* If it's a convex tile, unselect; otherwise, leave selection to guarantee you can toggle */
   switch (seg) {
```

</details>

There's a new prebundle script that sets up a connection with the Automerge repo. On the ReasonML side, `Automerge.re` providers helpers for using this connection to read and write Automerge docs by URL, and translating between raw and Hazel JSON values.

<details open>
<summary><code>src/web/www/prebundle.js</code> · // Automerge repo setup: exposes window.repo for the Autome…</summary>

<!-- changetour:hunk file=src/web/www/prebundle.js level=1 baseBlob=b82ec38f8874b60addf34bc25e953727bf8e70c2 -->

```diff
@@ -5,6 +5,19 @@ import hotkeys from 'hotkeys-js'
 import Algebrite from 'algebrite';
 window.Algebrite = Algebrite;

+// Automerge repo setup: exposes window.repo for the Automerge projectors.
+import {
+  IndexedDBStorageAdapter,
+  WebSocketClientAdapter,
+  Repo,
+} from "@automerge/vanillajs";
+
+window.repo = new Repo({
+  storage: new IndexedDBStorageAdapter(),
+  network: [new WebSocketClientAdapter("wss://sync3.automerge.org")],
+  enableRemoteHeadsGossiping: true,
+});
+
 // This is the default behavior for the hotkeys module but I'm overriding it for the
 // clipboard-shim and the ninja-keys command palette (which lives inside a shadow DOM).
 hotkeys.filter = event => {
```

</details>

<details open>
<summary><code>src/web/www/build-prebundle.mjs</code> · #!/usr/bin/env node</summary>

<!-- changetour:hunk file=src/web/www/build-prebundle.mjs level=1 baseBlob=143339c2b3165281875f7d2577961aae9b15bbd2 -->

```diff
@@ -0,0 +1,25 @@
+#!/usr/bin/env node
+
+// Bundles prebundle.js into bundled.js. Replaces a CLI esbuild invocation
+// because we need the wasm-loader plugin to handle Automerge's WASM
+// imports; the plugin embeds the WASM as base64, so bundled.js is the only
+// output and no separate .wasm file needs to be served.
+
+import {build} from "esbuild";
+import {wasmLoader} from "esbuild-plugin-wasm";
+import path from "node:path";
+import {fileURLToPath} from "node:url";
+
+const __dirname = path.dirname(fileURLToPath(import.meta.url));
+
+await build({
+  entryPoints: [path.join(__dirname, "prebundle.js")],
+  outfile: path.join(__dirname, "bundled.js"),
+  absWorkingDir: __dirname,
+  bundle: true,
+  format: "esm",
+  platform: "browser",
+  target: "esnext",
+  logLevel: "info",
+  plugins: [wasmLoader({mode: "embedded"})],
+});
```

</details>

<details open>
<summary><code>src/web/www/dune</code> · (deps prebundle.js)</summary>

<!-- changetour:hunk file=src/web/www/dune level=1 baseBlob=4720cafce50d1043088b110ceefba2cbc857545a -->

```diff
@@ -6,10 +6,11 @@

 (rule
  (targets bundled.js)
- (deps prebundle.js)
+ (deps
+  prebundle.js
+  build-prebundle.mjs
+  (source_tree %{workspace_root}/node_modules))
  (action
-  (run
-   %{project_root}/node_modules/esbuild/bin/esbuild
-   prebundle.js
-   --bundle
-   --outfile=bundled.js)))
+  (chdir
+   %{workspace_root}
+   (run node %{dep:build-prebundle.mjs}))))
```

</details>

<details open>
<summary><code>src/haz3lcore/projectors/implementations/Automerge.re</code> · open Js_of_ocaml;</summary>

<!-- changetour:hunk file=src/haz3lcore/projectors/implementations/Automerge.re level=1 baseBlob=29a6f6995b8a38ef0708228a9b6420d0d039b335 -->

```diff
@@ -0,0 +1,209 @@
+open Js_of_ocaml;
+
+/* Typed js_of_ocaml bindings for the automerge repo and documents.
+   - repo:    the document store; lives on globalThis.repo
+   - handle:  a live reference to a document; supports .doc(),
+              .on("change", cb), and .off("change", cb) */
+
+// A document handle obtained after repo.find() resolves.
+class type handle = {
+  pub doc: Js.meth(Js.Unsafe.any);
+  // handle##on_(event, callback): subscribe to document events.
+  pub on_: (Js.t(Js.js_string), Js.Unsafe.any) => Js.meth(unit);
+  // handle##off_(event, callback): unsubscribe from document events.
+  pub off_: (Js.t(Js.js_string), Js.Unsafe.any) => Js.meth(unit);
+};
+// handle##doc: returns the current document state.
+
+// The promise returned by repo.find(), resolving to a handle.
+class type promise = {
+  pub then_: Js.callback(Js.t(handle) => unit) => Js.meth(Js.Unsafe.any);
+};
+
+// The automerge-repo Repo instance (expected at globalThis.repo).
+class type repo = {
+  pub find: Js.t(Js.js_string) => Js.meth(Js.t(promise));
+};
+// repo##find(url): look up a document by its automerge URL.
+
+// Retrieve the global Repo instance (globalThis.repo).
+let get_repo = (): Js.t(repo) =>
+  Js.Unsafe.coerce(Js.Unsafe.get(Js.Unsafe.global, "repo"));
+
+// Check if the global Repo instance is available.
+let repo_is_ready = (): bool =>
+  Js.to_bool(
+    Js.Unsafe.pure_js_expr("typeof globalThis.repo !== 'undefined'"),
+  );
+
+// Call JSON.stringify on a JS value and return an OCaml string.
+let json_stringify = (value: Js.Unsafe.any): string => {
+  let json_obj = Js.Unsafe.get(Js.Unsafe.global, "JSON");
+  Js.to_string(
+    Js.Unsafe.meth_call(json_obj, "stringify", [|Js.Unsafe.inject(value)|]),
+  );
+};
+
+// Read the current document from a handle, JSON-stringify it,
+// and parse into a Hazel expression via the JsonADT codec.
+// Returns Error if the document is not yet available (null/undefined)
+// or if JSON parsing/conversion fails.
+let doc_to_exp = (handle: Js.t(handle)): result(Language.Exp.t, string) =>
+  try({
+    let doc = handle##doc;
+    let json_str = json_stringify(doc);
+    let yojson = Yojson.Safe.from_string(json_str);
+    HazelJson.JsonADT.yojson_to_exp(yojson);
+  }) {
+  | exn => Error("Document not available: " ++ Printexc.to_string(exn))
+  };
+
+// Convert a Hazel expression (JSON ADT) to a JSON string.
+// Inverse of the doc_to_exp path.
+let exp_to_json_string = (exp: Language.Exp.t): result(string, string) =>
+  switch (HazelJson.JsonADT.exp_to_yojson(exp)) {
+  | Ok(yojson) => Ok(Yojson.Safe.to_string(yojson))
+  | Error(msg) => Error(msg)
+  };
+
+/* Capture the patchwork-view element at module load time. Guarded for
+   non-browser hosts (Node test runner) where document is undefined. */
+let captured_patchwork_view: option(Js.Unsafe.any) = {
+  let doc = Js.Unsafe.get(Js.Unsafe.global, "document");
+  if (!Js.Optdef.test(doc)) {
+    None;
+  } else {
+    let cs = Js.Unsafe.get(doc, "currentScript");
+    if (Js.Opt.test(cs)) {
+      let pv =
+        Js.Unsafe.meth_call(
+          cs,
+          "closest",
+          [|Js.Unsafe.inject(Js.string("patchwork-view"))|],
+        );
+      if (Js.Opt.test(pv)) {
+        Some(pv);
+      } else {
+        None;
+      };
+    } else {
+      None;
+    };
+  };
+};
+
+/* Find the repo instance: prefer element.repo (from the patchwork-view
+   element), then fall back to window.repo */
+let find_repo = (): option(Js.t(repo)) => {
+  switch (captured_patchwork_view) {
+  | Some(el) =>
+    let r = Js.Unsafe.get(el, "repo");
+    if (Js.Optdef.test(r)) {
+      Some(Js.Unsafe.coerce(r));
+    } else if (Js.Optdef.test(Js.Unsafe.global##.repo)) {
+      Some(get_repo());
+    } else {
+      None;
+    };
+  | None =>
+    if (Js.Optdef.test(Js.Unsafe.global##.repo)) {
+      Some(get_repo());
+    } else {
+      None;
+    }
+  };
+};
+
+/* Write JSON data to an automerge document.
+   When running in an iframe (patchworkWriteToDoc injected by hazel-tool.js),
+   delegates to the parent-realm helper to avoid cross-realm object issues.
+   Falls back to direct repo write for standalone mode. */
+let write_to_doc = (url: string, json_string: string): unit => {
+  /* Prefer the parent-realm helper (avoids cross-realm proxy errors) */
+  let helper = Js.Unsafe.get(Js.Unsafe.global, "patchworkWriteToDoc");
+  if (Js.Optdef.test(helper) && Js.typeof(helper) == Js.string("function")) {
+    Js.Unsafe.fun_call(
+      helper,
+      [|
+        Js.Unsafe.inject(Js.string(url)),
+        Js.Unsafe.inject(Js.string(json_string)),
+      |],
+    );
+  } else {
+    /* Standalone fallback: write directly via repo */
+    switch (find_repo()) {
+    | None => ()
+    | Some(repo) =>
+      let promise: Js.t(promise) = repo##find(Js.string(url));
+      ignore(
+        promise##then_(
+          Js.wrap_callback((handle: Js.t(handle)) => {
+            let parsed =
+              Js.Unsafe.meth_call(
+                Js.Unsafe.global##._JSON,
+                "parse",
+                [|Js.Unsafe.inject(Js.string(json_string))|],
+              );
+            Js.Unsafe.meth_call(
+              handle,
+              "change",
+              [|
+                Js.Unsafe.inject(
+                  Js.wrap_callback((doc: Js.Unsafe.any) =>
+                    if (Js.typeof(parsed) == Js.string("object")
+                        && !Js.equals(parsed, Js.Unsafe.inject(Js.null))) {
+                      let doc_keys =
+                        Js.to_array(
+                          Js.Unsafe.meth_call(
+                            Js.Unsafe.global##._Object,
+                            "keys",
+                            [|Js.Unsafe.inject(doc)|],
+                          ),
+                        );
+                      Array.iter(
+                        key => {
+                          let k = Js.to_string(key);
+                          if (!
+                                Js.to_bool(
+                                  Js.Unsafe.meth_call(
+                                    parsed,
+                                    "hasOwnProperty",
+                                    [|Js.Unsafe.inject(key)|],
+                                  ),
+                                )) {
+                            Js.Unsafe.delete(doc, Js.string(k));
+                          };
+                        },
+                        doc_keys,
+                      );
+                      let parsed_keys =
+                        Js.to_array(
+                          Js.Unsafe.meth_call(
+                            Js.Unsafe.global##._Object,
+                            "keys",
+                            [|Js.Unsafe.inject(parsed)|],
+                          ),
+                        );
+                      Array.iter(
+                        key => {
+                          let k = Js.to_string(key);
+                          Js.Unsafe.set(
+                            doc,
+                            Js.string(k),
+                            Js.Unsafe.get(parsed, key),
+                          );
+                        },
+                        parsed_keys,
+                      );
+                    }
+                  ),
+                ),
+              |],
+            )
+            |> ignore;
+          }),
+        ),
+      );
+    };
+  };
+};
```

</details>

Setting up this prebundle script requires a lot of new dependencies.

<details open>
<summary><code>hazel.opam</code> · "ts2ocaml-jsoo-stdlib"</summary>

<!-- changetour:hunk file=hazel.opam level=1 baseBlob=1a2d609fc482b8b9ef9f22bc3447becbfa714e7d -->

```diff
@@ -33,6 +33,7 @@ depends: [
   "ppx_deriving_qcheck"
   "bignum"
   "csv"
+  "ts2ocaml-jsoo-stdlib"
   "odoc" {with-doc}
 ]
 build: [
```

</details>

<details open>
<summary><code>hazel.opam.locked</code> · "ts2ocaml-jsoo-stdlib" {= "1.4.6"}</summary>

<!-- changetour:hunk file=hazel.opam.locked level=1 baseBlob=b59f35d6eebda4ecf8f6bda870faf5b50f0e70ed -->

```diff
@@ -223,6 +223,7 @@ depends: [
   "time_now" {= "v0.16.0"}
   "timezone" {= "v0.16.0"}
   "topkg" {= "1.1.1"}
+  "ts2ocaml-jsoo-stdlib" {= "1.4.6"}
   "typerep" {= "v0.16.0"}
   "tyxml" {= "4.6.0"}
   "uchar" {= "0.0.2"}
```

</details>

<details open>
<summary><code>hazel.opam.locked</code> · pin-depends: [</summary>

<!-- changetour:hunk file=hazel.opam.locked level=1 baseBlob=b59f35d6eebda4ecf8f6bda870faf5b50f0e70ed -->

```diff
@@ -256,3 +257,9 @@ build: [
   ]
 ]
 dev-repo: "git+https://github.com/hazelgrove/hazel.git"
+pin-depends: [
+  [
+    "ts2ocaml-jsoo-stdlib.1.4.6"
+    "git+https://github.com/ocsigen/ts2ocaml.git#jsoo-stdlib-v1.4.6"
+  ]
+]
```

</details>

<details open>
<summary><code>package-lock.json</code> · "@automerge/vanillajs": "^2.5.0",</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -8,6 +8,7 @@
       "name": "hazel",
       "license": "MIT",
       "dependencies": {
+        "@automerge/vanillajs": "^2.5.0",
         "@esbuild-plugins/node-resolve": "^0.2.2",
         "algebrite": "^1.4.0",
         "hotkeys-js": "^3.8.7",
```

</details>

<details open>
<summary><code>package-lock.json</code> · "esbuild-plugin-wasm": "^1.1.0",</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -16,10 +17,167 @@
       "devDependencies": {
         "@types/node": "^22.14.0",
         "esbuild": "^0.25.1",
+        "esbuild-plugin-wasm": "^1.1.0",
         "vite": "^6.4.2",
         "vite-plugin-static-copy": "^2.3.2"
       }
     },
+    "node_modules/@automerge/automerge": {
+      "version": "3.2.6",
+      "resolved": "https://registry.npmjs.org/@automerge/automerge/-/automerge-3.2.6.tgz",
+      "integrity": "sha512-9/GXXfYYWNVGpnbRrGQzTNU4fWZ3XaEMeEg0OrpK4pvlQSpkmUBoirEb/4TMK6BwMysZGV5Yeneq3wwc7RNGfg==",
+      "license": "MIT"
+    },
+    "node_modules/@automerge/automerge-repo": {
+      "version": "2.5.6",
+      "resolved": "https://registry.npmjs.org/@automerge/automerge-repo/-/automerge-repo-2.5.6.tgz",
+      "integrity": "sha512-ZXM6TOAwm192g3+zIxYvlB+Z3O00NP+psErOwvbSype8fFO+dhc8tB/jPwfZJqmn1ULUz5w7gssKEynwcYFRSA==",
+      "license": "MIT",
+      "dependencies": {
+        "@automerge/automerge": "2.2.8 - 3",
+        "bs58check": "^3.0.1",
+        "cbor-x": "^1.3.0",
+        "debug": "^4.3.4",
+        "eventemitter3": "^5.0.1",
+        "fast-sha256": "^1.3.0",
+        "uuid": "^9.0.0",
+        "xstate": "^5.9.1"
+      }
+    },
+    "node_modules/@automerge/automerge-repo-network-broadcastchannel": {
+      "version": "2.5.6",
+      "resolved": "https://registry.npmjs.org/@automerge/automerge-repo-network-broadcastchannel/-/automerge-repo-network-broadcastchannel-2.5.6.tgz",
+      "integrity": "sha512-eHE8edlYRfwFnQ1JBKuAyxqKg7TNVFbloe5QgbM1CH8khAunLdeYJcpAAxWkPEU1iQOUjU5oLowDm7BQbsfEJw==",
+      "license": "MIT",
+      "dependencies": {
+        "@automerge/automerge-repo": "2.5.6"
+      }
+    },
+    "node_modules/@automerge/automerge-repo-network-messagechannel": {
+      "version": "2.5.6",
+      "resolved": "https://registry.npmjs.org/@automerge/automerge-repo-network-messagechannel/-/automerge-repo-network-messagechannel-2.5.6.tgz",
+      "integrity": "sha512-0xd8UMD3hMS3XnC5Rl40pAGqpkaW06Ox/GgEabKGkJM2d8bcVe2w1eklzOr7zJUIhDGrzPwFdCDNq1iNUHo5VA==",
+      "license": "MIT",
+      "dependencies": {
+        "@automerge/automerge-repo": "2.5.6",
+        "debug": "^4.3.4",
+        "eventemitter3": "^5.0.1"
+      }
+    },
+    "node_modules/@automerge/automerge-repo-network-websocket": {
+      "version": "2.5.6",
+      "resolved": "https://registry.npmjs.org/@automerge/automerge-repo-network-websocket/-/automerge-repo-network-websocket-2.5.6.tgz",
+      "integrity": "sha512-xs/xzOCTf5ZuqhDUtBO1VQ3QOnunEobjtgEqGNu7UcoD2uYWET1ANHVfS+ZzSV8IOA1O9jFAPXxzc4FXoSL0hQ==",
+      "license": "MIT",
+      "dependencies": {
+        "@automerge/automerge-repo": "2.5.6",
+        "cbor-x": "^1.3.0",
+        "debug": "^4.3.4",
+        "eventemitter3": "^5.0.1",
+        "isomorphic-ws": "^5.0.0",
+        "ws": "^8.7.0"
+      }
+    },
+    "node_modules/@automerge/automerge-repo-storage-indexeddb": {
+      "version": "2.5.6",
+      "resolved": "https://registry.npmjs.org/@automerge/automerge-repo-storage-indexeddb/-/automerge-repo-storage-indexeddb-2.5.6.tgz",
+      "integrity": "sha512-3349orECq2Mj3mo9q6f9ZeXwDkP3lAm6KaivP5+znUpv6Ze+IraE36DdL5GVdzIm+cXej0yWSOCQOalnTMPcQg==",
+      "license": "MIT",
+      "dependencies": {
+        "@automerge/automerge-repo": "2.5.6"
+      }
+    },
+    "node_modules/@automerge/vanillajs": {
+      "version": "2.5.6",
+      "resolved": "https://registry.npmjs.org/@automerge/vanillajs/-/vanillajs-2.5.6.tgz",
+      "integrity": "sha512-n9R2c2ORn/kKCZmZ5aTEatfqDEw4OwJLWQtNbKvjWkfK1I+ru9vqL4cB023P6Gj4FtVj4HNybDSrLnxrxmKnsQ==",
+      "license": "MIT",
+      "dependencies": {
+        "@automerge/automerge-repo": "2.5.6",
+        "@automerge/automerge-repo-network-broadcastchannel": "2.5.6",
+        "@automerge/automerge-repo-network-messagechannel": "2.5.6",
+        "@automerge/automerge-repo-network-websocket": "2.5.6",
+        "@automerge/automerge-repo-storage-indexeddb": "2.5.6"
+      }
+    },
+    "node_modules/@cbor-extract/cbor-extract-darwin-arm64": {
+      "version": "2.2.2",
+      "resolved": "https://registry.npmjs.org/@cbor-extract/cbor-extract-darwin-arm64/-/cbor-extract-darwin-arm64-2.2.2.tgz",
+      "integrity": "sha512-ZKZ/F8US7JR92J4DMct6cLW/Y66o2K576+zjlEN/MevH70bFIsB10wkZEQPLzl2oNh2SMGy55xpJ9JoBRl5DOA==",
+      "cpu": [
+        "arm64"
+      ],
+      "license": "MIT",
+      "optional": true,
+      "os": [
+        "darwin"
+      ]
+    },
+    "node_modules/@cbor-extract/cbor-extract-darwin-x64": {
+      "version": "2.2.2",
+      "resolved": "https://registry.npmjs.org/@cbor-extract/cbor-extract-darwin-x64/-/cbor-extract-darwin-x64-2.2.2.tgz",
+      "integrity": "sha512-32b1mgc+P61Js+KW9VZv/c+xRw5EfmOcPx990JbCBSkYJFY0l25VinvyyWfl+3KjibQmAcYwmyzKF9J4DyKP/Q==",
+      "cpu": [
+        "x64"
+      ],
+      "license": "MIT",
+      "optional": true,
+      "os": [
+        "darwin"
+      ]
+    },
+    "node_modules/@cbor-extract/cbor-extract-linux-arm": {
+      "version": "2.2.2",
+      "resolved": "https://registry.npmjs.org/@cbor-extract/cbor-extract-linux-arm/-/cbor-extract-linux-arm-2.2.2.tgz",
+      "integrity": "sha512-tNg0za41TpQfkhWjptD+0gSD2fggMiDCSacuIeELyb2xZhr7PrhPe5h66Jc67B/5dmpIhI2QOUtv4SBsricyYQ==",
+      "cpu": [
+        "arm"
+      ],
+      "license": "MIT",
+      "optional": true,
+      "os": [
+        "linux"
+      ]
+    },
+    "node_modules/@cbor-extract/cbor-extract-linux-arm64": {
+      "version": "2.2.2",
+      "resolved": "https://registry.npmjs.org/@cbor-extract/cbor-extract-linux-arm64/-/cbor-extract-linux-arm64-2.2.2.tgz",
+      "integrity": "sha512-wfqgzqCAy/Vn8i6WVIh7qZd0DdBFaWBjPdB6ma+Wihcjv0gHqD/mw3ouVv7kbbUNrab6dKEx/w3xQZEdeXIlzg==",
+      "cpu": [
+        "arm64"
+      ],
+      "license": "MIT",
+      "optional": true,
+      "os": [
+        "linux"
+      ]
+    },
+    "node_modules/@cbor-extract/cbor-extract-linux-x64": {
+      "version": "2.2.2",
+      "resolved": "https://registry.npmjs.org/@cbor-extract/cbor-extract-linux-x64/-/cbor-extract-linux-x64-2.2.2.tgz",
+      "integrity": "sha512-rpiLnVEsqtPJ+mXTdx1rfz4RtUGYIUg2rUAZgd1KjiC1SehYUSkJN7Yh+aVfSjvCGtVP0/bfkQkXpPXKbmSUaA==",
+      "cpu": [
+        "x64"
+      ],
+      "license": "MIT",
+      "optional": true,
+      "os": [
+        "linux"
+      ]
+    },
+    "node_modules/@cbor-extract/cbor-extract-win32-x64": {
+      "version": "2.2.2",
+      "resolved": "https://registry.npmjs.org/@cbor-extract/cbor-extract-win32-x64/-/cbor-extract-win32-x64-2.2.2.tgz",
+      "integrity": "sha512-dI+9P7cfWxkTQ+oE+7Aa6onEn92PHgfWXZivjNheCRmTBDBf2fx6RyTi0cmgpYLnD1KLZK9ZYrMxaPZ4oiXhGA==",
+      "cpu": [
+        "x64"
+      ],
+      "license": "MIT",
+      "optional": true,
+      "os": [
+        "win32"
+      ]
+    },
     "node_modules/@esbuild-plugins/node-resolve": {
       "version": "0.2.2",
       "resolved": "https://registry.npmjs.org/@esbuild-plugins/node-resolve/-/node-resolve-0.2.2.tgz",
```

</details>

<details open>
<summary><code>package-lock.json</code> · "node_modules/@noble/hashes": {</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -461,6 +619,18 @@
         "tslib": "^2.0.1"
       }
     },
+    "node_modules/@noble/hashes": {
+      "version": "1.8.0",
+      "resolved": "https://registry.npmjs.org/@noble/hashes/-/hashes-1.8.0.tgz",
+      "integrity": "sha512-jCs9ldd7NwzpgXDIf6P3+NrHh9/sD6CQdxHyjQI+h/6rDNo88ypBxxz45UDuZHz9r3tNz7N/VInSVoVdtXEI4A==",
+      "license": "MIT",
+      "engines": {
+        "node": "^14.21.3 || >=16"
+      },
+      "funding": {
+        "url": "https://paulmillr.com/funding/"
+      }
+    },
     "node_modules/@nodelib/fs.scandir": {
       "version": "2.1.5",
       "resolved": "https://registry.npmjs.org/@nodelib/fs.scandir/-/fs.scandir-2.1.5.tgz",
```

</details>

<details open>
<summary><code>package-lock.json</code> · "node_modules/base-x": {</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -831,6 +1001,12 @@
         "node": ">= 8"
       }
     },
+    "node_modules/base-x": {
+      "version": "4.0.1",
+      "resolved": "https://registry.npmjs.org/base-x/-/base-x-4.0.1.tgz",
+      "integrity": "sha512-uAZ8x6r6S3aUM9rbHGVOIsR15U/ZSc82b3ymnCPsT45Gk1DDvhDPdIgB5MrhirZWt+5K0EEPQH985kNqZgNPFw==",
+      "license": "MIT"
+    },
     "node_modules/big-integer": {
       "version": "1.6.52",
       "resolved": "https://registry.npmjs.org/big-integer/-/big-integer-1.6.52.tgz",
```

</details>

<details open>
<summary><code>package-lock.json</code> · "node_modules/bs58": {</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -866,6 +1042,56 @@
         "node": ">=8"
       }
     },
+    "node_modules/bs58": {
+      "version": "5.0.0",
+      "resolved": "https://registry.npmjs.org/bs58/-/bs58-5.0.0.tgz",
+      "integrity": "sha512-r+ihvQJvahgYT50JD05dyJNKlmmSlMoOGwn1lCcEzanPglg7TxYjioQUYehQ9mAR/+hOSd2jRc/Z2y5UxBymvQ==",
+      "license": "MIT",
+      "dependencies": {
+        "base-x": "^4.0.0"
+      }
+    },
+    "node_modules/bs58check": {
+      "version": "3.0.1",
+      "resolved": "https://registry.npmjs.org/bs58check/-/bs58check-3.0.1.tgz",
+      "integrity": "sha512-hjuuJvoWEybo7Hn/0xOrczQKKEKD63WguEjlhLExYs2wUBcebDC1jDNK17eEAD2lYfw82d5ASC1d7K3SWszjaQ==",
+      "license": "MIT",
+      "dependencies": {
+        "@noble/hashes": "^1.2.0",
+        "bs58": "^5.0.0"
+      }
+    },
+    "node_modules/cbor-extract": {
+      "version": "2.2.2",
+      "resolved": "https://registry.npmjs.org/cbor-extract/-/cbor-extract-2.2.2.tgz",
+      "integrity": "sha512-hlSxxI9XO2yQfe9g6msd3g4xCfDqK5T5P0fRMLuaLHhxn4ViPrm+a+MUfhrvH2W962RGxcBwEGzLQyjbDG1gng==",
+      "hasInstallScript": true,
+      "license": "MIT",
+      "optional": true,
+      "dependencies": {
+        "node-gyp-build-optional-packages": "5.1.1"
+      },
+      "bin": {
+        "download-cbor-prebuilds": "bin/download-prebuilds.js"
+      },
+      "optionalDependencies": {
+        "@cbor-extract/cbor-extract-darwin-arm64": "2.2.2",
+        "@cbor-extract/cbor-extract-darwin-x64": "2.2.2",
+        "@cbor-extract/cbor-extract-linux-arm": "2.2.2",
+        "@cbor-extract/cbor-extract-linux-arm64": "2.2.2",
+        "@cbor-extract/cbor-extract-linux-x64": "2.2.2",
+        "@cbor-extract/cbor-extract-win32-x64": "2.2.2"
+      }
+    },
+    "node_modules/cbor-x": {
+      "version": "1.6.4",
+      "resolved": "https://registry.npmjs.org/cbor-x/-/cbor-x-1.6.4.tgz",
+      "integrity": "sha512-UGKHjp6RHC6QuZ2yy5LCKm7MojM4716DwoSaqwQpaH4DvZvbBTGcoDNTiG9Y2lByXZYFEs9WRkS5tLl96IrF1Q==",
+      "license": "MIT",
+      "optionalDependencies": {
+        "cbor-extract": "^2.2.2"
+      }
+    },
     "node_modules/chokidar": {
       "version": "3.6.0",
       "resolved": "https://registry.npmjs.org/chokidar/-/chokidar-3.6.0.tgz",
```

</details>

<details open>
<summary><code>package-lock.json</code> · "node_modules/detect-libc": {</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -908,6 +1134,16 @@
         }
       }
     },
+    "node_modules/detect-libc": {
+      "version": "2.1.2",
+      "resolved": "https://registry.npmjs.org/detect-libc/-/detect-libc-2.1.2.tgz",
+      "integrity": "sha512-Btj2BOOO83o3WyH59e8MgXsxEQVcarkUOpEYrubB0urwnN10yQ364rsiByU11nZlqWYZm05i/of7io4mzihBtQ==",
+      "license": "Apache-2.0",
+      "optional": true,
+      "engines": {
+        "node": ">=8"
+      }
+    },
     "node_modules/esbuild": {
       "version": "0.25.1",
       "resolved": "https://registry.npmjs.org/esbuild/-/esbuild-0.25.1.tgz",
```

</details>

<details open>
<summary><code>package-lock.json</code> · "node_modules/esbuild-plugin-wasm": {</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -948,6 +1184,20 @@
         "@esbuild/win32-x64": "0.25.1"
       }
     },
+    "node_modules/esbuild-plugin-wasm": {
+      "version": "1.1.0",
+      "resolved": "https://registry.npmjs.org/esbuild-plugin-wasm/-/esbuild-plugin-wasm-1.1.0.tgz",
+      "integrity": "sha512-0bQ6+1tUbySSnxzn5jnXHMDvYnT0cN/Wd4Syk8g/sqAIJUg7buTIi22svS3Qz6ssx895NT+TgLPb33xi1OkZig==",
+      "dev": true,
+      "license": "MIT",
+      "engines": {
+        "node": ">=0.10.0"
+      },
+      "funding": {
+        "type": "individual",
+        "url": "https://ko-fi.com/tschrock"
+      }
+    },
     "node_modules/escape-string-regexp": {
       "version": "4.0.0",
       "resolved": "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-4.0.0.tgz",
```

</details>

<details open>
<summary><code>package-lock.json</code> · "node_modules/eventemitter3": {</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -960,6 +1210,12 @@
         "url": "https://github.com/sponsors/sindresorhus"
       }
     },
+    "node_modules/eventemitter3": {
+      "version": "5.0.4",
+      "resolved": "https://registry.npmjs.org/eventemitter3/-/eventemitter3-5.0.4.tgz",
+      "integrity": "sha512-mlsTRyGaPBjPedk6Bvw+aqbsXDtoAyAzm5MO7JgU+yVRyMQ5O8bD4Kcci7BS85f93veegeCPkL8R4GLClnjLFw==",
+      "license": "MIT"
+    },
     "node_modules/fast-glob": {
       "version": "3.3.3",
       "resolved": "https://registry.npmjs.org/fast-glob/-/fast-glob-3.3.3.tgz",
```

</details>

<details open>
<summary><code>package-lock.json</code> · "node_modules/fast-sha256": {</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -977,6 +1233,12 @@
         "node": ">=8.6.0"
       }
     },
+    "node_modules/fast-sha256": {
+      "version": "1.3.0",
+      "resolved": "https://registry.npmjs.org/fast-sha256/-/fast-sha256-1.3.0.tgz",
+      "integrity": "sha512-n11RGP/lrWEFI/bWdygLxhI+pVeo1ZYIVwvvPkW7azl/rOy+F3HYRZ2K5zeE9mmkhQppyv9sQFx0JM9UabnpPQ==",
+      "license": "Unlicense"
+    },
     "node_modules/fastq": {
       "version": "1.19.1",
       "resolved": "https://registry.npmjs.org/fastq/-/fastq-1.19.1.tgz",
```

</details>

<details open>
<summary><code>package-lock.json</code> · "node_modules/isomorphic-ws": {</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -1138,6 +1400,15 @@
         "node": ">=0.12.0"
       }
     },
+    "node_modules/isomorphic-ws": {
+      "version": "5.0.0",
+      "resolved": "https://registry.npmjs.org/isomorphic-ws/-/isomorphic-ws-5.0.0.tgz",
+      "integrity": "sha512-muId7Zzn9ywDsyXgTIafTry2sV3nySZeUDe6YedVd1Hvuuep5AsIlqK+XefWpYTyJG5e503F2xIuT2lcU6rCSw==",
+      "license": "MIT",
+      "peerDependencies": {
+        "ws": "*"
+      }
+    },
     "node_modules/jsonfile": {
       "version": "6.1.0",
       "resolved": "https://registry.npmjs.org/jsonfile/-/jsonfile-6.1.0.tgz",
```

</details>

<details open>
<summary><code>package-lock.json</code> · "node_modules/node-gyp-build-optional-packages": {</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -1242,6 +1513,21 @@
         "lit": "2.2.6"
       }
     },
+    "node_modules/node-gyp-build-optional-packages": {
+      "version": "5.1.1",
+      "resolved": "https://registry.npmjs.org/node-gyp-build-optional-packages/-/node-gyp-build-optional-packages-5.1.1.tgz",
+      "integrity": "sha512-+P72GAjVAbTxjjwUmwjVrqrdZROD4nf8KgpBoDxqXXTiYZZt/ud60dE5yvCSr9lRO8e8yv6kgJIC0K0PfZFVQw==",
+      "license": "MIT",
+      "optional": true,
+      "dependencies": {
+        "detect-libc": "^2.0.1"
+      },
+      "bin": {
+        "node-gyp-build-optional-packages": "bin.js",
+        "node-gyp-build-optional-packages-optional": "optional.js",
+        "node-gyp-build-optional-packages-test": "build-test.js"
+      }
+    },
     "node_modules/normalize-path": {
       "version": "3.0.0",
       "resolved": "https://registry.npmjs.org/normalize-path/-/normalize-path-3.0.0.tgz",
```

</details>

<details open>
<summary><code>package-lock.json</code> · "node_modules/uuid": {</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -1512,6 +1798,20 @@
         "node": ">= 10.0.0"
       }
     },
+    "node_modules/uuid": {
+      "version": "9.0.1",
+      "resolved": "https://registry.npmjs.org/uuid/-/uuid-9.0.1.tgz",
+      "integrity": "sha512-b+1eJOlsR9K8HJpow9Ok3fiWOWSIcIzXodvv0rQjVoOVNpWMpxf1wZNpt4y9h10odCNrqnYp1OBzRktckBe3sA==",
+      "deprecated": "uuid@10 and below is no longer supported.  For ESM codebases, update to uuid@latest.  For CommonJS codebases, use uuid@11 (but be aware this version will likely be deprecated in 2028).",
+      "funding": [
+        "https://github.com/sponsors/broofa",
+        "https://github.com/sponsors/ctavan"
+      ],
+      "license": "MIT",
+      "bin": {
+        "uuid": "dist/bin/uuid"
+      }
+    },
     "node_modules/vite": {
       "version": "6.4.2",
       "resolved": "https://registry.npmjs.org/vite/-/vite-6.4.2.tgz",
```

</details>

<details open>
<summary><code>package-lock.json</code> · },</summary>

<!-- changetour:hunk file=package-lock.json level=1 baseBlob=51e3d158f7752841ca0858708935b1c17f3b176a -->

```diff
@@ -1674,6 +1974,37 @@
         "@rollup/rollup-win32-x64-msvc": "4.39.0",
         "fsevents": "~2.3.2"
       }
+    },
+    "node_modules/ws": {
+      "version": "8.21.0",
+      "resolved": "https://registry.npmjs.org/ws/-/ws-8.21.0.tgz",
+      "integrity": "sha512-Vsp28b7DRcimFQvrqu2Wek3z1iYxDCWqHYB8Qsnk/S4RfaCQzPGPyBNuVjJV3cd6UiKtUtp6sNM77gWvzcCH+g==",
+      "license": "MIT",
+      "engines": {
+        "node": ">=10.0.0"
+      },
+      "peerDependencies": {
+        "bufferutil": "^4.0.1",
+        "utf-8-validate": ">=5.0.2"
+      },
+      "peerDependenciesMeta": {
+        "bufferutil": {
+          "optional": true
+        },
+        "utf-8-validate": {
+          "optional": true
+        }
+      }
+    },
+    "node_modules/xstate": {
+      "version": "5.32.1",
+      "resolved": "https://registry.npmjs.org/xstate/-/xstate-5.32.1.tgz",
+      "integrity": "sha512-IGX9q5vEOplWjVq79edfgjJfVV/lXCup9p/fQGgUabAveZrlRwWJ/mC2iZEE7wswXbWITBCoS7gmoFfcuWAwsQ==",
+      "license": "MIT",
+      "funding": {
+        "type": "opencollective",
+        "url": "https://opencollective.com/xstate"
+      }
     }
   }
 }
```

</details>

<details open>
<summary><code>package.json</code> · "@automerge/vanillajs": "^2.5.0",</summary>

<!-- changetour:hunk file=package.json level=1 baseBlob=fb04f79205bfacfa36d02b9c46d1151542cb5837 -->

```diff
@@ -10,6 +10,7 @@
   },
   "homepage": "https://hazel.org",
   "dependencies": {
+    "@automerge/vanillajs": "^2.5.0",
     "@esbuild-plugins/node-resolve": "^0.2.2",
     "algebrite": "^1.4.0",
     "hotkeys-js": "^3.8.7",
```

</details>

<details open>
<summary><code>package.json</code> · "esbuild-plugin-wasm": "^1.1.0",</summary>

<!-- changetour:hunk file=package.json level=1 baseBlob=fb04f79205bfacfa36d02b9c46d1151542cb5837 -->

```diff
@@ -18,6 +19,7 @@
   "devDependencies": {
     "@types/node": "^22.14.0",
     "esbuild": "^0.25.1",
+    "esbuild-plugin-wasm": "^1.1.0",
     "vite": "^6.4.2",
     "vite-plugin-static-copy": "^2.3.2"
   }
```

</details>

<details open>
<summary><code>src/haz3lcore/dune</code> · (libraries language)</summary>

<!-- changetour:hunk file=src/haz3lcore/dune level=1 baseBlob=83b10beb261e88d3a932fde170049952d3878695 -->

```diff
@@ -2,7 +2,7 @@

 (library
  (name haz3lcore)
- (libraries language)
+ (libraries language ts2ocaml-jsoo-stdlib)
  (js_of_ocaml)
  (instrumentation
   (backend bisect_ppx))
```

</details>
