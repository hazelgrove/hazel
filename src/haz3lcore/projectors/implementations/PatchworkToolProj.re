open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Js_of_ocaml;

/* Patchwork Tool Projector: embeds a Patchwork tool (Petrinaut, CatColab, etc.)
   inside a Hazel program via <patchwork-view>, subscribes to the same automerge
   document for live data, and converts it to HazelJSON to update the underlying
   syntax — so the program can use the tool's data. */

type patchwork_tool = {
  id: string,
  name: string,
  width: int,
  height: int,
};

/* Default dimensions for tools from the registry that don't specify size */
let default_width = 680;
let default_height = 490;

/* Known dimension overrides for tools whose default size differs */
let known_dimensions: list((string, (int, int))) = [
  ("petrinaut", (1050, 590)),
];

let dimensions_for = (id: string): (int, int) =>
  switch (List.assoc_opt(id, known_dimensions)) {
  | Some(dims) => dims
  | None => (default_width, default_height)
  };

/* Read available tools from the patchwork plugin registry
   (window.patchworkToolRegistry, set by prebundle.js via getRegistry("patchwork:tool")).
   Falls back to an empty list if the registry isn't available. */
let get_tools_from_registry = (): list(patchwork_tool) => {
  let registry = Js.Unsafe.global##.patchworkToolRegistry;
  if (Js.Optdef.test(registry)) {
    let plugins: array(Js.Unsafe.any) =
      Js.to_array(Js.Unsafe.meth_call(registry, "all", [||]));
    plugins
    |> Array.to_list
    |> List.filter_map(plugin => {
         let id = Js.to_string(Js.Unsafe.get(plugin, "id"));
         let name =
           if (Js.Optdef.test(Js.Unsafe.get(plugin, "name"))) {
             Js.to_string(Js.Unsafe.get(plugin, "name"));
           } else {
             id;
           };
         let (width, height) = dimensions_for(id);
         Some({
           id,
           name,
           width,
           height,
         });
       });
  } else {
    [];
  };
};

let tools = (): list(patchwork_tool) => get_tools_from_registry();

let find_tool = (id: string): option(patchwork_tool) =>
  List.find_opt(t => t.id == id, tools());

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type last_load =
    | Succeeded
    | Failed;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    tool: string,
    url: string,
    last_load: option(last_load),
    hot_reload: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetTool(string)
    | SetUrl(string)
    | SetLastLoad(last_load)
    | ToggleHotReload;

  let init = (a: Language.Any.t): option(model) =>
    switch (a) {
    | Exp(_) =>
      Some({
        tool: "",
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
        | _ => failwith("PatchworkToolProj: put: not expression"),
        Inline.Block,
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("PatchworkToolProj: put: lift failed")
    };

  let input_id = (id: Id.t): string => Id.cls(id) ++ "-pt-input";
  let patchwork_view_id = (id: Id.t): string => Id.cls(id) ++ "-pt-view";

  let focus_pointer = (id: Id.t) => {
    /* Focus the patchwork-view element so that TLDraw receives
       keyboard events (tool shortcuts, backspace to delete, etc.)
       instead of having them escape to Hazel's editor. */
    let el = JsUtil.get_elem_by_id(patchwork_view_id(id));
    el##focus;
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

  let url_placeholder = "automerge:<doc-url>";

  let placeholder = (model, _info) => {
    let m = font_metrics^;
    let px_to_grid = (value: int, multiple: float): int =>
      int_of_float(ceil(float_of_int(value) /. multiple));
    /* horizontal only covers the tab bar (inline portion);
       the tool pane below overflows via CSS */
    let url_len = String.length(model.url);
    let display_len = max(String.length(url_placeholder), url_len);
    /* +12 for focus dot, reload btn, hot reload toggle, status, margins;
       +14 for tool selector dropdown (longest option "Select tool...") */
    let horizontal = display_len + 26;
    /* Only reserve vertical space for the tool pane when the tool is
       connected (or hasn't failed yet). Otherwise just the tab bar. */
    let has_tool = String.length(model.tool) > 0;
    let has_url = String.length(model.url) > 0;
    let url_failed = model.last_load == Some(Failed);
    let rows =
      if (has_tool && has_url && !url_failed) {
        let tool_config = find_tool(model.tool);
        let tool_height =
          switch (tool_config) {
          | Some(t) => t.height
          | None => 200
          };
        px_to_grid(tool_height, m.row_height);
      } else {
        1;
      };
    ProjectorCore.Shape.{
      horizontal,
      vertical: Tab(rows),
    };
  };

  let update = (m: model, _info: info, action: action): model =>
    switch (action) {
    | SetTool(tool) => {
        ...m,
        tool,
      }
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
    let load_status_cls =
      switch (model.last_load) {
      | None when String.length(model.url) > 0 => "load-none"
      | None => ""
      | Some(Succeeded) => "load-succeeded"
      | Some(Failed) => "load-failed"
      };

    /* --- URL input with keyboard handling --- */
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
          Attr.class_("patchwork-tool-url-input"),
          Attr.placeholder(url_placeholder),
          Attr.string_property("value", model.url),
          Attr.on_input((_evt, value) => {
            let null_exp =
              Language.IdTagged.FreshGrammar.Exp.constructor("Null", None);
            let seg = put(info, null_exp);
            Effect.(Many([local(SetUrl(value)), parent(SetSyntax(seg))]));
          }),
          Attr.on_keydown(key_handler),
          Attr.on_pointerdown(evt => {
            /* Stop propagation so the projector wrapper doesn't
               steal focus away from the URL input to patchwork-view */
            Js.Unsafe.meth_call(evt, "stopPropagation", [||]) |> ignore;
            Effect.Ignore;
          }),
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

    /* --- Tool selector dropdown --- */
    let tool_options =
      [
        Node.create(
          "option",
          ~attrs=[
            Attr.string_property("value", ""),
            Attr.bool_property("disabled", true),
            Attr.bool_property("selected", String.length(model.tool) == 0),
          ],
          [Node.text("Select tool...")],
        ),
      ]
      @ List.map(
          (t: patchwork_tool) =>
            Node.create(
              "option",
              ~attrs=[
                Attr.string_property("value", t.id),
                Attr.bool_property("selected", model.tool == t.id),
              ],
              [Node.text(t.name)],
            ),
          tools(),
        );

    let tool_select =
      Node.create(
        "select",
        ~attrs=[
          Attr.class_("patchwork-tool-select"),
          Attr.on_change((_, value) => local(SetTool(value))),
          Attr.on_pointerdown(evt => {
            Js.Unsafe.meth_call(evt, "stopPropagation", [||]) |> ignore;
            Effect.Ignore;
          }),
        ],
        tool_options,
      );

    /* --- Automerge subscription (reuses AutomergeProj infrastructure) --- */
    let on_data = (exp: Language.Exp.t) => {
      ProjectorCore.set_bypass(info.id, exp);
      let null_exp =
        Language.IdTagged.FreshGrammar.Exp.constructor("Null", None);
      let seg = put(info, null_exp);
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

    AutomergeProj.ensure_subscribed(info.id, model.url, on_data, on_error);

    /* --- Hot reload toggle + manual reload button --- */
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
              let target = Js.Unsafe.get(evt, "currentTarget");
              JsUtil.rm_cls(target, "spinning");
              ignore(Js.Unsafe.get(target, "offsetWidth"));
              JsUtil.add_cls(target, "spinning");
              switch (Id.Map.find_opt(info.id, AutomergeProj.subscriptions^)) {
              | Some({handle: Some(h), _}) =>
                switch (Automerge.doc_to_exp(h)) {
                | Ok(exp) =>
                  ProjectorCore.set_bypass(info.id, exp);
                  let null_exp =
                    Language.IdTagged.FreshGrammar.Exp.constructor(
                      "Null",
                      None,
                    );
                  let seg = put(info, null_exp);
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
          Attr.on_mouseleave(evt => {
            JsUtil.rm_cls(Js.Unsafe.get(evt, "currentTarget"), "spinning");
            Effect.Ignore;
          }),
        ],
        [Node.text({js|🔄|js})],
      );

    /* --- Tool pane --- */
    let tool_config = find_tool(model.tool);
    let tool_pane =
      if (String.length(model.tool) > 0 && String.length(model.url) > 0) {
        let tool_width =
          switch (tool_config) {
          | Some(t) => t.width
          | None => 400
          };
        let tool_height =
          switch (tool_config) {
          | Some(t) => t.height
          | None => 200
          };
        Node.create(
          "patchwork-view",
          ~attrs=[
            Attr.id(patchwork_view_id(info.id)),
            Attr.create("doc-url", model.url),
            Attr.create("tool-id", model.tool),
            Attr.create("tabindex", "0"),
            Attr.create(
              "style",
              Printf.sprintf(
                "width: %dpx; height: %dpx; outline: none;",
                tool_width,
                tool_height,
              ),
            ),
          ],
          [],
        );
      } else if (String.length(model.tool) > 0) {
        Node.div(
          ~attrs=[Attr.classes(["patchwork-tool-placeholder"])],
          [Node.text("Enter an automerge document URL to connect.")],
        );
      } else {
        Node.div(
          ~attrs=[Attr.classes(["patchwork-tool-placeholder"])],
          [Node.text("Select a tool to get started.")],
        );
      };

    /* --- Assemble view --- */
    let tab_bar =
      Node.div(
        ~attrs=[Attr.classes(["patchwork-tool-tab-bar", "cols", "code"])],
        [
          Node.text({js|·|js}),
          tool_select,
          url_input,
          reload_btn,
          hot_reload_toggle,
        ],
      );

    View.mk(
      Node.div(
        ~attrs=[
          Attr.classes([
            "wrapper",
            "patchwork-tool-wrapper",
            load_status_cls,
          ]),
        ],
        [tab_bar, tool_pane],
      ),
    );
  };
};
