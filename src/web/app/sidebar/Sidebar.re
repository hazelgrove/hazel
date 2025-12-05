open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Js_of_ocaml;
open Util.JsUtil;

let tab = (~tooltip="", icon, action, isActive) => {
  let classes = ["tab"] @ (isActive ? ["active"] : []);
  div(
    ~attrs=[clss(classes), Attr.on_mousedown(action), Attr.title(tooltip)],
    [icon],
  );
};

let explain_this_tab = (~globals: Globals.t): Node.t => {
  let switch_explain_this = _ =>
    Effect.Many([
      globals.inject_global(
        Set(Sidebar(SwitchPanel(LanguageDocumentation))),
      ),
      Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["explain-this-button"])],
    [
      tab(
        Icons.explain_this,
        ~tooltip="Switch to Language Documentation",
        switch_explain_this,
        globals.settings.sidebar.panel == LanguageDocumentation
        && globals.settings.sidebar.show,
      ),
    ],
  );
};

let assistant_tab = (~globals: Globals.t): Node.t => {
  let switch_assistant = _ =>
    Effect.Many([
      globals.inject_global(Set(Sidebar(SwitchPanel(HelpfulAssistant)))),
      Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["assistant-button"])],
    [
      tab(
        Icons.hazelnut_agent,
        ~tooltip="Switch to Helpful Assistant",
        switch_assistant,
        globals.settings.sidebar.panel == HelpfulAssistant
        && globals.settings.sidebar.show,
      ),
    ],
  );
};

let collapse_tab = (~globals: Globals.t): Node.t => {
  let tooltip =
    globals.settings.sidebar.show ? "Collapse Sidebar" : "Expand Sidebar";
  let icon = globals.settings.sidebar.show ? Icons.collapse : Icons.expand;
  let switch_assistant = _ =>
    Effect.Many([
      globals.inject_global(Set(Sidebar(ToggleShow))),
      Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["collapse-button"])],
    [tab(icon, ~tooltip, switch_assistant, false)],
  );
};

let persistent_view = (~globals: Globals.t) =>
  div(
    ~attrs=[Attr.id("persistent")],
    [
      div(
        ~attrs=[clss(["tabs"])],
        [explain_this_tab(~globals), assistant_tab(~globals)],
      ),
    ],
  );

let updateElementStyles = (new_width: int) => {
  let elements = [
    ("side-bar", "width"),
    ("prompt-display-container", "right"),
    ("history-menu", "right"),
  ];
  List.iter(
    ((id, style)) => {
      switch (get_elem_by_id_opt(id)) {
      | Some(elem) =>
        let value =
          style == "width"
            ? string_of_int(new_width) ++ "px"
            : string_of_int(new_width + 20) ++ "px";
        let elem_style = Js.Unsafe.coerce(elem)##.style;
        switch (style) {
        | "width" => elem_style##.width := Js.string(value)
        | "right" => elem_style##.right := Js.string(value)
        | _ => ()
        };
      | None => ()
      }
    },
    elements,
  );
};

let resetElementStyles = () => {
  let elements = [
    ("side-bar", "width"),
    ("prompt-display-container", "right"),
    ("history-menu", "right"),
  ];
  List.iter(
    ((id, style)) => {
      switch (get_elem_by_id_opt(id)) {
      | Some(elem) =>
        let elem_style = Js.Unsafe.coerce(elem)##.style;
        switch (style) {
        | "width" => elem_style##.width := Js.string("")
        | "right" => elem_style##.right := Js.string("")
        | _ => ()
        };
      | None => ()
      }
    },
    elements,
  );
};

let resize_handle = (): Node.t => {
  let isResizing = ref(false);

  let rec handle_mousemove = event => {
    if (isResizing^) {
      let current_x = Js.Unsafe.coerce(event)##.clientX;
      let window_width = Dom_html.window##.innerWidth;
      let persistent_width = 38.9;
      let new_width =
        max(400, window_width - current_x - int_of_float(persistent_width));
      updateElementStyles(new_width);
    };
    ();
  }
  and handle_mouseup = _ => {
    isResizing := false;
    let doc = Js.Unsafe.coerce(Dom_html.document);
    let _ = doc##removeEventListener("mousemove", handle_mousemove);
    let _ = doc##removeEventListener("mouseup", handle_mouseup);
    ();
  };

  let handle_mousedown = _ => {
    isResizing := true;
    let doc = Js.Unsafe.coerce(Dom_html.document);
    let _ = doc##addEventListener("mousemove", handle_mousemove);
    let _ = doc##addEventListener("mouseup", handle_mouseup);
    Effect.Ignore;
  };

  div(
    ~attrs=[clss(["resize-handle"]), Attr.on_mousedown(handle_mousedown)],
    [],
  );
};

let view =
    (
      ~globals: Globals.t,
      ~explain_this_inject,
      ~explainThisModel: ExplainThisModel.t,
      ~editors_inject,
      ~editors: Editors.Model.t,
      ~signal,
      info: option(Language.Info.t),
    ) => {
  let sub =
    globals.settings.sidebar.show
      ? div(
          ~attrs=[Attr.id("side-bar"), Attr.tabindex(1)],
          [
            resize_handle(),
            switch (globals.settings.sidebar.panel) {
            | LanguageDocumentation =>
              ExplainThis.view(
                ~globals,
                ~inject=explain_this_inject,
                ~explainThisModel,
                info,
              )
            | HelpfulAssistant =>
              AgentView.view(~globals, ~editors_inject, ~editors, ~signal)
            },
          ],
        )
      : {
        resetElementStyles();
        div([]);
      };
  div(~attrs=[Attr.id("sidebars")], [sub, persistent_view(~globals)]);
};
