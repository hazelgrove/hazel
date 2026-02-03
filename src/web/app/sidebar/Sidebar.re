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

let switch_to = (~globals: Globals.t, panel: SidebarModel.Settings.panel, _) =>
  Effect.Many([
    globals.inject_global(Set(Sidebar(SwitchPanel(panel)))),
    Effect.Stop_propagation,
  ]);

let switch_assistant = (~globals: Globals.t, _) =>
  Effect.Many([
    globals.inject_global(Set(Sidebar(ToggleShow))),
    Effect.Stop_propagation,
  ]);

let tab_of =
    (
      ~panel: SidebarModel.Settings.panel,
      ~cls: list(string),
      ~icon: Node.t,
      ~tooltip: string,
      ~globals: Globals.t,
    )
    : Node.t =>
  div(
    ~attrs=[clss(cls)],
    [
      tab(
        icon,
        ~tooltip,
        switch_to(~globals, panel),
        globals.settings.sidebar.panel == panel
        && globals.settings.sidebar.show,
      ),
    ],
  );

let explain_this_tab = (~globals: Globals.t): Node.t =>
  tab_of(
    ~panel=LanguageDocumentation,
    ~cls=["explain-this-button"],
    ~icon=Icons.explain_this,
    ~tooltip="Switch to Language Documentation",
    ~globals,
  );

let assistant_tab = (~globals: Globals.t): Node.t =>
  tab_of(
    ~panel=HelpfulAssistant,
    ~cls=["assistant-button"],
    ~icon=Icons.hazelnut_agent,
    ~tooltip="Switch to Helpful Assistant",
    ~globals,
  );

let probes_tab = (~globals: Globals.t): Node.t =>
  tab_of(
    ~panel=Probes,
    ~cls=["probes-button"],
    ~icon=Icons.shutter,
    ~tooltip="Switch to Probes Panel",
    ~globals,
  );

let log_control_tab = (~globals: Globals.t): Node.t =>
  tab_of(
    ~panel=LogControl,
    ~cls=["log-control-button"],
    ~icon=Icons.gear,
    ~tooltip="Switch to Log Control Panel",
    ~globals,
  );

let collapse_tab = (~globals: Globals.t): Node.t => {
  let tooltip =
    globals.settings.sidebar.show ? "Collapse Sidebar" : "Expand Sidebar";
  let icon = globals.settings.sidebar.show ? Icons.collapse : Icons.expand;
  div(
    ~attrs=[clss(["collapse-button"])],
    [tab(icon, ~tooltip, switch_assistant(~globals), false)],
  );
};

let persistent_view = (~globals: Globals.t) =>
  div(
    ~attrs=[Attr.id("persistent")],
    [
      div(
        ~attrs=[clss(["tabs"])],
        [
          explain_this_tab(~globals),
          assistant_tab(~globals),
          probes_tab(~globals),
        ]
        @ (
          globals.settings.show_log_panel ? [log_control_tab(~globals)] : []
        ),
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
      ~cursor: Cursor.cursor(Editors.Update.t),
      ~explain_this_inject,
      ~assistant_inject,
      ~signal,
      ~explainThisModel: ExplainThisModel.t,
      ~assistantModel: AssistantModel.t,
      ~log_model: LogSidebar.Model.t,
      ~log_count: int,
      ~editor,
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
              AssistantView.view(
                ~globals,
                ~signal,
                ~inject=assistant_inject,
                ~model=assistantModel,
                ~editor,
              )
            | Probes =>
              ProbeSidebar.view(
                ~globals,
                ~explain_this_inject,
                ~cursor,
                ~editor,
              )
            | LogControl =>
              LogSidebar.view(
                ~inject=globals.inject_global,
                ~inject_log=_ => Effect.Ignore, // For now, ignore LogSidebar internal updates
                ~model=log_model,
                ~log_entries_count=log_count,
              )
            },
          ],
        )
      : {
        resetElementStyles();
        div([]);
      };
  div(~attrs=[Attr.id("sidebars")], [sub, persistent_view(~globals)]);
};
