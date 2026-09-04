open Virtual_dom.Vdom;
open Node;
open Util_web.WebUtil;
open Js_of_ocaml;
open Util_web.JsUtil;

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
    /* Keep editor focus when switching sidebar sections. */
    Effect.Prevent_default,
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
    ~tooltip="Open Hazel Coding Agent",
    ~globals,
  );

let probes_tab = (~globals: Globals.t): Node.t =>
  tab_of(
    ~panel=Probes,
    ~cls=["probes-button"],
    ~icon=Icons.microscope2,
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

let debug_info_tab = (~globals: Globals.t): Node.t =>
  tab_of(
    ~panel=DebugInfo,
    ~cls=["debug-info-button"],
    ~icon=Icons.info,
    ~tooltip="Switch to Debug Info Panel",
    ~globals,
  );

let problems_tab_icon =
    (counts: list((SidebarModel.Settings.problem_category, int))): Node.t => {
  open SidebarModel.Settings;
  let digit_cls = n =>
    n >= 100 ? "digits-3" : n >= 10 ? "digits-2" : "digits-1";
  /* Aggregate counts by badge group: sum counts sharing the same badge_cls */
  let grouped =
    List.fold_left(
      (acc, (cat, n)) => {
        let cls = category_badge_cls(cat);
        let sev = category_badge_severity(cat);
        let label = category_badge_label(cat);
        switch (List.assoc_opt(cls, acc)) {
        | Some((total, s, _)) => [
            (cls, (total + n, max(s, sev), label)),
            ...List.remove_assoc(cls, acc),
          ]
        | None => [(cls, (n, sev, label)), ...acc]
        };
      },
      [],
      counts,
    );
  let sorted =
    List.sort(
      ((_, (_, s1, _)), (_, (_, s2, _))) => compare(s2, s1),
      grouped,
    );
  let (status_class, icon_text, title) =
    switch (List.find_opt(((_, (n, _, _))) => n > 0, sorted)) {
    | Some((cls, (n, _, label))) =>
      let plural = n > 1 ? label ++ "s" : label;
      (
        cls ++ " " ++ digit_cls(n),
        string_of_int(n),
        string_of_int(n) ++ " " ++ plural,
      );
    | None => ("no-errors", {|✓|}, "No errors")
    };
  div(
    ~attrs=[
      clss(["tab-status-indicator", status_class]),
      Attr.title(title),
    ],
    [span(~attrs=[], [text(icon_text)])],
  );
};

let problems_tab =
    (
      ~globals: Globals.t,
      ~counts: list((SidebarModel.Settings.problem_category, int)),
    )
    : Node.t =>
  tab_of(
    ~panel=Problems,
    ~cls=["problems-button"],
    ~icon=problems_tab_icon(counts),
    ~tooltip="Switch to Problems Panel",
    ~globals,
  );

let persistent_view =
    (
      ~globals: Globals.t,
      ~counts: list((SidebarModel.Settings.problem_category, int)),
    ) =>
  div(
    ~attrs=[Attr.id("persistent")],
    [
      div(
        ~attrs=[clss(["tabs"])],
        [
          explain_this_tab(~globals),
          assistant_tab(~globals),
          probes_tab(~globals),
          problems_tab(~globals, ~counts),
        ]
        @ (
          globals.settings.show_log_panel ? [log_control_tab(~globals)] : []
        )
        @ (
          globals.settings.show_debug_panel ? [debug_info_tab(~globals)] : []
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
      ~explainThisModel: ExplainThisModel.t,
      ~log_model: LogSidebar.Model.t,
      ~log_count: int,
      ~editors_inject,
      ~editors: Editors.Model.t,
      ~selection: Editors.Selection.t,
      ~editor: CodeWithStatics.Model.t,
      ~problem_editors:
         list((option(string), list(CodeWithStatics.Model.t))),
      ~signal,
    ) => {
  let problem_collection =
    Haz3lcore.ProblemCollection.make(
      ~display_warnings=globals.settings.core.display_warnings,
      List.map(
        ((label, editors: list(CodeWithStatics.Model.t))) =>
          Haz3lcore.ProblemCollection.{
            label,
            sources:
              List.map(
                (e: CodeWithStatics.Model.t) =>
                  Haz3lcore.ProblemCollection.{
                    statics: e.statics,
                    syntax: e.editor.syntax,
                  },
                editors,
              ),
          },
        problem_editors,
      ),
    );
  let counts = problem_collection.counts;
  /* See Page.calculate: use the live selection so Prelude/Setup focus
     doesn't show up as "in a derivation" via the stale model.pos. */
  let derivation_info =
    Editors.Selection.get_derivation_info(~selection, editors);
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
                {
                  cursor: cursor.info,
                  deduction: derivation_info,
                },
              )
            | HelpfulAssistant =>
              AgentView.view(~globals, ~editors_inject, ~editors, ~signal)
            | Probes =>
              ProbeSidebar.view(
                ~globals,
                ~explain_this_inject,
                ~cursor,
                ~editor,
              )
            | LogControl =>
              LogSidebar.view(
                ~globals,
                ~model=log_model,
                ~log_entries_count=log_count,
              )
            | Problems =>
              ProblemSidebar.view(
                ~globals,
                ~cursor,
                ~collection=problem_collection,
              )
            | DebugInfo => DebugSidebar.view(~globals, ~cursor)
            },
          ],
        )
      : {
        resetElementStyles();
        div([]);
      };
  div(
    ~attrs=[Attr.id("sidebars")],
    [sub, persistent_view(~globals, ~counts)],
  );
};
