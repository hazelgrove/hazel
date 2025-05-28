module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Js_of_ocaml;

let tab = (~tooltip="", icon, action, isActive) => {
  let classes = ["tab"] @ (isActive ? ["active"] : []);
  div(
    ~attrs=[clss(classes), Attr.on_mousedown(action), Attr.title(tooltip)],
    [icon],
  );
};

let explain_this_tab = (~globals: Globals.t): Node.t => {
  let tooltip = "Switch to Language Documentation";
  let switch_explain_this = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(
        Set(Sidebar(SwitchPanel(LanguageDocumentation))),
      ),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["explain-this-button"])],
    [
      tab(
        Icons.explain_this,
        ~tooltip,
        switch_explain_this,
        globals.settings.sidebar.panel == LanguageDocumentation
        && globals.settings.sidebar.show,
      ),
    ],
  );
};

let assistant_tab = (~globals: Globals.t): Node.t => {
  let tooltip = "Switch to Helpful Assistant";
  let switch_assistant = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Sidebar(SwitchPanel(HelpfulAssistant)))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["assistant-button"])],
    [
      tab(
        Icons.hazelnut_agent,
        ~tooltip,
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
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Sidebar(ToggleShow))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["collapse-button"])],
    [tab(icon, ~tooltip, switch_assistant, false)],
  );
};

let persistent_view = (~globals: Globals.t) => {
  div(
    ~attrs=[Attr.id("persistent")],
    [
      div(
        ~attrs=[clss(["tabs"])],
        [
          explain_this_tab(~globals),
          assistant_tab(~globals),
          collapse_tab(~globals),
        ],
      ),
    ],
  );
};

let resize_handle = (): Node.t => {
  let isResizing = ref(false);

  let rec handle_mousemove = event => {
    if (isResizing^) {
      let current_x = Js.Unsafe.coerce(event)##.clientX;
      let window_width = Dom_html.window##.innerWidth;
      let persistent_width = 38.9; /* or 39 if you want to round */
      let new_width =
        max(400, window_width - current_x - int_of_float(persistent_width));
      let sidebar =
        Js.Unsafe.coerce(Dom_html.document)##getElementById("side-bar");
      let prompt_display_container =
        Js.Unsafe.coerce(Dom_html.document)##getElementById(
          "prompt-display-container",
        );
      let history_menu =
        Js.Unsafe.coerce(Dom_html.document)##getElementById("history-menu");
      if (Js.Opt.test(sidebar)) {
        sidebar##.style##.width :=
          Js.string(string_of_int(new_width) ++ "px");
      };
      if (Js.Opt.test(prompt_display_container)) {
        prompt_display_container##.style##.right :=
          Js.string(string_of_int(new_width + 20) ++ "px");
      };
      if (Js.Opt.test(history_menu)) {
        history_menu##.style##.right :=
          Js.string(string_of_int(new_width + 20) ++ "px");
      };
    };
    ();
  }
  and handle_mouseup = _ => {
    isResizing := false;
    let _ =
      Js.Unsafe.coerce(Dom_html.document)##removeEventListener(
        "mousemove",
        handle_mousemove,
      );
    let _ =
      Js.Unsafe.coerce(Dom_html.document)##removeEventListener(
        "mouseup",
        handle_mouseup,
      );
    ();
  };

  let handle_mousedown = _ => {
    isResizing := true;
    let _ =
      Js.Unsafe.coerce(Dom_html.document)##addEventListener(
        "mousemove",
        handle_mousemove,
      );
    let _ =
      Js.Unsafe.coerce(Dom_html.document)##addEventListener(
        "mouseup",
        handle_mouseup,
      );
    Virtual_dom.Vdom.Effect.Ignore;
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
      ~assistant_inject,
      ~signal,
      ~explainThisModel: ExplainThisModel.t,
      ~assistantModel: AssistantModel.t,
      info: option(Info.t),
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
              )
            },
          ],
        )
      : {
        div([]);
      };
  div(~attrs=[Attr.id("sidebars")], [sub, persistent_view(~globals)]);
};
