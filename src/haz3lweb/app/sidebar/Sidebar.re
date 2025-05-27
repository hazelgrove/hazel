open Virtual_dom.Vdom;
open Node;
open Util.Web;

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
