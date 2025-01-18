open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Haz3lcore;

let tab = (~tooltip="", icon, action) =>
  div(
    ~attrs=[clss(["tab"]), Attr.on_mousedown(action), Attr.title(tooltip)],
    [icon],
  );

let explain_this_tab = (~globals: Globals.t): Node.t => {
  let tooltip = "Switch to Language Documentation";
  let switch_explain_this = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(
        Set(Sidebar(SwitchWindow(LanguageDocumentation))),
      ),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["explain-this-button"])],
    [tab(Icons.explain_this, ~tooltip, switch_explain_this)],
  );
};

let assistant_tab = (~globals: Globals.t): Node.t => {
  let tooltip = "Switch to Helpful Assistant";
  let switch_assistant = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Sidebar(SwitchWindow(HelpfulAssistant)))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["assistant-button"])],
    [tab(Icons.assistant, ~tooltip, switch_assistant)],
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
    [tab(icon, ~tooltip, switch_assistant)],
  );
};

let persistent_view = (~globals: Globals.t, ~inject: 'a => Effect.t(unit)) => {
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

let view = (~globals: Globals.t, ~inject: 'a => Effect.t(unit)) => {
  div(~attrs=[Attr.id("persistent-sidebar")], []);
};
