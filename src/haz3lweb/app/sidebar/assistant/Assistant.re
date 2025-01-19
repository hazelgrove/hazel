open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Haz3lcore;

let llm_toggle = (~globals: Globals.t): Node.t => {
  let tooltip = "Toggle Manual LLM";
  let toggle_llm = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Assistant(ToggleLLM))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["llm-button"])],
    [
      text("Manual LLM: "),
      Widgets.toggle(
        ~tooltip,
        "🕵️‍♀️",
        globals.settings.assistant.llm,
        toggle_llm,
      ),
    ],
  );
};

let lsp_toggle = (~globals: Globals.t): Node.t => {
  let tooltip = "Toggle Manual LSP";
  let toggle_lsp = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Assistant(ToggleLSP))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["lsp-button"])],
    [
      text("Manual LSP: "),
      Widgets.toggle(
        ~tooltip,
        "🧑‍🔧",
        globals.settings.assistant.lsp,
        toggle_lsp,
      ),
    ],
  );
};

let begin_chat_button = (~globals: Globals.t): Node.t => {
  let tooltip = "Begin Chat";
  let begin_chat = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Assistant(UpdateChatStatus))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["chat-button"]), Attr.on_click(begin_chat)],
    [Widgets.button_named(~tooltip, None, begin_chat)],
  );
};

let end_chat_button = (~globals: Globals.t): Node.t => {
  let tooltip = "End Chat";
  let end_chat = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Assistant(UpdateChatStatus))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["chat-button"]), Attr.on_click(end_chat)],
    [Widgets.button_named(~tooltip, None, end_chat)],
  );
};

let settings_box = (~globals: Globals.t): Node.t => {
  div(
    ~attrs=[clss(["settings-box"])],
    [
      llm_toggle(~globals),
      lsp_toggle(~globals),
      begin_chat_button(~globals),
    ],
  );
};

let view = (~globals: Globals.t, ~inject: 'a => Effect.t(unit)) => {
  div(
    ~attrs=[Attr.id("side-bar")],
    [
      div(
        ~attrs=[Attr.id("assistant")],
        [
          div(
            ~attrs=[clss(["header"])],
            [
              text("Agentic Assistant Chat"),
              globals.settings.assistant.ongoing_chat
                ? end_chat_button(~globals) : None,
            ],
          ),
          globals.settings.assistant.ongoing_chat
            ? None : settings_box(~globals),
        ],
      ),
    ],
  );
};
