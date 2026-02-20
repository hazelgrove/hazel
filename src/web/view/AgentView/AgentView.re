open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

let view =
    (
      ~globals: Globals.t,
      ~editors_inject,
      ~editors: Editors.Model.t,
      ~signal: Editors.View.signal => 'a,
    )
    : Node.t => {
  let agent_globals = globals.settings.agent_globals;
  switch (agent_globals.active_screen) {
  | AgentGlobals.Model.MainMenu => AgentMainMenuView.view(~globals, ~signal)
  | AgentGlobals.Model.AgentChatInterface =>
    div(
      ~attrs=[clss(["chat-interface-placeholder"])],
      [text("Agent coming soon to Scratch and other modes.")],
    )
  };
};
