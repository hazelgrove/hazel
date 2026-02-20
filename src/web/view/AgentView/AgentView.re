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
    switch (editors) {
    | Scratch(m)
    | Documentation(m) =>
      let scratchpad = List.nth(m.scratchpads, m.current);
      let agent_model = scratchpad.agent;
      let agent_inject = (action: Agent.Agent.Update.Action.t) =>
        editors_inject(
          Editors.Update.Scratch(ScratchMode.Update.AgentAction(action)),
        );
      ChatView.view(~globals, ~agent_model, ~agent_inject, ~signal);
    | Tutorial(_)
    | Exercises(_) =>
      div(
        ~attrs=[clss(["chat-interface-placeholder"])],
        [text("Agent is available in Scratch and Documentation modes.")],
      )
    }
  };
};
