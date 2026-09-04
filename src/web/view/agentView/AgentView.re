open Virtual_dom.Vdom;
open Node;
open Util_web.WebUtil;

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
    let placeholder =
      div(
        ~attrs=[clss(["chat-interface-placeholder"])],
        [text("Agent is available in Scratch and Documentation modes.")],
      );
    switch (editors) {
    | Scratch(m)
    | Documentation(m) =>
      let scratchpad = List.nth(m.scratchpads, m.current);
      switch (scratchpad.kind) {
      | ScratchMode.Scratchpad.Code({editor, agent}) =>
        let agent_inject = (action: Agent.Update.Action.t) =>
          editors_inject(
            Editors.Update.Scratch(ScratchMode.Update.AgentAction(action)),
          );
        ChatView.view(
          ~globals,
          ~agent_model=agent,
          ~agent_inject,
          ~signal,
          ~code_with_statics=editor.editor,
          ~eval_result=editor.result,
        );
      | ScratchMode.Scratchpad.Drv(_) => placeholder
      };
    | Tutorial(_)
    | Exercises(_) => placeholder
    };
  };
};
