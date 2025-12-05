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
    // TODO: Get agent model from project or store
    // For now, initialize a default agent model
    let project_mode =
      switch (editors) {
      | Projects(project_mode) => Some(project_mode)
      | _ => None
      };
    switch (project_mode) {
    | None =>
      div(
        ~attrs=[clss(["chat-interface-placeholder"])],
        [text("Agent is currently only supported in project mode.")],
      )
    | Some(project_mode) =>
      let agent_model = ProjectMode.Utils.current_project(project_mode).agent;
      let agent_inject = (action: Agent.Agent.Update.Action.t) =>
        editors_inject(
          Editors.Update.Projects(
            ProjectMode.Update.Project(
              Project.Update.AgentAction(action, None),
              Some(project_mode.current),
            ),
          ),
        );
      ChatView.view(~globals, ~agent_model, ~agent_inject, ~signal);
    };
  };
};
