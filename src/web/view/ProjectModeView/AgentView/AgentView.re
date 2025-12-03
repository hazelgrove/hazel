open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

let view = (~globals: Globals.t): Node.t => {
  let agent_globals = globals.settings.agent_globals;
  switch (agent_globals.active_screen) {
  | AgentGlobals.Model.MainMenu => AgentMainMenuView.view(~globals)
  | AgentGlobals.Model.AgentChatInterface =>
    // TODO: Implement chat interface view
    let switch_to_main_menu = _ => {
      let switch_interface_action =
        AgentGlobals.Update.SwitchInterface(AgentGlobals.Model.MainMenu)
        |> (action => Settings.Update.AgentGlobals(action));
      Effect.Many([
        globals.inject_global(Globals.Action.Set(switch_interface_action)),
        Effect.Stop_propagation,
      ]);
    };
    div(
      ~attrs=[clss(["chat-interface-container"])],
      [
        div(
          ~attrs=[clss(["chat-interface-placeholder"])],
          [text("Chat Interface (Coming Soon)")],
        ),
        div(
          ~attrs=[clss(["back-to-menu-button-container"])],
          [
            div(
              ~attrs=[
                clss(["back-to-menu-button"]),
                Attr.on_click(switch_to_main_menu),
              ],
              [text("Back to Settings")],
            ),
          ],
        ),
      ],
    );
  };
};
