open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Icons;

let view =
    (
      ~globals: Globals.t,
      ~agent_model: Agent.Agent.Model.t,
      ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
      ~signal: Editors.View.signal => Effect.t(unit),
    )
    : Node.t => {
  let chat_system = agent_model.chat_system;

  // Settings button - switch back to main menu
  let switch_to_settings = _ => {
    let switch_interface_action =
      AgentGlobals.Update.SwitchInterface(AgentGlobals.Model.MainMenu)
      |> (action => Settings.Update.AgentGlobals(action));
    Effect.Many([
      globals.inject_global(Globals.Action.Set(switch_interface_action)),
      Effect.Stop_propagation,
    ]);
  };

  // New chat button
  let new_chat = _ => {
    let system_prompt = agent_model.prompting.system_prompt;
    let dev_notes = agent_model.prompting.dev_notes;
    Effect.Many([
      agent_inject(
        Agent.Agent.Update.Action.ChatSystemAction(
          Agent.ChatSystem.Update.Action.NewChat(system_prompt, dev_notes),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  // History button - switch to history screen
  let toggle_history = (screen: Agent.ChatSystem.Model.active_screen) => {
    Effect.Many([
      agent_inject(
        Agent.Agent.Update.Action.ChatSystemAction(
          Agent.ChatSystem.Update.Action.SwitchScreen(screen),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  // Exit button - switch back to chat screen
  let switch_to_chat = _ => {
    Effect.Many([
      agent_inject(
        Agent.Agent.Update.Action.ChatSystemAction(
          Agent.ChatSystem.Update.Action.SwitchScreen(
            Agent.ChatSystem.Model.Chat,
          ),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  // Get ordered list of chats (earliest at bottom, so reverse the list)
  let chats_list =
    List.rev(Agent.ChatSystem.Utils.chats_to_list(chat_system));

  // View selection handlers (Chat vs Workbench)
  let switch_to_chat_messages = _ => {
    Effect.Many([
      agent_inject(
        Agent.Agent.Update.Action.ChatSystemAction(
          Agent.ChatSystem.Update.Action.SwitchView(
            Agent.ChatSystem.Model.ChatMessages,
          ),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  let switch_to_workbench = _ => {
    Effect.Many([
      agent_inject(
        Agent.Agent.Update.Action.ChatSystemAction(
          Agent.ChatSystem.Update.Action.SwitchView(
            Agent.ChatSystem.Model.Workbench,
          ),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  div(
    ~attrs=[clss(["chat-view-container"])],
    [
      // Header
      div(
        ~attrs=[clss(["chat-view-header"])],
        [
          div(
            ~attrs=[clss(["chat-view-header-left"])],
            [
              // View selection buttons (only show when in Chat screen)
              if (chat_system.ui.active_screen == Agent.ChatSystem.Model.Chat) {
                div(
                  ~attrs=[clss(["chat-view-buttons"])],
                  [
                    div(
                      ~attrs=[
                        clss(
                          ["chat-view-button"]
                          @ (
                            chat_system.ui.active_view
                            == Agent.ChatSystem.Model.ChatMessages
                              ? ["active"] : []
                          ),
                        ),
                        Attr.on_click(switch_to_chat_messages),
                      ],
                      [text("Chat")],
                    ),
                    div(
                      ~attrs=[
                        clss(
                          ["chat-view-button"]
                          @ (
                            chat_system.ui.active_view
                            == Agent.ChatSystem.Model.Workbench
                              ? ["active"] : []
                          ),
                        ),
                        Attr.on_click(switch_to_workbench),
                      ],
                      [text("Workbench")],
                    ),
                  ],
                );
              } else {
                div([]);
              },
            ],
          ),
          div(
            ~attrs=[clss(["chat-view-header-right"])],
            [
              div(
                ~attrs=[
                  clss(["chat-view-header-icon-button"]),
                  Attr.on_click(new_chat),
                  Attr.title("New Chat"),
                ],
                [add],
              ),
              div(
                ~attrs=[
                  clss(
                    ["chat-view-header-icon-button"]
                    @ (
                      chat_system.ui.active_screen
                      == Agent.ChatSystem.Model.History
                        ? ["active"] : []
                    ),
                  ),
                  Attr.on_click(_ =>
                    toggle_history(
                      switch (chat_system.ui.active_screen) {
                      | Agent.ChatSystem.Model.History => Agent.ChatSystem.Model.Chat
                      | Agent.ChatSystem.Model.Chat => Agent.ChatSystem.Model.History
                      },
                    )
                  ),
                  Attr.title("History"),
                ],
                [history],
              ),
              div(
                ~attrs=[
                  clss(["chat-view-header-button"]),
                  Attr.on_click(switch_to_settings),
                ],
                [text("Settings")],
              ),
            ],
          ),
        ],
      ),
      // Content area - switch between Chat and History
      switch (chat_system.ui.active_screen) {
      | Agent.ChatSystem.Model.Chat =>
        // Within Chat screen, switch between ChatMessages and Workbench
        switch (chat_system.ui.active_view) {
        | Agent.ChatSystem.Model.ChatMessages =>
          ChatMessagesView.view(
            ~globals,
            ~agent_model,
            ~agent_inject,
            ~signal,
          )
        | Agent.ChatSystem.Model.Workbench =>
          div(
            ~attrs=[clss(["chat-view-content"])],
            [text("Workbench View (Coming Soon)")],
          )
        }
      | Agent.ChatSystem.Model.History =>
        div(
          ~attrs=[clss(["chat-view-content", "history-view"])],
          [
            // Exit button
            div(
              ~attrs=[clss(["history-exit-button-container"])],
              [
                div(
                  ~attrs=[
                    clss(["history-exit-button"]),
                    Attr.on_click(switch_to_chat),
                  ],
                  [text("Return to Chat")],
                ),
              ],
            ),
            // Chat list
            div(
              ~attrs=[clss(["history-chat-list"])],
              List.map(
                (chat: Agent.Chat.Model.t) => {
                  let is_active = chat.id == chat_system.current;
                  let classes =
                    ["history-chat-item"] @ (is_active ? ["active"] : []);
                  let switch_to_this_chat = _ => {
                    Effect.Many([
                      agent_inject(
                        Agent.Agent.Update.Action.ChatSystemAction(
                          Agent.ChatSystem.Update.Action.SwitchChat(chat.id),
                        ),
                      ),
                      // Also switch back to Chat screen to view the selected chat
                      agent_inject(
                        Agent.Agent.Update.Action.ChatSystemAction(
                          Agent.ChatSystem.Update.Action.SwitchScreen(
                            Agent.ChatSystem.Model.Chat,
                          ),
                        ),
                      ),
                      Effect.Stop_propagation,
                    ]);
                  };
                  let time_ago = TimeUtil.format_time_diff(chat.created_at);
                  div(
                    ~attrs=[
                      clss(classes),
                      Attr.on_click(switch_to_this_chat),
                    ],
                    [
                      div(
                        ~attrs=[clss(["history-chat-item-title"])],
                        [text(chat.title)],
                      ),
                      div(
                        ~attrs=[clss(["history-chat-item-time"])],
                        [text(time_ago)],
                      ),
                    ],
                  );
                },
                chats_list,
              ),
            ),
          ],
        )
      },
    ],
  );
};
