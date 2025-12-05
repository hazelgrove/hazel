open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Js_of_ocaml;
open Icons;

let _ = confirm; // Temporary. Silencing warnings from unused Icon open.

let view =
    (
      ~globals as _: Globals.t,
      ~agent_model: Agent.Agent.Model.t,
      ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
      ~signal: Editors.View.signal => Effect.t(unit),
    )
    : Node.t => {
  let chat_system = agent_model.chat_system;
  let current_chat_id = chat_system.current;
  let current_chat =
    Agent.ChatSystem.Utils.find_chat(current_chat_id, chat_system);
  let messages = Agent.Chat.Utils.get(current_chat);

  // Auto-resize textarea helper
  let autosize_textarea = (id: string) => {
    Js.Opt.iter(
      Dom_html.document##getElementById(Js.string(id)),
      el => {
        let textarea = Js.Unsafe.coerce(el);
        textarea##.style##.height := Js.string("auto");
        let scroll_height = textarea##.scrollHeight;
        let max_height = 200; // max height in pixels
        let height = min(scroll_height, max_height);
        textarea##.style##.height := Js.string(string_of_int(height) ++ "px");
        textarea##.style##.overflowY :=
          Js.string(scroll_height > max_height ? "auto" : "hidden");
      },
    );
  };

  // Get current text box content from model
  let current_text = chat_system.ui.current_text_box_content;

  // Handle textarea input - Attr.on_input provides (event, string)
  let handle_textarea_input = (_event, value: string) => {
    JsUtil.delay(0.0, () => autosize_textarea("chat-message-input"));
    Effect.Many([
      agent_inject(
        Agent.Agent.Update.Action.ChatSystemAction(
          Agent.ChatSystem.Update.Action.SaveTextBoxContent(value),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  // Send message handler
  let send_message = _ => {
    let message_content = String.trim(current_text);
    if (String.length(message_content) > 0) {
      // Create user message
      let user_message = Agent.Message.Utils.mk_user_message(message_content);
      // Send the message and clear the text box
      Effect.Many([
        agent_inject(
          Agent.Agent.Update.Action.SendMessage(
            user_message,
            current_chat_id,
          ),
        ),
        agent_inject(
          Agent.Agent.Update.Action.ChatSystemAction(
            Agent.ChatSystem.Update.Action.SaveTextBoxContent(""),
          ),
        ),
        Effect.Stop_propagation,
      ]);
    } else {
      Effect.Stop_propagation;
    };
  };

  // Send handler for editable user messages
  let send_edited_message = (message_id: Id.t, unique_id: string) => {
    let updated_content =
      Js.Opt.case(
        Dom_html.document##getElementById(Js.string(unique_id)),
        () => "",
        el => {
          let textarea = Js.Unsafe.coerce(el);
          Js.to_string(textarea##.value);
        },
      );
    let trimmed_content = String.trim(updated_content);
    if (String.length(trimmed_content) > 0) {
      // Create a new user message with updated content
      let updated_message =
        Agent.Message.Utils.mk_user_message(trimmed_content);
      // Send the message (appends and triggers LLM response)
      Effect.Many([
        agent_inject(
          Agent.Agent.Update.Action.ChatSystemAction(
            Agent.ChatSystem.Update.Action.ChatAction(
              Agent.Chat.Update.Action.BranchOff(
                Agent.Chat.Utils.parent_of(message_id, current_chat).id,
              ),
              current_chat_id,
            ),
          ),
        ),
        agent_inject(
          Agent.Agent.Update.Action.SendMessage(
            updated_message,
            current_chat_id,
          ),
        ),
        Effect.Stop_propagation,
      ]);
    } else {
      Effect.Stop_propagation;
    };
  };

  // Helper function to render branch navigation buttons
  let render_branch_navigation = (message: Agent.Message.Model.t) => {
    // Find the parent of this message
    let parent_opt =
      try(Some(Agent.Chat.Utils.parent_of(message.id, current_chat))) {
      | _ => None
      };
    switch (parent_opt) {
    | Some(parent) =>
      let num_children = List.length(parent.children);
      if (num_children > 1) {
        // Find current message's index among parent's children
        let rec find_index = (idx: int, children: list(Id.t)): int => {
          switch (children) {
          | [] => 0
          | [hd, ...tl] =>
            if (hd == message.id) {
              idx;
            } else {
              find_index(idx + 1, tl);
            }
          };
        };
        let current_index = find_index(0, parent.children);
        // Calculate next and previous indices using modulo
        let next_index = (current_index + 1) mod num_children;
        let prev_index = (current_index - 1 + num_children) mod num_children;
        // Get sibling IDs
        let next_sibling_id = List.nth(parent.children, next_index);
        let prev_sibling_id = List.nth(parent.children, prev_index);
        Some(
          div(
            ~attrs=[clss(["branch-navigation"])],
            [
              div(
                ~attrs=[
                  clss(["branch-nav-button", "branch-nav-left"]),
                  Attr.on_click(_ => {
                    Effect.Many([
                      agent_inject(
                        Agent.Agent.Update.Action.ChatSystemAction(
                          Agent.ChatSystem.Update.Action.ChatAction(
                            Agent.Chat.Update.Action.SwitchBranch(
                              parent.id,
                              prev_sibling_id,
                            ),
                            current_chat_id,
                          ),
                        ),
                      ),
                      Effect.Stop_propagation,
                    ])
                  }),
                  Attr.title("Previous branch"),
                ],
                [Icons.back],
              ),
              div(
                ~attrs=[
                  clss(["branch-nav-button", "branch-nav-right"]),
                  Attr.on_click(_ => {
                    Effect.Many([
                      agent_inject(
                        Agent.Agent.Update.Action.ChatSystemAction(
                          Agent.ChatSystem.Update.Action.ChatAction(
                            Agent.Chat.Update.Action.SwitchBranch(
                              parent.id,
                              next_sibling_id,
                            ),
                            current_chat_id,
                          ),
                        ),
                      ),
                      Effect.Stop_propagation,
                    ])
                  }),
                  Attr.title("Next branch"),
                ],
                [Icons.forward],
              ),
            ],
          ),
        );
      } else {
        None;
      };
    | None => None
    };
  };

  // Render a message based on its role
  let num_messages = List.length(messages);
  let render_message = (index: int, message: Agent.Message.Model.t) => {
    let branch_nav = render_branch_navigation(message);
    let is_last_message = index == num_messages - 1;
    switch (message.role) {
    | Agent.Message.Model.User =>
      // User messages on the right, editable
      let unique_id = "user-message-input-" ++ string_of_int(index);
      div(
        ~attrs=[clss(["message-container", "user-message-container"])],
        [
          div(
            ~attrs=[clss(["user-message-wrapper"])],
            [
              div(
                ~attrs=[clss(["user-message-input-container"])],
                [
                  textarea(
                    ~attrs=[
                      clss(["user-message-input"]),
                      Attr.id(unique_id),
                      Attr.value(message.content),
                      Attr.property("autocomplete", Js.Unsafe.inject("off")),
                      Attr.on_focus(_ => {
                        // Lock height on focus to prevent resizing - maintain current size
                        Js.Opt.iter(
                          Dom_html.document##getElementById(
                            Js.string(unique_id),
                          ),
                          el => {
                            let textarea = Js.Unsafe.coerce(el);
                            // Get the current computed height and lock it
                            let current_height = textarea##.offsetHeight;
                            textarea##.style##.height :=
                              Js.string(
                                string_of_int(current_height) ++ "px",
                              );
                            textarea##.style##.overflowY := Js.string("auto");
                          },
                        );
                        Effect.Many([
                          signal(
                            Editors.View.MakeActive(
                              Editors.Selection.Projects(
                                ProjectMode.Selection.TextBox,
                              ),
                            ),
                          ),
                          Effect.Stop_propagation,
                        ]);
                      }),
                      Attr.on_blur(_ => {
                        // Reset height on blur to allow natural sizing when not focused
                        Js.Opt.iter(
                          Dom_html.document##getElementById(
                            Js.string(unique_id),
                          ),
                          el => {
                            let textarea = Js.Unsafe.coerce(el);
                            textarea##.style##.height := Js.string("auto");
                          },
                        );
                        Effect.Stop_propagation;
                      }),
                      Attr.on_input((_event, _value) => {
                        Effect.Stop_propagation
                               // Don't resize while typing - keep current height
                      }),
                      Attr.on_copy(_ => Effect.Stop_propagation),
                      Attr.on_paste(_ => {
                        JsUtil.delay(0.0, () => autosize_textarea(unique_id));
                        Effect.Stop_propagation;
                      }),
                      Attr.on_cut(_ => Effect.Stop_propagation),
                    ],
                    [text(message.content)],
                  ),
                  div(
                    ~attrs=[
                      clss([
                        "send-button",
                        "icon",
                        "user-message-send-button",
                      ]),
                      Attr.on_mousedown(_event => {
                        // Prevent textarea from losing focus when clicking button
                        Js.Opt.iter(
                          Dom_html.document##getElementById(
                            Js.string(unique_id),
                          ),
                          el => {
                            let textarea = Js.Unsafe.coerce(el);
                            textarea##focus;
                          },
                        );
                        Effect.Many([
                          send_edited_message(message.id, unique_id),
                          Effect.Prevent_default,
                        ]);
                      }),
                      Attr.title("Send Message"),
                    ],
                    [Icons.send],
                  ),
                ],
              ),
              switch (branch_nav) {
              | Some(nav) => nav
              | None => div(~attrs=[], [])
              },
            ],
          ),
        ],
      );
    | Agent.Message.Model.Agent =>
      // Agent messages on the left
      let message_content =
        if (message.content == "" && is_last_message) {
          // Show loading dots animation only for the last message when content is empty
          div(
            ~attrs=[clss(["agent-message-loading-dots"])],
            [
              div(~attrs=[clss(["dot", "dot1"])], []),
              div(~attrs=[clss(["dot", "dot2"])], []),
              div(~attrs=[clss(["dot"])], []),
            ],
          );
        } else {
          div(~attrs=[clss(["agent-message"])], [text(message.content)]);
        };
      div(
        ~attrs=[clss(["message-container", "agent-message-container"])],
        [
          div(
            ~attrs=[clss(["agent-message-wrapper"])],
            [
              message_content,
              switch (branch_nav) {
              | Some(nav) => nav
              | None => div(~attrs=[], [])
              },
            ],
          ),
        ],
      );
    | Agent.Message.Model.System(system_kind) =>
      // System messages centered, greyed out (or red if error)
      let is_error =
        switch (system_kind) {
        | Agent.Message.Model.Error(_) => true
        | _ => false
        };
      div(
        ~attrs=[
          clss(
            ["message-container", "system-message-container"]
            @ (is_error ? ["error"] : []),
          ),
        ],
        [
          div(~attrs=[clss(["system-message"])], [text(message.content)]),
          switch (branch_nav) {
          | Some(nav) => nav
          | None => div(~attrs=[], [])
          },
        ],
      );
    };
  };

  div(
    ~attrs=[clss(["chat-messages-view"])],
    [
      // Messages display area
      div(
        ~attrs=[clss(["chat-messages-container"])],
        List.mapi(render_message, messages),
      ),
      // Input area at bottom
      div(
        ~attrs=[clss(["chat-input-container"])],
        [
          div(
            ~attrs=[clss(["chat-message-input-container"])],
            [
              textarea(
                ~attrs=[
                  clss(["chat-message-input"]),
                  Attr.id("chat-message-input"),
                  Attr.placeholder("Type your message..."),
                  Attr.property("autocomplete", Js.Unsafe.inject("off")),
                  Attr.on_focus(_ => {
                    // Lock height on focus to prevent resizing while typing
                    Js.Opt.iter(
                      Dom_html.document##getElementById(
                        Js.string("chat-message-input"),
                      ),
                      el => {
                        let textarea = Js.Unsafe.coerce(el);
                        let current_height = textarea##.offsetHeight;
                        textarea##.style##.height :=
                          Js.string(string_of_int(current_height) ++ "px");
                        textarea##.style##.overflowY := Js.string("auto");
                      },
                    );
                    Effect.Many([
                      signal(
                        Editors.View.MakeActive(
                          Editors.Selection.Projects(
                            ProjectMode.Selection.TextBox,
                          ),
                        ),
                      ),
                      Effect.Stop_propagation,
                    ]);
                  }),
                  Attr.on_blur(_ => {
                    // Resize on blur to fit content
                    JsUtil.delay(0.0, () =>
                      autosize_textarea("chat-message-input")
                    );
                    Effect.Stop_propagation;
                  }),
                  Attr.on_input(handle_textarea_input),
                  Attr.on_copy(_ => Effect.Stop_propagation),
                  Attr.on_paste(_ => {
                    // Resize after paste
                    JsUtil.delay(0.0, () =>
                      autosize_textarea("chat-message-input")
                    );
                    Effect.Stop_propagation;
                  }),
                  Attr.on_cut(_ => Effect.Stop_propagation),
                  Attr.string_property("value", current_text),
                ],
                [text(current_text)],
              ),
              if (String.length(String.trim(current_text)) > 0) {
                div(
                  ~attrs=[
                    clss(["send-button", "icon", "chat-message-send-button"]),
                    Attr.on_click(send_message),
                    Attr.title("Send Message"),
                  ],
                  [Icons.send],
                );
              } else {
                div(
                  ~attrs=[
                    clss([
                      "send-button-disabled",
                      "icon",
                      "chat-message-send-button",
                    ]),
                    Attr.title("Send Message Disabled"),
                  ],
                  [Icons.send],
                );
              },
            ],
          ),
        ],
      ),
    ],
  );
};
