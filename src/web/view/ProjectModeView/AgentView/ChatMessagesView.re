open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Js_of_ocaml;

open JsUtil;

let _ = confirm; // Temporary. Silencing warnings from unused Icon open.

// View components for different views
module ViewComponents = {
  let prompt_view =
      (
        ~content: string,
        ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
        ~chat_id: Id.t,
      )
      : Node.t => {
    div(
      ~attrs=[clss(["full-screen-view"])],
      [
        div(
          ~attrs=[clss(["view-header"])],
          [
            div(~attrs=[clss(["view-title"])], [text("System Prompt")]),
            div(
              ~attrs=[
                clss(["view-close-button", "icon"]),
                Attr.on_click(_ =>
                  Effect.Many([
                    agent_inject(
                      Agent.Agent.Update.Action.ChatSystemAction(
                        Agent.ChatSystem.Update.Action.ChatAction(
                          Agent.Chat.Update.Action.SwitchView(
                            Agent.Chat.Model.Messages,
                          ),
                          chat_id,
                        ),
                      ),
                    ),
                    Effect.Stop_propagation,
                  ])
                ),
              ],
              [Icons.cancel],
            ),
          ],
        ),
        div(
          ~attrs=[clss(["view-content", "system-message"])],
          [text(content)],
        ),
      ],
    );
  };

  let developer_notes_view =
      (
        ~content: string,
        ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
        ~chat_id: Id.t,
      )
      : Node.t => {
    div(
      ~attrs=[clss(["full-screen-view"])],
      [
        div(
          ~attrs=[clss(["view-header"])],
          [
            div(~attrs=[clss(["view-title"])], [text("Developer Notes")]),
            div(
              ~attrs=[
                clss(["view-close-button", "icon"]),
                Attr.on_click(_ =>
                  Effect.Many([
                    agent_inject(
                      Agent.Agent.Update.Action.ChatSystemAction(
                        Agent.ChatSystem.Update.Action.ChatAction(
                          Agent.Chat.Update.Action.SwitchView(
                            Agent.Chat.Model.Messages,
                          ),
                          chat_id,
                        ),
                      ),
                    ),
                    Effect.Stop_propagation,
                  ])
                ),
              ],
              [Icons.cancel],
            ),
          ],
        ),
        div(
          ~attrs=[clss(["view-content", "system-message"])],
          [text(content)],
        ),
      ],
    );
  };

  let context_view =
      (
        ~content: string,
        ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
        ~chat_id: Id.t,
      )
      : Node.t => {
    div(
      ~attrs=[clss(["full-screen-view"])],
      [
        div(
          ~attrs=[clss(["view-header"])],
          [
            div(~attrs=[clss(["view-title"])], [text("Agent Context")]),
            div(
              ~attrs=[
                clss(["view-close-button", "icon"]),
                Attr.on_click(_ =>
                  Effect.Many([
                    agent_inject(
                      Agent.Agent.Update.Action.ChatSystemAction(
                        Agent.ChatSystem.Update.Action.ChatAction(
                          Agent.Chat.Update.Action.SwitchView(
                            Agent.Chat.Model.Messages,
                          ),
                          chat_id,
                        ),
                      ),
                    ),
                    Effect.Stop_propagation,
                  ])
                ),
              ],
              [Icons.cancel],
            ),
          ],
        ),
        div(
          ~attrs=[clss(["view-content", "system-message"])],
          [text(content)],
        ),
      ],
    );
  };

  let workbench_view =
      (
        ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
        ~chat_id: Id.t,
      )
      : Node.t => {
    div(
      ~attrs=[clss(["full-screen-view"])],
      [
        div(
          ~attrs=[clss(["view-header"])],
          [
            div(~attrs=[clss(["view-title"])], [text("Workbench")]),
            div(
              ~attrs=[
                clss(["view-close-button", "icon"]),
                Attr.on_click(_ =>
                  Effect.Many([
                    agent_inject(
                      Agent.Agent.Update.Action.ChatSystemAction(
                        Agent.ChatSystem.Update.Action.ChatAction(
                          Agent.Chat.Update.Action.SwitchView(
                            Agent.Chat.Model.Messages,
                          ),
                          chat_id,
                        ),
                      ),
                    ),
                    Effect.Stop_propagation,
                  ])
                ),
              ],
              [Icons.cancel],
            ),
          ],
        ),
        div(
          ~attrs=[clss(["view-content"])],
          [text("Workbench view - to be implemented")],
        ),
      ],
    );
  };
};

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
  let chunked_chat = Agent.ChunkedUIChat.Utils.mk(current_chat);

  // Auto-resize textarea helper
  let autosize_textarea = (id: string) => {
    Js.Opt.iter(
      Dom_html.document##getElementById(Js.string(id)),
      el => {
        let textarea = Js.Unsafe.coerce(el);
        textarea##.style##.height := Js.string("auto");
        let scroll_height = textarea##.scrollHeight;
        // Use max-height from CSS (400px for user messages, 200px for chat input)
        let max_height =
          if (String.starts_with(~prefix="user-message-input-", id)) {
            400;
          } else {
            200;
          };
        let height = min(scroll_height, max_height);
        textarea##.style##.height := Js.string(string_of_int(height) ++ "px");
        textarea##.style##.overflowY :=
          Js.string(scroll_height > max_height ? "auto" : "hidden");
      },
    );
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
  let render_branch_navigation = (message_id: Id.t): Node.t => {
    // Find parent message and check if it has multiple children
    let parent_msg_opt =
      try(Some(Agent.Chat.Utils.parent_of(message_id, current_chat))) {
      | _ => None
      };
    switch (parent_msg_opt) {
    | Some(parent_msg) =>
      let num_children = List.length(parent_msg.children);
      if (num_children > 1) {
        // Find current child index
        let current_child_opt = parent_msg.current_child;
        let current_index =
          switch (current_child_opt) {
          | Some(current_child_id) =>
            let rec find_index = (idx: int, children: list(Id.t)): int =>
              switch (children) {
              | [] => 0
              | [id, ..._] when id == current_child_id => idx
              | [_, ...rest] => find_index(idx + 1, rest)
              };
            find_index(0, parent_msg.children);
          | None => 0
          };
        let can_go_left = current_index > 0;
        let can_go_right = current_index < num_children - 1;
        let switch_to_prev = _ =>
          if (can_go_left) {
            let prev_child_id =
              List.nth(parent_msg.children, current_index - 1);
            Effect.Many([
              agent_inject(
                Agent.Agent.Update.Action.ChatSystemAction(
                  Agent.ChatSystem.Update.Action.ChatAction(
                    Agent.Chat.Update.Action.SwitchBranch(
                      parent_msg.id,
                      prev_child_id,
                    ),
                    current_chat_id,
                  ),
                ),
              ),
              Effect.Stop_propagation,
            ]);
          } else {
            Effect.Stop_propagation;
          };
        let switch_to_next = _ =>
          if (can_go_right) {
            let next_child_id =
              List.nth(parent_msg.children, current_index + 1);
            Effect.Many([
              agent_inject(
                Agent.Agent.Update.Action.ChatSystemAction(
                  Agent.ChatSystem.Update.Action.ChatAction(
                    Agent.Chat.Update.Action.SwitchBranch(
                      parent_msg.id,
                      next_child_id,
                    ),
                    current_chat_id,
                  ),
                ),
              ),
              Effect.Stop_propagation,
            ]);
          } else {
            Effect.Stop_propagation;
          };
        div(
          ~attrs=[clss(["branch-navigation"])],
          [
            div(
              ~attrs=[
                clss(["branch-nav-button", can_go_left ? "" : "disabled"]),
                Attr.on_click(switch_to_prev),
                Attr.title("Previous branch"),
              ],
              [Icons.back],
            ),
            div(
              ~attrs=[
                clss(["branch-nav-button", can_go_right ? "" : "disabled"]),
                Attr.on_click(switch_to_next),
                Attr.title("Next branch"),
              ],
              [Icons.forward],
            ),
          ],
        );
      } else {
        div(~attrs=[], []);
      };
    | None => div(~attrs=[], [])
    };
  };

  // Render a chunk from the chunked UI model
  let num_chunks = List.length(chunked_chat.log);

  // Auto-size all user message textareas after chunks are rendered
  // Use requestAnimationFrame to ensure DOM is ready
  ignore(
    Dom_html.window##requestAnimationFrame(
      Js.wrap_callback((_timestamp: float) => {
        JsUtil.delay(0.0, () => {
          for (i in 0 to num_chunks - 1) {
            let id = "user-message-input-" ++ string_of_int(i);
            autosize_textarea(id);
          }
        })
      }),
    ),
  );

  let render_chunk = (index: int, chunk: Agent.ChunkedUIChat.Model.chunk) => {
    switch (chunk) {
    | Agent.ChunkedUIChat.Model.UserMessage(user_msg) =>
      // User messages on the right, editable
      let unique_id = "user-message-input-" ++ string_of_int(index);
      // Auto-size on mount - run after element is inserted into DOM
      ignore(JsUtil.delay(0.0, () => autosize_textarea(unique_id)));
      div(
        ~attrs=[clss(["message-container", "user-message-container"])],
        [
          // User identifier
          div(
            ~attrs=[clss(["message-identifier", "user-identifier"])],
            [text("You")],
          ),
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
                      Attr.value(user_msg.content),
                      Attr.property("autocomplete", Js.Unsafe.inject("off")),
                      Attr.on_focus(_ => {
                        JsUtil.delay(0.0, () => autosize_textarea(unique_id));
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
                        JsUtil.delay(0.0, () => autosize_textarea(unique_id));
                        Effect.Stop_propagation;
                      }),
                      Attr.on_input((_event, _value) => {
                        JsUtil.delay(0.0, () => autosize_textarea(unique_id));
                        Effect.Stop_propagation;
                      }),
                      Attr.on_keydown(event => {
                        let key =
                          Js.Optdef.to_option(Js.Unsafe.get(event, "key"));
                        let shift_pressed = Key.shift_held(event);
                        switch (key) {
                        | Some("Enter") when !shift_pressed =>
                          // Enter without Shift: send message and blur
                          Js.Opt.iter(
                            Dom_html.document##getElementById(
                              Js.string(unique_id),
                            ),
                            el => {
                              let textarea = Js.Unsafe.coerce(el);
                              textarea##blur();
                            },
                          );
                          Effect.Many([
                            send_edited_message(
                              user_msg.origin_id,
                              unique_id,
                            ),
                            Effect.Prevent_default,
                            Effect.Stop_propagation,
                          ]);
                        | Some("Enter") =>
                          // Shift+Enter: allow default (newline)
                          Effect.Stop_propagation
                        | _ => Effect.Stop_propagation
                        };
                      }),
                      Attr.on_copy(_ => Effect.Stop_propagation),
                      Attr.on_paste(_ => {
                        JsUtil.delay(0.0, () => autosize_textarea(unique_id));
                        Effect.Stop_propagation;
                      }),
                      Attr.on_cut(_ => Effect.Stop_propagation),
                    ],
                    [text(user_msg.content)],
                  ),
                  div(
                    ~attrs=[
                      clss([
                        "send-button",
                        "icon",
                        "user-message-send-button",
                      ]),
                      Attr.on_mousedown(_event => {
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
                          send_edited_message(user_msg.origin_id, unique_id),
                          Effect.Prevent_default,
                        ]);
                      }),
                      Attr.title("Send Message"),
                    ],
                    [Icons.send],
                  ),
                ],
              ),
              // Branch navigation buttons below the input container
              render_branch_navigation(user_msg.origin_id),
            ],
          ),
        ],
      );
    | Agent.ChunkedUIChat.Model.AgentResponseChunk(agent_chunk) =>
      // Agent response chunk - display messages linearly
      // Render each message in the content list linearly
      // Filter out empty agent messages - don't display them at all
      let linear_messages_display =
        agent_chunk.content
        |> List.filter_map((msg: Agent.Message.Model.t) => {
             switch (msg.role) {
             | Agent =>
               // Only show agent message if it has content
               if (msg.content != "" && String.trim(msg.content) != "") {
                 Some(
                   div(
                     ~attrs=[clss(["agent-message"])],
                     [text(msg.content)],
                   ),
                 );
               } else {
                 None; // Don't display empty agent messages
               }
             | ToolResult(tool_call, success) =>
               // Tool call message - display inline
               let status_text = success ? "[success]" : "[failure]";
               Some(
                 div(
                   ~attrs=[clss(["agent-tool-call-inline"])],
                   [
                     div(
                       ~attrs=[clss(["tool-call-display"])],
                       [text(tool_call.name ++ " " ++ status_text)],
                     ),
                   ],
                 ),
               );
             | _ => None
             }
           });

      let linear_display = linear_messages_display;

      // Render all tool calls at the end - separated by double line breaks
      let tool_calls_summary_display =
        if (List.length(agent_chunk.tool_calls) > 0) {
          let tool_call_texts =
            agent_chunk.tool_calls
            |> List.map(
                 (nugget: Agent.ChunkedUIChat.Model.tool_call_info_nugget) => {
                 let status_text = nugget.success ? "[success]" : "[failure]";
                 nugget.tool_call.name ++ " " ++ status_text;
               });
          // Join with double line breaks
          let combined_tool_calls = String.concat("\n\n", tool_call_texts);
          div(
            ~attrs=[clss(["agent-tool-calls"])],
            [
              div(
                ~attrs=[clss(["tool-call-display"])],
                [text(combined_tool_calls)],
              ),
            ],
          );
        } else {
          div(~attrs=[], []);
        };

      div(
        ~attrs=[clss(["message-container", "agent-message-container"])],
        [
          // Corylus identifier
          div(
            ~attrs=[clss(["message-identifier", "llm-identifier"])],
            [Icons.corylus, text("Filbert")],
          ),
          div(
            ~attrs=[clss(["agent-message-wrapper"])],
            linear_display @ [tool_calls_summary_display],
          ),
        ],
      );
    | Agent.ChunkedUIChat.Model.ErrorMessage(error_content) =>
      // Error messages centered, red
      div(
        ~attrs=[
          clss(["message-container", "system-message-container", "error"]),
        ],
        [div(~attrs=[clss(["system-message"])], [text(error_content)])],
      )
    };
  };

  // Check current view and render appropriate view
  switch (current_chat.current_view) {
  | Agent.Chat.Model.Messages =>
    // Normal messages view (content only, bottom bar handled in ChatView)
    div(
      ~attrs=[clss(["chat-messages-view"])],
      [
        // Toast notification for copy
        div(
          ~attrs=[clss(["copy-toast"]), Attr.id("copy-toast")],
          [text("Copied!")],
        ),
        // Chunks display area
        div(
          ~attrs=[clss(["chat-messages-container"])],
          List.mapi(render_chunk, chunked_chat.log),
        ),
      ],
    )
  | Agent.Chat.Model.Prompt =>
    ViewComponents.prompt_view(
      ~content=chunked_chat.prompt,
      ~agent_inject,
      ~chat_id=current_chat_id,
    )
  | Agent.Chat.Model.DeveloperNotes =>
    ViewComponents.developer_notes_view(
      ~content=chunked_chat.developer_notes,
      ~agent_inject,
      ~chat_id=current_chat_id,
    )
  | Agent.Chat.Model.AgentEditorView
  | Agent.Chat.Model.StaticErrors =>
    // Both AgentEditorView and StaticErrors now show context
    ViewComponents.context_view(
      ~content=chunked_chat.context,
      ~agent_inject,
      ~chat_id=current_chat_id,
    )
  | Agent.Chat.Model.Workbench =>
    ViewComponents.workbench_view(~agent_inject, ~chat_id=current_chat_id)
  };
};
