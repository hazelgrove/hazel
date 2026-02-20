open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Haz3lcore;
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

type timeline_node = {
  segment: option(Segment.t),
  label: string,
  index: int,
};

let view =
    (
      ~globals: Globals.t,
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
                              Editors.Selection.Assistant,
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
             | Agent(_) =>
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
             | ToolResult(tool_result) =>
               // Tool call message - display inline with expand/collapse
               let toggle_expanded = _ => {
                 Effect.Many([
                   agent_inject(
                     Agent.Agent.Update.Action.ChatSystemAction(
                       Agent.ChatSystem.Update.Action.ChatAction(
                         Agent.Chat.Update.Action.MessageAction(
                           msg.id,
                           Agent.Message.Update.SetToolResultExpanded(
                             !tool_result.expanded,
                           ),
                         ),
                         current_chat_id,
                       ),
                     ),
                   ),
                   Effect.Stop_propagation,
                 ]);
               };
               Some(
                 ToolResultView.view(
                   ~globals,
                   ~tool_result,
                   ~toggle_expanded,
                 ),
               );
             | _ => None
             }
           });

      let linear_display = linear_messages_display;

      let is_edit_tool_call =
          (tool_result: OpenRouter.Reply.Model.tool_result): bool => {
        switch (tool_result.tool_call.name) {
        | "initialize"
        | "update_definition"
        | "update_body"
        | "update_pattern"
        | "update_binding_clause"
        | "delete_binding_clause"
        | "delete_body"
        | "insert_before"
        | "insert_after" => true
        | _ => false
        };
      };

      let edit_tool_results =
        agent_chunk.tool_results |> List.filter(is_edit_tool_call);

      // Extract message IDs and tool results from content for toggle wiring
      let tool_result_messages =
        agent_chunk.content
        |> List.filter_map((msg: Agent.Message.Model.t) =>
             switch (msg.role) {
             | ToolResult(tool_result) => Some((msg.id, tool_result))
             | _ => None
             }
           );

      let edit_calls_summary =
        switch (edit_tool_results) {
        | [] =>
          div(
            ~attrs=[
              clss(["agent-tool-summary", "agent-tool-summary-empty"]),
            ],
            [
              div(
                ~attrs=[clss(["agent-tool-summary-header"])],
                [text("Edits Performed")],
              ),
              div(
                ~attrs=[clss(["agent-tool-summary-empty-text"])],
                [text("No edit tool calls were made in this response.")],
              ),
            ],
          )
        | [first, ...rest] =>
          // Build interleaved list: node, tool result, node, tool result, ...
          let all_edits = [first, ...rest];

          // Helper to render a timeline node
          let render_node = (node: timeline_node) => {
            let is_disabled =
              switch (node.segment) {
              | None => true
              | Some(_) => false
              };
            let is_active =
              switch (agent_model.active_timeline_node) {
              | Some(active_index) => node.index == active_index
              | None => false
              };
            let on_click = _ =>
              switch (node.segment) {
              | None => Effect.Stop_propagation
              | Some(segment) =>
                Effect.Many([
                  agent_inject(
                    Agent.Agent.Update.Action.LoadSegmentIntoEditor(segment),
                  ),
                  agent_inject(
                    Agent.Agent.Update.Action.SetActiveTimelineNode(
                      Some(node.index),
                    ),
                  ),
                  Effect.Stop_propagation,
                ])
              };
            div(
              ~attrs=[
                clss([
                  "timeline-node",
                  is_disabled ? "disabled" : "",
                  is_active ? "active" : "",
                ]),
                Attr.on_click(on_click),
                Attr.title(
                  is_disabled
                    ? node.label ++ " (no segment data)" : node.label,
                ),
              ],
              [],
            );
          };

          // Helper to render a tool result
          let render_tool_result =
              (tool_result: OpenRouter.Reply.Model.tool_result) => {
            switch (
              List.find_opt(
                (
                  (_, msg_tool_result): (
                    Id.t,
                    OpenRouter.Reply.Model.tool_result,
                  ),
                ) =>
                  msg_tool_result.tool_call.id == tool_result.tool_call.id,
                tool_result_messages,
              )
            ) {
            | Some((msg_id, msg_tool_result)) =>
              let toggle_expanded = _ =>
                Effect.Many([
                  agent_inject(
                    Agent.Agent.Update.Action.ChatSystemAction(
                      Agent.ChatSystem.Update.Action.ChatAction(
                        Agent.Chat.Update.Action.MessageAction(
                          msg_id,
                          Agent.Message.Update.SetToolResultExpanded(
                            !msg_tool_result.expanded,
                          ),
                        ),
                        current_chat_id,
                      ),
                    ),
                  ),
                  Effect.Stop_propagation,
                ]);
              Some(
                ToolResultView.view(
                  ~globals,
                  ~tool_result=msg_tool_result,
                  ~toggle_expanded,
                ),
              );
            | None => None
            };
          };

          // Build timeline states
          let initial_node: timeline_node = {
            segment: first.before_segment,
            label: "Initial",
            index: 0,
          };

          // Build interleaved elements: initial node, then for each edit: tool result + node
          let interleaved_elements = {
            let initial = [render_node(initial_node)];
            let rest_elements =
              List.mapi(
                (i, tool_result: OpenRouter.Reply.Model.tool_result) => {
                  let tool_result_view = render_tool_result(tool_result);
                  let next_node: timeline_node = {
                    segment: tool_result.after_segment,
                    label: "After Edit " ++ string_of_int(i + 1),
                    index: i + 1,
                  };
                  let node_view = render_node(next_node);

                  // Return list of [tool_result, node]
                  switch (tool_result_view) {
                  | Some(view) => [view, node_view]
                  | None => [node_view]
                  };
                },
                all_edits,
              )
              |> List.flatten;

            initial @ rest_elements;
          };

          div(
            ~attrs=[clss(["agent-tool-summary"])],
            [
              div(
                ~attrs=[clss(["agent-tool-summary-header"])],
                [text("Edits Performed")],
              ),
              div(
                ~attrs=[clss(["agent-tool-summary-content"])],
                interleaved_elements,
              ),
            ],
          );
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
            linear_display @ [edit_calls_summary],
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
          List.mapi(render_chunk, chunked_chat.log)
          @ (
            switch (agent_model.awaiting_response) {
            | Some(awaiting_id) when awaiting_id == current_chat_id => [
                div(
                  ~attrs=[
                    clss(["message-container", "agent-message-container"]),
                  ],
                  [
                    div(
                      ~attrs=[
                        clss(["message-identifier", "llm-identifier"]),
                      ],
                      [text("Agent")],
                    ),
                    div(
                      ~attrs=[clss(["agent-message-loading-dots"])],
                      [
                        span(~attrs=[clss(["dot", "dot1"])], []),
                        span(~attrs=[clss(["dot", "dot2"])], []),
                        span(~attrs=[clss(["dot"])], []),
                      ],
                    ),
                  ],
                ),
              ]
            | _ => []
            }
          ),
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
