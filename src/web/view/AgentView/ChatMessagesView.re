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
        ~globals: Globals.t,
        ~code_with_statics: CodeWithStatics.Model.t,
        ~agent_view: AgentContext.Model.t,
        ~eval_result: EvalResult.Model.t,
        ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
        ~chat_id: Id.t,
      )
      : Node.t => {
    let segment =
      CompositionView.Public.segment_for_agent_context(
        code_with_statics.editor,
        agent_view,
      );
    let static_error_lines =
      ErrorPrint.all(
        CompositionGo.Public.mk_statics(
          code_with_statics.editor.state.zipper,
        ),
      );
    let static_section =
      div(
        ~attrs=[clss(["agent-context-section"])],
        [
          div(
            ~attrs=[clss(["agent-context-section-title"])],
            [text("Static diagnostics")],
          ),
          if (static_error_lines == []) {
            div(
              ~attrs=[clss(["agent-context-static-ok"])],
              [text("No static errors.")],
            );
          } else {
            div(
              ~attrs=[clss(["agent-context-static-list"])],
              List.map(
                (line: string) =>
                  div(
                    ~attrs=[clss(["agent-context-static-line"])],
                    [text(line)],
                  ),
                static_error_lines,
              ),
            );
          },
        ],
      );
    let test_section = {
      let test_results_opt = EvalResult.Model.test_results(eval_result);
      div(
        ~attrs=[clss(["agent-context-section"])],
        [
          div(
            ~attrs=[clss(["agent-context-section-title"])],
            [text("Tests")],
          ),
          switch (test_results_opt) {
          | None =>
            div(
              ~attrs=[clss(["agent-context-muted"])],
              [
                text(
                  "Test results are not available yet (evaluation may still be running).",
                ),
              ],
            )
          | Some(results) when results.total == 0 =>
            div(
              ~attrs=[clss(["agent-context-muted"])],
              [text("No tests in this program.")],
            )
          | Some(results) =>
            let summary =
              Language.TestResults.test_summary_str(results) |> String.trim;
            let rows =
              List.mapi(
                (i, status: Language.TestStatus.t) => {
                  let status_cls =
                    switch (status) {
                    | Pass => "agent-context-test-pass"
                    | Fail => "agent-context-test-fail"
                    | Indet => "agent-context-test-indet"
                    };
                  let icon =
                    switch (status) {
                    | Pass => Icons.confirm
                    | Fail => Icons.cancel
                    | Indet => Icons.circle_with_no_check
                    };
                  div(
                    ~attrs=[clss(["agent-context-test-row", status_cls])],
                    [
                      div(
                        ~attrs=[clss(["agent-context-test-icon"])],
                        [icon],
                      ),
                      div(
                        ~attrs=[clss(["agent-context-test-label"])],
                        [
                          text(
                            "Test "
                            ++ string_of_int(i + 1)
                            ++ ": "
                            ++ Language.TestStatus.to_string(status),
                          ),
                        ],
                      ),
                    ],
                  );
                },
                results.statuses,
              );
            div(
              ~attrs=[clss(["agent-context-tests"])],
              [
                div(
                  ~attrs=[clss(["agent-context-test-summary"])],
                  [text(summary)],
                ),
                div(~attrs=[clss(["agent-context-test-rows"])], rows),
              ],
            );
          },
        ],
      );
    };
    let program_section =
      if (segment == []) {
        div(
          ~attrs=[clss(["agent-context-code-scroll"])],
          [
            div(
              ~attrs=[clss(["agent-context-empty"])],
              [text("No program in this scratchpad.")],
            ),
          ],
        );
      } else {
        div(
          ~attrs=[clss(["agent-context-code-scroll"])],
          [
            div(
              ~attrs=[
                clss(["agent-context-code", "tool-call-diff-segment"]),
              ],
              [
                ToolResultView.render_segment(
                  ~globals,
                  ~shallow_complete=false,
                  segment,
                ),
              ],
            ),
          ],
        );
      };
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
          ~attrs=[clss(["view-content", "agent-context-body"])],
          [
            div(
              ~attrs=[clss(["agent-context-section"])],
              [
                div(
                  ~attrs=[clss(["agent-context-section-title"])],
                  [text("Program View")],
                ),
                program_section,
              ],
            ),
            static_section,
            test_section,
          ],
        ),
      ],
    );
  };

  let tools_view =
      (
        ~agent_model: Agent.Agent.Model.t,
        ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
        ~chat_id: Id.t,
      )
      : Node.t => {
    let tools = agent_model.prompting.tools;
    let disabled = agent_model.prompting.disabled_tool_names;
    let expanded = agent_model.tools_view_expanded;

    let tool_items =
      tools
      |> List.filter_map((tool: API.Json.t) => {
           switch (Agent.Agent.ToolUtils.get_name(tool)) {
           | Some(name) =>
             let category = Agent.Agent.ToolUtils.category_of_tool(name);
             let description =
               Agent.Agent.ToolUtils.get_description(tool)
               |> Option.value(~default="No description.");
             let is_enabled = !List.mem(name, disabled);
             let is_expanded = List.mem(name, expanded);
             Some((name, category, description, is_enabled, is_expanded));
           | None => None
           }
         });

    let grouped =
      tool_items
      |> List.sort((a, b) => {
           let (_, cat_a, _, _, _) = a;
           let (_, cat_b, _, _, _) = b;
           String.compare(cat_a, cat_b);
         })
      |> List.fold_left(
           (acc, (name, cat, desc, enabled, exp)) => {
             switch (List.assoc_opt(cat, acc)) {
             | Some(items) =>
               List.remove_assoc(cat, acc)
               @ [(cat, [(name, desc, enabled, exp), ...items])]
             | None => [(cat, [(name, desc, enabled, exp)])] @ acc
             }
           },
           [],
         )
      |> List.rev
      |> List.map(((cat, items)) => (cat, List.rev(items)));

    let render_tool =
        (name: string, desc: string, is_enabled: bool, is_expanded: bool) => {
      let toggle_tool = _ =>
        Effect.Many([
          agent_inject(
            Agent.Agent.Update.Action.SetToolEnabled(name, !is_enabled),
          ),
          Effect.Stop_propagation,
        ]);
      let toggle_expand = _ =>
        Effect.Many([
          agent_inject(
            Agent.Agent.Update.Action.ToggleToolsViewExpanded(name),
          ),
          Effect.Stop_propagation,
        ]);
      div(
        ~attrs=[clss(["tools-view-item"])],
        [
          div(
            ~attrs=[
              clss(["tools-view-item-header"]),
              Attr.on_click(toggle_expand),
            ],
            [
              div(
                ~attrs=[clss(["tools-view-item-expand-icon"])],
                [text(is_expanded ? "▾" : "▸")],
              ),
              div(
                ~attrs=[
                  clss(["tools-view-item-toggle"]),
                  Attr.on_click(toggle_tool),
                ],
                [
                  is_enabled
                    ? Icons.circle_with_check : Icons.circle_with_no_check,
                ],
              ),
              div(~attrs=[clss(["tools-view-item-name"])], [text(name)]),
            ],
          ),
          if (is_expanded) {
            div(
              ~attrs=[clss(["tools-view-item-detail"])],
              [
                div(~attrs=[clss(["tools-view-item-desc"])], [text(desc)]),
              ],
            );
          } else {
            div(~attrs=[], []);
          },
        ],
      );
    };

    let render_category =
        (category: string, items: list((string, string, bool, bool))) => {
      div(
        ~attrs=[clss(["tools-view-category"])],
        [
          div(
            ~attrs=[clss(["tools-view-category-title"])],
            [text(category)],
          ),
          div(
            ~attrs=[clss(["tools-view-category-items"])],
            List.map(
              ((name, desc, enabled, exp)) =>
                render_tool(name, desc, enabled, exp),
              List.rev(items),
            ),
          ),
        ],
      );
    };

    div(
      ~attrs=[clss(["full-screen-view", "tools-view"])],
      [
        div(
          ~attrs=[clss(["view-header"])],
          [
            div(~attrs=[clss(["view-title"])], [text("Agent Tools")]),
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
          ~attrs=[clss(["view-content", "tools-view-content"])],
          List.map(((cat, items)) => render_category(cat, items), grouped),
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
  cursor_id: option(Id.t),
  label: string,
  index: int,
};

let view =
    (
      ~globals: Globals.t,
      ~agent_model: Agent.Agent.Model.t,
      ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
      ~signal: Editors.View.signal => Effect.t(unit),
      ~code_with_statics: CodeWithStatics.Model.t,
      ~eval_result: EvalResult.Model.t,
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
             | Agent.Message.Model.System(Agent.Message.Model.RetryNote) =>
               Some(
                 div(
                   ~attrs=[
                     clss(["agent-system-message", "agent-retry-note"]),
                   ],
                   [text(msg.content)],
                 ),
               )
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

      /* Shared with the chat-wide replay feature; includes selector-based
         edit tools (selector_update, selector_delete, selector_insert_*). */
      let is_edit_tool_call = (tool_result: AgentToolResult.tool_result): bool =>
        Agent.Replay.Utils.is_edit_tool(tool_result.tool_call.name);

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

      let summary_dom_id = "tool-summary-" ++ string_of_int(index);

      let toggle_summary_collapsed = _ => {
        Js_of_ocaml.(
          Js.Opt.iter(
            Dom_html.document##getElementById(Js.string(summary_dom_id)),
            el => {
              let cl = el##.classList;
              if (Js.to_bool(cl##contains(Js.string("collapsed")))) {
                cl##remove(Js.string("collapsed"));
              } else {
                cl##add(Js.string("collapsed"));
              };
            },
          )
        );
        Effect.Stop_propagation;
      };

      let scroll_to_tool_call = (tool_call_id: string, msg_id: Id.t) => {
        let dom_id = "tool-call-" ++ tool_call_id;
        JsUtil.delay(50.0, () =>
          Js_of_ocaml.(
            Js.Opt.iter(
              Dom_html.document##getElementById(Js.string(dom_id)),
              el => {
                let opts =
                  Js.Unsafe.obj([|
                    ("behavior", Js.Unsafe.inject(Js.string("smooth"))),
                    ("block", Js.Unsafe.inject(Js.string("center"))),
                  |]);
                ignore(
                  Js.Unsafe.meth_call(
                    el,
                    "scrollIntoView",
                    [|Js.Unsafe.inject(opts)|],
                  ),
                );
              },
            )
          )
        );
        Effect.Many([
          agent_inject(
            Agent.Agent.Update.Action.ChatSystemAction(
              Agent.ChatSystem.Update.Action.ChatAction(
                Agent.Chat.Update.Action.MessageAction(
                  msg_id,
                  Agent.Message.Update.SetToolResultExpanded(true),
                ),
                current_chat_id,
              ),
            ),
          ),
          Effect.Stop_propagation,
        ]);
      };

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
          let all_edits = [first, ...rest];

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
                    Agent.Agent.Update.Action.LoadTimelineSegment(
                      segment,
                      node.cursor_id,
                      node.index,
                    ),
                  ),
                  Effect.Stop_propagation,
                ])
              };
            div(
              ~attrs=[
                clss([
                  "timeline-node",
                  "timeline-step",
                  is_disabled ? "disabled" : "",
                  is_active ? "active" : "",
                ]),
                Attr.on_click(on_click),
                Attr.title(
                  is_disabled
                    ? node.label ++ " (no segment data)" : node.label,
                ),
              ],
              [
                span(
                  ~attrs=[clss(["timeline-node-label"])],
                  [text(node.label)],
                ),
              ],
            );
          };

          let render_summary_tool_link =
              (tool_result: AgentToolResult.tool_result) => {
            let status_class =
              tool_result.success ? "tool-call-success" : "tool-call-failure";
            let msg_id_opt =
              List.find_opt(
                ((_, msg_tr): (Id.t, AgentToolResult.tool_result)) =>
                  msg_tr.tool_call.id == tool_result.tool_call.id,
                tool_result_messages,
              );
            let on_click = _ =>
              switch (msg_id_opt) {
              | Some((msg_id, _)) =>
                scroll_to_tool_call(tool_result.tool_call.id, msg_id)
              | None => Effect.Stop_propagation
              };
            div(
              ~attrs=[
                clss(["summary-tool-link"]),
                Attr.on_click(on_click),
                Attr.title(
                  "Jump to " ++ tool_result.tool_call.name ++ " in chat",
                ),
              ],
              [
                div(
                  ~attrs=[clss(["tool-call-status-icon", status_class])],
                  [tool_result.success ? Icons.confirm : Icons.cancel],
                ),
                span(
                  ~attrs=[clss(["summary-tool-link-name"])],
                  [text(tool_result.tool_call.name)],
                ),
                span(
                  ~attrs=[clss(["summary-tool-link-arrow"])],
                  [text({|↗|})],
                ),
              ],
            );
          };

          let initial_node: timeline_node = {
            segment: first.before_segment,
            cursor_id: first.before_cursor_id,
            label: "Initial",
            index: 0,
          };

          let interleaved_elements = {
            let initial = [render_node(initial_node)];
            let (elements, _) =
              List.fold_left(
                ((acc, node_idx), tool_result: AgentToolResult.tool_result) => {
                  let tool_link = render_summary_tool_link(tool_result);
                  if (tool_result.success) {
                    let next_node: timeline_node = {
                      segment: tool_result.after_segment,
                      cursor_id: tool_result.after_cursor_id,
                      label: "After Edit " ++ string_of_int(node_idx),
                      index: node_idx,
                    };
                    let node_view = render_node(next_node);
                    (acc @ [tool_link, node_view], node_idx + 1);
                  } else {
                    (acc @ [tool_link], node_idx);
                  };
                },
                ([], 1),
                all_edits,
              );
            initial @ elements;
          };

          let restore_button =
            switch (agent_model.restore_editor_state) {
            | Some(_) =>
              div(
                ~attrs=[
                  clss(["agent-restore-original-button"]),
                  Attr.on_click(_ =>
                    Effect.Many([
                      agent_inject(Agent.Agent.Update.Action.RestoreOriginal),
                      Effect.Stop_propagation,
                    ])
                  ),
                  Attr.title(
                    "Restore your code to the state before viewing history",
                  ),
                ],
                [Icons.undo, text("Restore Original")],
              )
            | None => div(~attrs=[], [])
            };

          div(
            ~attrs=[
              clss(["agent-tool-summary", "collapsed"]),
              Attr.id(summary_dom_id),
            ],
            [
              div(
                ~attrs=[
                  clss(["agent-tool-summary-header"]),
                  Attr.on_click(toggle_summary_collapsed),
                ],
                [
                  div(
                    ~attrs=[clss(["agent-tool-summary-title"])],
                    [
                      span(
                        ~attrs=[clss(["summary-collapse-icon"])],
                        [text({|▾|})],
                      ),
                      text("Edits Performed"),
                    ],
                  ),
                  restore_button,
                ],
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
          // Filbert identifier
          div(
            ~attrs=[clss(["message-identifier", "llm-identifier"])],
            [Icons.filbert, text("Filbert")],
          ),
          div(
            ~attrs=[clss(["agent-message-wrapper"])],
            linear_display @ [edit_calls_summary],
          ),
        ],
      );
    | Agent.ChunkedUIChat.Model.CompactionNotice({method, content}) =>
      div(
        ~attrs=[clss(["message-container", "compaction-notice-container"])],
        [
          div(
            ~attrs=[
              clss(["message-identifier", "compaction-notice-identifier"]),
            ],
            [text("Conversation compacted")],
          ),
          div(
            ~attrs=[clss(["compaction-notice-method"])],
            [text("Method: " ++ method)],
          ),
          div(~attrs=[clss(["compaction-notice-body"])], [text(content)]),
        ],
      )
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
        // Replay controls: step through agent edits via undo/redo
        ReplayView.view(~globals, ~agent_model, ~agent_inject, ~current_chat),
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
          )
          @ (
            switch (agent_model.compaction_in_progress) {
            | Some(id) when id == current_chat_id => [
                div(
                  ~attrs=[clss(["compaction-in-progress-banner"])],
                  [text("Compacting conversation…")],
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
  | Agent.Chat.Model.Tools =>
    ViewComponents.tools_view(
      ~agent_model,
      ~agent_inject,
      ~chat_id=current_chat_id,
    )
  | Agent.Chat.Model.AgentEditorView
  | Agent.Chat.Model.StaticErrors =>
    // Both AgentEditorView and StaticErrors now show context
    ViewComponents.context_view(
      ~globals,
      ~code_with_statics,
      ~agent_view=current_chat.agent_view,
      ~eval_result,
      ~agent_inject,
      ~chat_id=current_chat_id,
    )
  | Agent.Chat.Model.Workbench =>
    ViewComponents.workbench_view(~agent_inject, ~chat_id=current_chat_id)
  };
};
