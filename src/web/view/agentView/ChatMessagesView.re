open Virtual_dom.Vdom;
open Node;
open Util_web.WebUtil;
open Util;
open Haz3lcore;
open Js_of_ocaml;

open JsUtil;

// View components for different views
module ViewComponents = {
  let prompt_view =
      (
        ~content: string,
        ~agent_inject: Agent.Update.Action.t => Effect.t(unit),
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
                      Agent.Update.Action.ChatSystemAction(
                        ChatSystem.Update.Action.ChatAction(
                          Chat.Update.Action.SwitchView(Chat.Model.Messages),
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
          ~attrs=[clss(["view-content", "full-screen-doc"])],
          [text(content)],
        ),
      ],
    );
  };

  let developer_notes_view =
      (
        ~content: string,
        ~agent_inject: Agent.Update.Action.t => Effect.t(unit),
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
                      Agent.Update.Action.ChatSystemAction(
                        ChatSystem.Update.Action.ChatAction(
                          Chat.Update.Action.SwitchView(Chat.Model.Messages),
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
          ~attrs=[clss(["view-content", "full-screen-doc"])],
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
        ~current_chat: Chat.Model.t,
        ~agent_inject: Agent.Update.Action.t => Effect.t(unit),
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
              ~attrs=[clss(["view-header-actions"])],
              [
                div(
                  ~attrs=[
                    clss(["view-close-button", "icon"]),
                    Attr.title(
                      "Copy the exact context snapshot text sent to the model (XML-style blocks + footer).",
                    ),
                    Attr.on_click(_ => {
                      let snapshot =
                        Agent.Utils.llm_context_snapshot_text(
                          ~session_mode=
                            globals.settings.agent_globals.session_mode,
                          ~cell_result=eval_result,
                          code_with_statics,
                          current_chat,
                        );
                      copy_via_shim(snapshot);
                      show_copy_toast();
                      Effect.Stop_propagation;
                    }),
                  ],
                  [Icons.copy],
                ),
                div(
                  ~attrs=[
                    clss(["view-close-button", "icon"]),
                    Attr.on_click(_ =>
                      Effect.Many([
                        agent_inject(
                          Agent.Update.Action.ChatSystemAction(
                            ChatSystem.Update.Action.ChatAction(
                              Chat.Update.Action.SwitchView(
                                Chat.Model.Messages,
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
        ~agent_model: Agent.Model.t,
        ~agent_inject: Agent.Update.Action.t => Effect.t(unit),
        ~chat_id: Id.t,
      )
      : Node.t => {
    /* Use [[CompositionUtils.Public.tools]], not [[prompting.tools]]: the latter is
       persisted with the scratchpad and can lag behind the in-code registry. */
    let tools = CompositionUtils.Public.tools;
    let disabled = agent_model.prompting.disabled_tool_names;
    let expanded = agent_model.tools_view_expanded;

    let tool_items =
      tools
      |> List.filter_map((tool: API.Json.t) => {
           switch (Agent.ToolUtils.get_name(tool)) {
           | Some(name) =>
             let category = Agent.ToolUtils.category_of_tool(name);
             let description =
               Agent.ToolUtils.get_description(tool)
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
            Agent.Update.Action.SetToolEnabled(name, !is_enabled),
          ),
          Effect.Stop_propagation,
        ]);
      let toggle_expand = _ =>
        Effect.Many([
          agent_inject(Agent.Update.Action.ToggleToolsViewExpanded(name)),
          Effect.Stop_propagation,
        ]);
      div(
        ~attrs=[
          clss(
            ["tools-view-item"]
            @ (is_enabled ? [] : ["tools-view-item-disabled"])
            @ (is_expanded ? ["tools-view-item-expanded"] : []),
          ),
        ],
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
                  Attr.title(
                    is_enabled
                      ? "Disable tool for this chat"
                      : "Enable tool for this chat",
                  ),
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
      let all_enabled = List.for_all(((_, _, en, _)) => en, items);
      let toggle_category = _ =>
        Effect.Many([
          agent_inject(
            Agent.Update.Action.SetToolsInCategoryEnabled(
              category,
              !all_enabled,
            ),
          ),
          Effect.Stop_propagation,
        ]);
      div(
        ~attrs=[clss(["tools-view-category"])],
        [
          div(
            ~attrs=[clss(["tools-view-category-title-row"])],
            [
              div(
                ~attrs=[clss(["tools-view-category-title-label"])],
                [text(category)],
              ),
              div(
                ~attrs=[
                  clss(
                    [
                      "tools-view-item-toggle",
                      "tools-view-category-master-toggle",
                    ]
                    @ (all_enabled ? [] : ["tools-view-category-master-off"]),
                  ),
                  Attr.on_click(toggle_category),
                  Attr.title(
                    all_enabled
                      ? "Disable all tools in this category for this chat"
                      : "Enable all tools in this category for this chat",
                  ),
                ],
                [
                  all_enabled
                    ? Icons.circle_with_check : Icons.circle_with_no_check,
                ],
              ),
            ],
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
                      Agent.Update.Action.ChatSystemAction(
                        ChatSystem.Update.Action.ChatAction(
                          Chat.Update.Action.SwitchView(Chat.Model.Messages),
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
        ~agent_inject: Agent.Update.Action.t => Effect.t(unit),
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
                      Agent.Update.Action.ChatSystemAction(
                        ChatSystem.Update.Action.ChatAction(
                          Chat.Update.Action.SwitchView(Chat.Model.Messages),
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

/** Changes when the messages pane content or trailing banners change height. */
let chat_messages_scroll_stamp =
    (
      ~chunked_chat: ChunkedUIChat.Model.t,
      ~awaiting_dots: bool,
      ~compaction_banner: bool,
      ~pending_content: string,
      ~pending_reasoning: string,
    )
    : int => {
  let acc = ref(0);
  let mix = (n: int) => acc := Hashtbl.hash((acc^, n));
  List.iter(
    (chunk: ChunkedUIChat.Model.chunk) =>
      switch (chunk) {
      | UserMessage({content, origin_id}) =>
        mix(1);
        mix(String.length(content));
        mix(Hashtbl.hash(origin_id));
      | AgentResponseChunk({content, agent_reasoning, tool_results}) =>
        mix(2);
        List.iter(
          (m: Message.Model.t) => {
            mix(String.length(m.content));
            mix(Hashtbl.hash(m.id));
            switch (m.reasoning) {
            | Some(r) => mix(String.length(r))
            | None => mix(0)
            };
            switch (m.role) {
            | ToolResult(tr) => mix(tr.expanded ? 1 : 0)
            | Agent(_) => mix(3)
            | User => mix(4)
            | System(_) => mix(5)
            };
          },
          content,
        );
        List.iter(s => mix(String.length(s)), agent_reasoning);
        List.iter(
          (tr: AgentToolResult.tool_result) => {
            mix(String.length(tr.content));
            mix(tr.expanded ? 1 : 0);
            mix(tr.success ? 1 : 0);
            mix(Hashtbl.hash(tr.tool_call.id));
          },
          tool_results,
        );
      | CompactionNotice({method, content}) =>
        mix(6);
        mix(String.length(method));
        mix(String.length(content));
      | ErrorMessage(s) =>
        mix(7);
        mix(String.length(s));
      | ResponseCancelledMessage(s) =>
        mix(8);
        mix(String.length(s));
      | SlashCommandOutputMessage(payload) =>
        mix(9);
        switch (payload) {
        | CostOutput(p) =>
          mix(91);
          mix(p.cost_input_tokens);
          mix(p.cost_output_tokens);
          mix(String.length(p.cost_model));
        | CreditsOutput(p) =>
          mix(92);
          mix(int_of_float(p.credits_used *. 1000.));
          mix(int_of_float(p.credits_total *. 1000.));
        | UsageOutput(p) =>
          mix(93);
          mix(int_of_float(p.usage_total *. 1000.));
        | KeyOutput(k) =>
          mix(96);
          mix(String.length(k));
        | HelpOutput(p) =>
          mix(94);
          mix(List.length(p.help_entries));
        | Notice(s) =>
          mix(97);
          mix(String.length(s));
        | SlashError(s) =>
          mix(95);
          mix(String.length(s));
        };
      },
    chunked_chat.log,
  );
  mix(awaiting_dots ? 11 : 0);
  mix(compaction_banner ? 13 : 0);
  /* Keeps the stamp moving while streaming deltas accumulate into the
     in-progress bubble; otherwise the scroll hook sees a stable input
     and the user has to manually chase the stream. */
  mix(String.length(pending_content));
  mix(String.length(pending_reasoning));
  acc^;
};

/** Scroll the messages list to the bottom only while the user stays pinned to the bottom. */
module ChatMessagesScrollHook = {
  module State = {
    type t = {
      mutable stick_to_bottom: bool,
      mutable listener_id: option(Dom_html.event_listener_id),
    };
  };

  module Input = {
    [@deriving sexp_of]
    type t = int;

    let combine = (_, y) => y;
  };

  let bottom_slack_px = 12.0;

  let is_near_bottom = (el: Js.t(Dom_html.element)): bool => {
    let h = Js.Unsafe.coerce(el);
    let st = float_of_int(h##.scrollTop);
    let ch = float_of_int(h##.clientHeight);
    let sh = float_of_int(h##.scrollHeight);
    sh -. st -. ch <= bottom_slack_px;
  };

  let scroll_to_bottom = (el: Js.t(Dom_html.element)): unit => {
    let h = Js.Unsafe.coerce(el);
    h##.scrollTop :=  h##.scrollHeight;
  };

  /** After vdom patch, flex layout and user-message textarea autosize (rAF + [delay 0])
      can grow [scrollHeight] without changing the scroll stamp — schedule follow-up scrolls. */
  let schedule_scroll_to_bottom = (el: Js.t(Dom_html.element)): unit => {
    let run = () => scroll_to_bottom(el);
    ignore(
      Dom_html.window##requestAnimationFrame(
        Js.wrap_callback((_: float) => {
          run();
          JsUtil.delay(
            0.0,
            () => {
              run();
              JsUtil.delay(0.0, run);
            },
          );
        }),
      ),
    );
  };

  include Virtual_dom.Vdom.Attr.Hooks.Make({
    module State = State;
    module Input = Input;

    let init = (_input: Input.t, _element) =>
      State.{
        stick_to_bottom: true,
        listener_id: None,
      };

    let on_mount = (_input: Input.t, state: State.t, element) => {
      scroll_to_bottom(element);
      schedule_scroll_to_bottom(element);
      let handler =
        Dom.handler(_evt => {
          state.stick_to_bottom = is_near_bottom(element);
          Js._true;
        });
      let id =
        Dom.addEventListener(
          element,
          Dom_html.Event.scroll,
          handler,
          Js._false,
        );
      state.listener_id = Some(id);
    };

    let update =
        (~old_input: Input.t, ~new_input: Input.t, state: State.t, element) =>
      if (old_input != new_input && state.stick_to_bottom) {
        scroll_to_bottom(element);
        schedule_scroll_to_bottom(element);
      };

    let destroy = (_input: Input.t, state: State.t, _element) =>
      switch (state.listener_id) {
      | Some(id) => Dom_html.removeEventListener(id)
      | None => ()
      };
  });
};

let view =
    (
      ~globals: Globals.t,
      ~agent_model: Agent.Model.t,
      ~agent_inject: Agent.Update.Action.t => Effect.t(unit),
      ~signal: Editors.View.signal => Effect.t(unit),
      ~code_with_statics: CodeWithStatics.Model.t,
      ~eval_result: EvalResult.Model.t,
    )
    : Node.t => {
  let chat_system = agent_model.chat_system;
  let current_chat_id = chat_system.current;
  let current_chat = ChatSystem.Utils.find_chat(current_chat_id, chat_system);
  let chunked_chat = ChunkedUIChat.Utils.mk(current_chat);
  // Build the high-level node map once for this render pass, used by tool-call rows
  // for stale-path detection and cmd/ctrl-click jump targets.
  let node_map: option(HighLevelNodeMap.t) = {
    let z = code_with_statics.editor.state.zipper;
    let info_map = CompositionGo.Public.mk_statics(z);
    HighLevelNodeMap.build(z, info_map);
  };

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
      let updated_message = Message.Utils.mk_user_message(trimmed_content);
      // Send the message (appends and triggers LLM response)
      Effect.Many([
        agent_inject(
          Agent.Update.Action.ChatSystemAction(
            ChatSystem.Update.Action.ChatAction(
              Chat.Update.Action.BranchOff(
                Chat.Utils.parent_of(message_id, current_chat).id,
              ),
              current_chat_id,
            ),
          ),
        ),
        agent_inject(
          Agent.Update.Action.SendMessage(updated_message, current_chat_id),
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
      try(Some(Chat.Utils.parent_of(message_id, current_chat))) {
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
                Agent.Update.Action.ChatSystemAction(
                  ChatSystem.Update.Action.ChatAction(
                    Chat.Update.Action.SwitchBranch(
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
                Agent.Update.Action.ChatSystemAction(
                  ChatSystem.Update.Action.ChatAction(
                    Chat.Update.Action.SwitchBranch(
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

  let render_chunk = (index: int, chunk: ChunkedUIChat.Model.chunk) => {
    switch (chunk) {
    | ChunkedUIChat.Model.UserMessage(user_msg) =>
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
    | ChunkedUIChat.Model.AgentResponseChunk(agent_chunk) =>
      // Agent response chunk - display messages linearly
      // Render each message in the content list linearly.
      // Group consecutive ToolResult messages into a "batch" wrapper:
      // tool calls produced by a single LLM turn are emitted as one batch
      // (an Agent message ends the batch, as does a RetryNote / non-tool entry).
      let render_tool_node =
          (msg: Message.Model.t, tool_result: AgentToolResult.tool_result) => {
        let toggle_expanded = _ => {
          Effect.Many([
            agent_inject(
              Agent.Update.Action.ChatSystemAction(
                ChatSystem.Update.Action.ChatAction(
                  Chat.Update.Action.MessageAction(
                    msg.id,
                    Message.Update.SetToolResultExpanded(
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
        ToolResultView.view(
          ~globals,
          ~node_map?,
          ~tool_result,
          ~toggle_expanded,
          (),
        );
      };
      let wrap_batch = (nodes: list(Node.t)): Node.t =>
        switch (nodes) {
        | [single] =>
          // Solo tool call — no batch decoration
          single
        | _ => div(~attrs=[clss(["agent-tool-call-batch"])], nodes)
        };
      let humanize_tokens = (n: int): string =>
        if (n >= 1000) {
          Printf.sprintf("%.1fk", float_of_int(n) /. 1000.0);
        } else {
          string_of_int(n);
        };
      let render_token_chip =
          (
            ~msg_id: Id.t,
            ~usage: OpenRouter.Reply.Model.usage,
            ~prev_usage: option(OpenRouter.Reply.Model.usage),
          )
          : Node.t => {
        let cache_read =
          Option.value(~default=0, usage.cache_read_input_tokens);
        let cache_creation =
          Option.value(~default=0, usage.cache_creation_input_tokens);
        let cache_write = Option.value(~default=0, usage.cache_write_tokens);
        /* Billed amount as reported by OpenRouter, in credits. Shown to as many
           places as a sub-cent charge needs; this is the only number here that
           reflects what we are actually charged (see [[OpenRouter.Reply.Model.usage]]
           on why prompt_tokens is not a billing figure). */
        let fmt_credits = (c: float): string =>
          Printf.sprintf("%.6f", c) ++ " cr";
        let new_this_turn =
          switch (prev_usage) {
          | Some(p) when p.model_id == usage.model_id =>
            max(
              0,
              usage.prompt_tokens - p.prompt_tokens - p.completion_tokens,
            )
          | _ => usage.prompt_tokens
          };
        let preexisting = max(0, usage.prompt_tokens - new_this_turn);
        let chip_dom_id = "agent-token-chip-" ++ Id.to_string(msg_id);
        let toggle = _ => {
          Js.Opt.iter(
            Dom_html.document##getElementById(Js.string(chip_dom_id)),
            el => {
              let cl = el##.classList;
              let cls_str = Js.string("agent-token-chip-expanded");
              if (Js.to_bool(cl##contains(cls_str))) {
                cl##remove(cls_str);
              } else {
                cl##add(cls_str);
              };
            },
          );
          Effect.Stop_propagation;
        };
        let summary_parts = {
          let base = [
            humanize_tokens(usage.prompt_tokens) ++ " in",
            humanize_tokens(usage.completion_tokens) ++ " out",
          ];
          let base =
            if (cache_read > 0) {
              base @ ["cached: " ++ humanize_tokens(cache_read)];
            } else {
              base;
            };
          switch (usage.cost) {
          | Some(c) => base @ [fmt_credits(c)]
          | None => base
          };
        };
        let row = (label, value) =>
          div(
            ~attrs=[clss(["agent-token-chip-row"])],
            [
              span(
                ~attrs=[clss(["agent-token-chip-label"])],
                [text(label)],
              ),
              span(
                ~attrs=[clss(["agent-token-chip-value"])],
                [text(value)],
              ),
            ],
          );
        let raw_json =
          try(
            OpenRouter.Reply.Model.yojson_of_usage(usage)
            |> Yojson.Safe.pretty_to_string
          ) {
          | _ => ""
          };
        div(
          ~attrs=[
            Attr.id(chip_dom_id),
            clss(["agent-token-chip"]),
            Attr.on_click(toggle),
            Attr.title("Click for detailed token breakdown"),
          ],
          [
            div(
              ~attrs=[clss(["agent-token-chip-summary"])],
              [text(String.concat(" \xc2\xb7 ", summary_parts))],
            ),
            div(
              ~attrs=[clss(["agent-token-chip-body"])],
              [
                row(
                  "Total payload tokens",
                  humanize_tokens(usage.prompt_tokens),
                ),
                row("New tokens this turn", humanize_tokens(new_this_turn)),
                row(
                  "Preexisting (prior context)",
                  humanize_tokens(preexisting),
                ),
                row("Cache read", humanize_tokens(cache_read)),
                row("Cache creation", humanize_tokens(cache_creation)),
                row("Cache write", humanize_tokens(cache_write)),
                row(
                  "Output tokens",
                  humanize_tokens(usage.completion_tokens),
                ),
                row(
                  "Cost (billed)",
                  switch (usage.cost) {
                  | Some(c) => fmt_credits(c)
                  | None => "—"
                  },
                ),
                row(
                  "Upstream cost (BYOK)",
                  switch (usage.upstream_inference_cost) {
                  | Some(c) => fmt_credits(c)
                  | None => "—"
                  },
                ),
                Node.pre(
                  ~attrs=[clss(["agent-token-chip-raw"])],
                  [text(raw_json)],
                ),
              ],
            ),
          ],
        );
      };
      let prev_agent_usage_ref =
        ref(None: option(OpenRouter.Reply.Model.usage));
      let (linear_display_rev, pending_batch) =
        List.fold_left(
          ((acc, batch), msg: Message.Model.t) => {
            let flush = (acc, batch) =>
              switch (List.rev(batch)) {
              | [] => acc
              | xs => [wrap_batch(xs), ...acc]
              };
            switch (msg.role) {
            | ToolResult(tool_result) => (
                acc,
                [render_tool_node(msg, tool_result), ...batch],
              )
            | Agent(usage_opt) =>
              let acc = flush(acc, batch);
              let metadata_node: option(Node.t) =
                switch (usage_opt) {
                | Some(usage) =>
                  Some(
                    render_token_chip(
                      ~msg_id=msg.id,
                      ~usage,
                      ~prev_usage=prev_agent_usage_ref^,
                    ),
                  )
                | None => None
                };
              prev_agent_usage_ref := usage_opt;
              let show_thinking = globals.settings.agent_globals.show_thinking;
              let format_thinking_duration = (ms: int): string => {
                let secs = max(0, ms / 1000);
                if (secs < 60) {
                  "Thought for " ++ string_of_int(secs) ++ "s";
                } else {
                  let m = secs / 60;
                  let s = secs mod 60;
                  "Thought for "
                  ++ string_of_int(m)
                  ++ "m "
                  ++ string_of_int(s)
                  ++ "s";
                };
              };
              let reasoning_node: option(Node.t) =
                switch (show_thinking, msg.reasoning) {
                | (true, Some(text_content))
                    when String.trim(text_content) != "" =>
                  let header_text =
                    switch (msg.reasoning_duration_ms) {
                    | Some(ms) => format_thinking_duration(ms)
                    | None => "Thinking"
                    };
                  Some(
                    div(
                      ~attrs=[clss(["agent-thinking-block"])],
                      [
                        div(
                          ~attrs=[clss(["agent-thinking-header"])],
                          [text(header_text)],
                        ),
                        div(
                          ~attrs=[clss(["agent-thinking-text"])],
                          [AgentMessageMarkdown.view(text_content)],
                        ),
                      ],
                    ),
                  );
                | _ => None
                };
              let content_node: option(Node.t) =
                if (msg.content != "" && String.trim(msg.content) != "") {
                  Some(
                    div(
                      ~attrs=[clss(["agent-message"])],
                      [AgentMessageMarkdown.view(msg.content)],
                    ),
                  );
                } else {
                  None;
                };
              let nodes =
                List.filter_map(
                  x => x,
                  [reasoning_node, content_node, metadata_node],
                );
              switch (nodes) {
              | [] => (acc, [])
              | xs => (List.rev_append(xs, acc), [])
              };
            | System(RetryNote) =>
              let acc = flush(acc, batch);
              let node =
                div(
                  ~attrs=[
                    clss(["agent-system-message", "agent-retry-note"]),
                  ],
                  [text(msg.content)],
                );
              ([node, ...acc], []);
            | System(ResponseCancelled)
            | _ => (acc, batch)
            };
          },
          ([], []),
          agent_chunk.content,
        );
      let linear_display =
        List.rev(
          switch (List.rev(pending_batch)) {
          | [] => linear_display_rev
          | xs => [wrap_batch(xs), ...linear_display_rev]
          },
        );

      let is_edit_tool_call = (tool_result: AgentToolResult.tool_result): bool => {
        switch (tool_result.tool_call.name) {
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
        |> List.filter_map((msg: Message.Model.t) =>
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
            Agent.Update.Action.ChatSystemAction(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.MessageAction(
                  msg_id,
                  Message.Update.SetToolResultExpanded(true),
                ),
                current_chat_id,
              ),
            ),
          ),
          Effect.Stop_propagation,
        ]);
      };

      let edit_summary_nodes =
        switch (edit_tool_results) {
        | [] => []
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
                    Agent.Update.Action.LoadTimelineSegment(
                      segment,
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
              if (tool_result.skipped) {
                "tool-call-skipped";
              } else if (tool_result.success) {
                "tool-call-success";
              } else {
                "tool-call-failure";
              };
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
            let summary_opt =
              ToolCallSummary.of_tool_call(tool_result.tool_call);
            let signifier_node =
              switch (summary_opt) {
              | Some({signifier: Some(s), _}) =>
                span(~attrs=[clss(["tool-call-signifier"])], [text(s)])
              | _ => Node.none
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
                  [
                    if (tool_result.skipped) {
                      Icons.circle_with_minus;
                    } else if (tool_result.success) {
                      Icons.confirm;
                    } else {
                      Icons.cancel;
                    },
                  ],
                ),
                span(
                  ~attrs=[clss(["summary-tool-link-name"])],
                  [
                    text(
                      ToolCallSummary.display_name_for(tool_result.tool_call),
                    ),
                  ],
                ),
                signifier_node,
                span(
                  ~attrs=[clss(["summary-tool-link-arrow"])],
                  [text({|↗|})],
                ),
              ],
            );
          };

          let initial_node: timeline_node = {
            segment: first.before_segment,
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
                      agent_inject(Agent.Update.Action.RestoreOriginal),
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

          [
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
            ),
          ];
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
            linear_display @ edit_summary_nodes,
          ),
        ],
      );
    | ChunkedUIChat.Model.CompactionNotice({method, content}) =>
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
          div(
            ~attrs=[clss(["compaction-notice-body"])],
            [AgentMessageMarkdown.view(content)],
          ),
        ],
      )
    | ChunkedUIChat.Model.ErrorMessage(error_content) =>
      // Error messages centered, red
      div(
        ~attrs=[
          clss(["message-container", "system-message-container", "error"]),
        ],
        [div(~attrs=[clss(["system-message"])], [text(error_content)])],
      )
    | ChunkedUIChat.Model.ResponseCancelledMessage(content) =>
      div(
        ~attrs=[
          clss([
            "message-container",
            "system-message-container",
            "response-cancelled-message-container",
          ]),
        ],
        [
          div(
            ~attrs=[
              clss([
                "system-message",
                "agent-system-message",
                "agent-response-cancelled",
              ]),
            ],
            [text(content)],
          ),
        ],
      )
    | ChunkedUIChat.Model.SlashCommandOutputMessage(payload) =>
      SlashCommandOutputView.view(payload)
    };
  };

  // Check current view and render appropriate view
  switch (current_chat.current_view) {
  | Chat.Model.Messages =>
    // Normal messages view (content only, bottom bar handled in ChatView)
    let awaiting_dots =
      switch (agent_model.awaiting_response) {
      | Some(id) when id == current_chat_id => true
      | _ => false
      };
    let compaction_banner =
      switch (agent_model.compaction_in_progress) {
      | Some(id) when id == current_chat_id => true
      | _ => false
      };
    let scroll_sync_stamp =
      chat_messages_scroll_stamp(
        ~chunked_chat,
        ~awaiting_dots,
        ~compaction_banner,
        ~pending_content=agent_model.pending_assistant_content,
        ~pending_reasoning=agent_model.pending_assistant_reasoning,
      );
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
          ~attrs=[
            clss(["chat-messages-container"]),
            Attr.create_hook(
              "hazel-chat-messages-auto-scroll",
              ChatMessagesScrollHook.create(scroll_sync_stamp),
            ),
          ],
          List.mapi(render_chunk, chunked_chat.log)
          @ (
            switch (agent_model.awaiting_response) {
            | Some(awaiting_id) when awaiting_id == current_chat_id =>
              let show_thinking = globals.settings.agent_globals.show_thinking;
              let pending_content = agent_model.pending_assistant_content;
              let pending_reasoning = agent_model.pending_assistant_reasoning;
              let has_reasoning =
                show_thinking && String.trim(pending_reasoning) != "";
              let has_content = String.trim(pending_content) != "";
              let reasoning_node: option(Node.t) =
                if (has_reasoning) {
                  Some(
                    div(
                      ~attrs=[clss(["agent-thinking-block"])],
                      [
                        div(
                          ~attrs=[clss(["agent-thinking-header"])],
                          [text("Thinking")],
                        ),
                        div(
                          ~attrs=[
                            clss([
                              "agent-thinking-text",
                              "agent-streaming-plaintext",
                            ]),
                          ],
                          [text(pending_reasoning)],
                        ),
                      ],
                    ),
                  );
                } else {
                  None;
                };
              /* Render the partial buffer as markdown on every frame; dangling
                 inline constructs are virtually closed so they format in place
                 (see AgentMessageMarkdown.close_dangling_inline). */
              let content_node: option(Node.t) =
                if (has_content) {
                  Some(
                    div(
                      ~attrs=[clss(["agent-message"])],
                      [AgentMessageMarkdown.view_streaming(pending_content)],
                    ),
                  );
                } else {
                  None;
                };
              let body_nodes: list(Node.t) =
                if (has_reasoning || has_content) {
                  List.filter_map(x => x, [reasoning_node, content_node]);
                } else {
                  [
                    div(
                      ~attrs=[clss(["agent-message-loading-dots"])],
                      [
                        span(~attrs=[clss(["dot", "dot1"])], []),
                        span(~attrs=[clss(["dot", "dot2"])], []),
                        span(~attrs=[clss(["dot"])], []),
                      ],
                    ),
                  ];
                };
              [
                div(
                  ~attrs=[
                    clss(["message-container", "agent-message-container"]),
                  ],
                  [
                    div(
                      ~attrs=[
                        clss(["message-identifier", "llm-identifier"]),
                      ],
                      [Icons.filbert, text("Filbert")],
                    ),
                    ...body_nodes,
                  ],
                ),
              ];
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
    );
  | Chat.Model.Prompt =>
    ViewComponents.prompt_view(
      ~content=chunked_chat.prompt,
      ~agent_inject,
      ~chat_id=current_chat_id,
    )
  | Chat.Model.DeveloperNotes =>
    ViewComponents.developer_notes_view(
      ~content=chunked_chat.developer_notes,
      ~agent_inject,
      ~chat_id=current_chat_id,
    )
  | Chat.Model.Tools =>
    ViewComponents.tools_view(
      ~agent_model,
      ~agent_inject,
      ~chat_id=current_chat_id,
    )
  | Chat.Model.AgentEditorView
  | Chat.Model.StaticErrors =>
    // Both AgentEditorView and StaticErrors now show context
    ViewComponents.context_view(
      ~globals,
      ~code_with_statics,
      ~agent_view=current_chat.agent_view,
      ~eval_result,
      ~current_chat,
      ~agent_inject,
      ~chat_id=current_chat_id,
    )
  | Chat.Model.Workbench =>
    ViewComponents.workbench_view(~agent_inject, ~chat_id=current_chat_id)
  };
};
