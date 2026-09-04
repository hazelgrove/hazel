open Virtual_dom.Vdom;
open Node;
open Util_web.WebUtil;
open Util_web;
open Js_of_ocaml;
open Haz3lcore;

open JsUtil;

// Shared bottom bar component for Chat and Workbench views
let view =
    (
      ~globals: Globals.t,
      ~agent_model: Agent.Model.t,
      ~agent_inject: Agent.Update.Action.t => Effect.t(unit),
      ~signal: Editors.View.signal => Effect.t(unit),
      ~chunked_chat: ChunkedUIChat.Model.t,
      ~current_chat_id: Id.t,
    )
    : Node.t => {
  let chat_system = agent_model.chat_system;
  let current_chat = ChatSystem.Utils.find_chat(current_chat_id, chat_system);
  let is_compacting =
    switch (agent_model.compaction_in_progress) {
    | Some(id) when id == current_chat_id => true
    | _ => false
    };
  let is_awaiting_assistant =
    switch (agent_model.awaiting_response) {
    | Some(id) when id == current_chat_id => true
    | _ => false
    };
  let agent_busy = is_compacting || is_awaiting_assistant;

  let slash_menu = chat_system.ui.slash_menu;

  let clear_text_effect =
    agent_inject(
      Agent.Update.Action.ChatSystemAction(
        ChatSystem.Update.Action.SaveTextBoxContent(""),
      ),
    );

  let dispatch_slash = (action: Agent.Update.Action.t) =>
    Effect.Many([
      agent_inject(action),
      clear_text_effect,
      Effect.Stop_propagation,
    ]);

  let effect_run_slash_command = (name: string) =>
    switch (name) {
    | "compact" =>
      dispatch_slash(
        Agent.Update.Action.RequestForcedCompaction(current_chat_id),
      )
    | "session-usage" =>
      dispatch_slash(
        Agent.Update.Action.RunSlashCommandCost(current_chat_id),
      )
    | "account-usage" =>
      dispatch_slash(
        Agent.Update.Action.RunSlashCommandFetchCredits(current_chat_id),
      )
    | "help" =>
      dispatch_slash(
        Agent.Update.Action.RunSlashCommandHelp(current_chat_id),
      )
    | "key" =>
      dispatch_slash(
        Agent.Update.Action.RunSlashCommandShowKey(current_chat_id),
      )
    | "key-usage" =>
      dispatch_slash(
        Agent.Update.Action.RunSlashCommandFetchUsage(current_chat_id),
      )
    | "show-thinking" =>
      // Toggle the global flag and confirm with a UI-only Notice. The "after"
      // state is the inverse of the current value, since the toggle reducer
      // hasn't run yet at the time we format the message.
      let next_on = !globals.settings.agent_globals.show_thinking;
      let notice =
        "Show thinking messages toggled " ++ (next_on ? "on" : "off");
      Effect.Many([
        globals.inject_global(
          Globals.Action.SetAgentGlobals(
            AgentGlobals.Update.ToggleShowThinking,
          ),
        ),
        agent_inject(
          Agent.Update.Action.AppendSlashCommandOutput(
            current_chat_id,
            Message.Model.Notice(notice),
          ),
        ),
        clear_text_effect,
        Effect.Stop_propagation,
      ]);
    | _ => Effect.Stop_propagation
    };

  // Auto-resize textarea helper
  let autosize_textarea = (id: string) => {
    Js.Opt.iter(
      Dom_html.document##getElementById(Js.string(id)),
      el => {
        let textarea = Js.Unsafe.coerce(el);
        textarea##.style##.height := Js.string("auto");
        let scroll_height = textarea##.scrollHeight;
        let max_height = 200;
        let height = min(scroll_height, max_height);
        textarea##.style##.height := Js.string(string_of_int(height) ++ "px");
        textarea##.style##.overflowY :=
          Js.string(scroll_height > max_height ? "auto" : "hidden");
      },
    );
  };

  // Get current text box content from model
  let current_text = chat_system.ui.current_text_box_content;

  // Handle textarea input
  let handle_textarea_input = (_event, value: string) => {
    JsUtil.delay(0.0, () => autosize_textarea("chat-message-input"));
    Effect.Many([
      agent_inject(
        Agent.Update.Action.ChatSystemAction(
          ChatSystem.Update.Action.SaveTextBoxContent(value),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  // Send / queue: while the agent is busy, SendMessage enqueues for later (see Agent.send_message).
  let send_message = _ => {
    let message_content = String.trim(current_text);
    if (String.length(message_content) > 0) {
      let user_message = Message.Utils.mk_user_message(message_content);
      Effect.Many([
        agent_inject(
          Agent.Update.Action.SendMessage(user_message, current_chat_id),
        ),
        agent_inject(
          Agent.Update.Action.ChatSystemAction(
            ChatSystem.Update.Action.SaveTextBoxContent(""),
          ),
        ),
        Effect.Stop_propagation,
      ]);
    } else {
      Effect.Stop_propagation;
    };
  };

  // Handler functions for icon buttons
  let switch_to_prompt = _ => {
    Effect.Many([
      agent_inject(
        Agent.Update.Action.ChatSystemAction(
          ChatSystem.Update.Action.ChatAction(
            Chat.Update.Action.SwitchView(Chat.Model.Prompt),
            current_chat_id,
          ),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  let switch_to_tools = _ => {
    Effect.Many([
      agent_inject(
        Agent.Update.Action.ChatSystemAction(
          ChatSystem.Update.Action.ChatAction(
            Chat.Update.Action.SwitchView(Chat.Model.Tools),
            current_chat_id,
          ),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  let switch_to_context_view = _ => {
    Effect.Many([
      agent_inject(
        Agent.Update.Action.ChatSystemAction(
          ChatSystem.Update.Action.ChatAction(
            Chat.Update.Action.SwitchView(Chat.Model.AgentEditorView),
            current_chat_id,
          ),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  // Export messages function
  let export_chat = _ => {
    let messages = Chat.Utils.get(current_chat);
    let messages_json =
      Chat.Utils.json_of_messages(
        messages,
        AgentGlobals.get_active_llm_id(globals.settings.agent_globals),
      );
    let filename =
      StringUtil.sanitize_filename(current_chat.title)
      ++ "_openrouter_"
      ++ string_of_float(current_chat.created_at);
    download_json(filename, messages_json);
    Effect.Stop_propagation;
  };

  // Copy chat as human-readable text function with toast notification
  let stop_agent = _ =>
    Effect.Many([
      agent_inject(Agent.Update.Action.StopAgenticLoop),
      Effect.Stop_propagation,
    ]);

  let copy_chat = _ => {
    let messages = Chat.Utils.get(current_chat);
    let user_facing_messages =
      List.filter(
        (msg: Message.Model.t) =>
          switch (msg.role) {
          | Message.Model.System(_) => false
          | _ => true
          },
        messages,
      );
    let format_message = (msg: Message.Model.t): string => {
      switch (msg.role) {
      | Message.Model.User => "User: " ++ msg.content ++ "\n\n"
      | Message.Model.Agent(_) => "LLM: " ++ msg.content ++ "\n\n"
      | Message.Model.ToolResult(tool_result) =>
        "Tool Call: "
        ++ tool_result.tool_call.name
        ++ " "
        ++ (
          tool_result.skipped
            ? "[not executed]"
            : tool_result.success ? "[success]" : "[failure]"
        )
        ++ "\n\n"
      | Message.Model.System(_) => ""
      };
    };
    let formatted_text =
      List.fold_left(
        (acc, msg) => acc ++ format_message(msg),
        "",
        user_facing_messages,
      );
    JsUtil.focus_clipboard_shim();
    Js.Opt.iter(
      Dom_html.document##getElementById(Js.string("clipboard-shim")),
      clipboard_shim_el => {
        let clipboard_shim = Js.Unsafe.coerce(clipboard_shim_el);
        clipboard_shim##.value := Js.string(formatted_text);
        ignore(clipboard_shim##select);
        ignore(
          Dom_html.document##execCommand(
            Js.string("copy"),
            Js.bool(false),
            Js.Opt.empty,
          ),
        );
      },
    );
    Js.Opt.iter(
      Dom_html.document##getElementById(Js.string("copy-toast")),
      toast => {
        toast##.classList##add(Js.string("show"));
        ignore(
          Dom_html.window##setTimeout(
            Js.wrap_callback(() => {
              toast##.classList##remove(Js.string("show"))
            }),
            2000.0,
          ),
        );
      },
    );
    Effect.Stop_propagation;
  };

  /** Provider-reported prompt_tokens from the last agent turn when applicable; [None] → “—” in the
      label (e.g. before any reply or after compaction until the next assistant message reports usage). */
  let last_prompt_tokens_opt: option(int) =
    Chat.Utils.context_meter_prompt_tokens(current_chat);
  let context_limit_opt =
    AgentGlobals.context_meter_limit_for_active(
      globals.settings.agent_globals,
    );
  let raw_context_opt =
    AgentGlobals.context_length_for_active(globals.settings.agent_globals);
  let (meter_base_label, meter_pct_line_opt, fill_pct_opt, hover_title_pct) = {
    let fmt_tokens = m =>
      m mod 1000 == 0 ? string_of_int(m / 1000) ++ "k" : string_of_int(m);
    let n_str =
      switch (last_prompt_tokens_opt) {
      | Some(n) => string_of_int(n)
      | None => "—"
      };
    let m_str =
      switch (context_limit_opt) {
      | Some(m) => fmt_tokens(m)
      | None => "—"
      };
    /* The meter shows the compaction budget, not the model's raw window;
       say so when they differ (see AgentGlobals.effective_context_meter_limit). */
    let capped_note =
      switch (context_limit_opt, raw_context_opt) {
      | (Some(limit), Some(raw)) when limit < raw =>
        Some(
          "compaction budget "
          ++ fmt_tokens(limit)
          ++ " (model window "
          ++ fmt_tokens(raw)
          ++ ")",
        )
      | _ => None
      };
    let used_word = capped_note == None ? " context used" : " budget used";
    let base_label = n_str ++ " / " ++ m_str ++ used_word;
    let title = pct =>
      switch (pct, capped_note) {
      | (Some(p), Some(note)) => Some(p ++ " — " ++ note)
      | (Some(p), None) => Some(p)
      | (None, note) => note
      };
    switch (last_prompt_tokens_opt, context_limit_opt) {
    | (Some(n), Some(m)) when m > 0 =>
      let frac = float_of_int(n) /. float_of_int(m);
      let bar_pct = min(100, int_of_float(ceil(frac *. 100.0)));
      let pct_line = "(" ++ Printf.sprintf("%.1f%%", frac *. 100.0) ++ ")";
      (
        base_label,
        Some(pct_line),
        Some(bar_pct),
        title(Some(Printf.sprintf("%.2f%%", frac *. 100.0))),
      );
    | _ => (base_label, None, None, title(None))
    };
  };

  let token_context_meter_node: Node.t =
    div(
      ~attrs=[
        clss(["token-context-meter"]),
        ...switch (hover_title_pct) {
           | Some(t) => [Attr.title(t)]
           | None => []
           },
      ],
      [
        div(
          ~attrs=[clss(["token-context-meter-label"])],
          [text(meter_base_label)],
        ),
        div(
          ~attrs=[clss(["context-meter-track"])],
          [
            div(
              ~attrs=[
                clss(["context-meter-fill"]),
                ...switch (fill_pct_opt) {
                   | Some(pct) => [
                       Attr.style(
                         Css_gen.create(
                           ~field="width",
                           ~value=string_of_int(pct) ++ "%",
                         ),
                       ),
                     ]
                   | None => [
                       Attr.style(
                         Css_gen.create(~field="width", ~value="0%"),
                       ),
                     ]
                   },
              ],
              [],
            ),
          ],
        ),
        switch (meter_pct_line_opt) {
        | Some(line) =>
          div(~attrs=[clss(["token-context-meter-pct"])], [text(line)])
        | None => div(~attrs=[], [])
        },
      ],
    );

  // "change model" text button — routes to MainMenu screen.
  let switch_to_main_menu = _ => {
    Effect.Many([
      globals.inject_global(
        Globals.Action.SetAgentGlobals(
          AgentGlobals.Update.SwitchInterface(AgentGlobals.Model.MainMenu),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };
  // Pretty-print model id by stripping provider prefix and title-casing the slug.
  // "google/gemini-3-flash-preview" -> "Gemini 3 Flash Preview".
  let pretty_model_name = (id: string): string => {
    let after_slash =
      switch (String.index_opt(id, '/')) {
      | Some(i) => String.sub(id, i + 1, String.length(id) - i - 1)
      | None => id
      };
    let parts = String.split_on_char('-', after_slash);
    let cap = (s: string): string =>
      if (s == "") {
        s;
      } else {
        let first = String.sub(s, 0, 1) |> String.uppercase_ascii;
        let rest = String.sub(s, 1, String.length(s) - 1);
        first ++ rest;
      };
    String.concat(" ", List.map(cap, parts));
  };

  let change_model_button: Node.t =
    div(
      ~attrs=[
        clss(["change-model-button"]),
        Attr.on_click(switch_to_main_menu),
        Attr.title("Choose a different model"),
      ],
      [text("change model")],
    );

  let model_name_label: option(Node.t) = {
    let agent_globals = globals.settings.agent_globals;
    switch (agent_globals.active_llm) {
    | None => None
    | Some(llm) =>
      Some(
        div(
          ~attrs=[clss(["change-model-current-name"])],
          [text(pretty_model_name(llm.id))],
        ),
      )
    };
  };

  /* Cache indicator: lit when the most recent assistant turn reported
     cache_read_input_tokens > 0 AND the recorded model_id matches the
     currently-active LLM. Anthropic prompt caches are per-model, so a
     model switch (or expiry) correctly drops the indicator. */
  let cache_indicator_label: option(Node.t) = {
    let active_id =
      AgentGlobals.get_active_llm_id(globals.settings.agent_globals);
    let last_usage =
      List.fold_left(
        (acc, m: Message.Model.t) =>
          switch (m.role) {
          | Agent(Some(u)) => Some(u)
          | _ => acc
          },
        None,
        Chat.Utils.get(current_chat),
      );
    let (lit, tooltip) =
      switch (last_usage, active_id) {
      | (
          Some(
            {cache_read_input_tokens: Some(n), model_id: Some(mid), _}: OpenRouter.Reply.Model.usage,
          ),
          Some(aid),
        )
          when n > 0 && mid == aid => (
          true,
          Printf.sprintf(
            "Prompt cache hit on last turn (%d cached tokens). Cache is per-model; switching models invalidates it.",
            n,
          ),
        )
      | _ => (
          false,
          "No prompt cache hit on the last turn. Cache is per-model (Anthropic only) and expires after ~5 min idle.",
        )
      };
    let classes = ["cache-indicator-dot"] @ (lit ? ["lit"] : []);
    Some(div(~attrs=[clss(classes), Attr.title(tooltip)], []));
  };

  // Reasoning effort dropup (only for models that support reasoning).
  // Rendered as an absolute-positioned overlay inside the chat input container,
  // anchored to its bottom-left corner.
  let reasoning_effort_dropup: Node.t = {
    let agent_globals = globals.settings.agent_globals;
    if (AgentGlobals.active_supports_reasoning(agent_globals)) {
      let current_label =
        switch (agent_globals.reasoning_effort) {
        | None => "Off"
        | Some(Low) => "Low"
        | Some(Medium) => "Medium"
        | Some(High) => "High"
        };
      let set_effort =
          (e: option(OpenRouter.Payload.Model.effort_level), _evt) => {
        // Close the dropup after selection. Defer the blur to the next tick so any
        // focus reshuffle from the mousedown/click has already happened.
        JsUtil.delay(0.0, () =>
          Js.Opt.iter(
            Dom_html.document##.activeElement,
            el => {
              let coerced = Js.Unsafe.coerce(el);
              ignore(coerced##blur());
            },
          )
        );
        Effect.Many([
          globals.inject_global(
            Globals.Action.SetAgentGlobals(
              AgentGlobals.Update.SetReasoningEffort(e),
            ),
          ),
          Effect.Stop_propagation,
        ]);
      };
      let menu_item =
          (label: string, e: option(OpenRouter.Payload.Model.effort_level)) => {
        let selected =
          e == agent_globals.reasoning_effort ? ["selected"] : [];
        div(
          ~attrs=[
            clss(["reasoning-effort-menu-item"] @ selected),
            Attr.on_mousedown(set_effort(e)),
          ],
          [text(label)],
        );
      };
      div(
        ~attrs=[
          clss(["reasoning-effort-dropup"]),
          Attr.create("tabindex", "0"),
        ],
        [
          div(
            ~attrs=[
              clss(["reasoning-effort-button"]),
              Attr.title(
                "Reasoning effort (only sent for models that support it)",
              ),
            ],
            [text("\xE2\x8C\x83 " ++ current_label)],
          ),
          div(
            ~attrs=[clss(["reasoning-effort-menu"])],
            [
              menu_item("Off", None),
              menu_item("Low", Some(Low)),
              menu_item("Medium", Some(Medium)),
              menu_item("High", Some(High)),
            ],
          ),
        ],
      );
    } else {
      Node.none;
    };
  };

  let top_bar_collapsed = globals.settings.agent_globals.collapse_top_bar;
  let toggle_top_bar = _ =>
    Effect.Many([
      globals.inject_global(
        Globals.Action.SetAgentGlobals(
          AgentGlobals.Update.ToggleCollapseTopBar,
        ),
      ),
      Effect.Stop_propagation,
    ]);
  let top_bar_toggle_button: Node.t =
    div(
      ~attrs=[
        clss(["top-bar-collapse-toggle"]),
        Attr.on_click(toggle_top_bar),
        Attr.title(
          top_bar_collapsed
            ? "Show context meter and chat tools"
            : "Hide context meter and chat tools",
        ),
      ],
      [text(top_bar_collapsed ? "\xE2\x8C\x84" : "\xE2\x8C\x83")],
    );

  // Input area at bottom with buttons above
  div(
    ~attrs=[clss(["chat-input-container"])],
    [
      // Action buttons row - above input, left side buttons and right side export
      top_bar_collapsed
        ? Node.none
        : div(
            ~attrs=[clss(["chat-action-buttons-row"])],
            [
              // Left side buttons
              div(
                ~attrs=[clss(["chat-action-buttons-left"])],
                [
                  // Prompt button
                  if (chunked_chat.prompt != "") {
                    div(
                      ~attrs=[
                        clss(["chat-action-button", "icon"]),
                        Attr.on_click(switch_to_prompt),
                        Attr.title("View System Prompt"),
                      ],
                      [Icons.prompt],
                    );
                  } else {
                    div(~attrs=[], []);
                  },
                  // Tools button - configure which tools the agent can use
                  div(
                    ~attrs=[
                      clss(["chat-action-button", "icon"]),
                      Attr.on_click(switch_to_tools),
                      Attr.title("Configure Agent Tools"),
                    ],
                    [Icons.wrench],
                  ),
                  // Context View button (shows agent editor view, static errors, and workbench)
                  if (chunked_chat.context != "") {
                    div(
                      ~attrs=[
                        clss(["chat-action-button", "icon"]),
                        Attr.on_click(switch_to_context_view),
                        Attr.title("View Agent Context"),
                      ],
                      [Icons.agent_view],
                    );
                  } else {
                    div(~attrs=[], []);
                  },
                ],
              ),
              token_context_meter_node,
              // Right side export and copy buttons
              div(
                ~attrs=[clss(["chat-action-buttons-right"])],
                [
                  div(
                    ~attrs=[
                      clss(["chat-action-button", "icon"]),
                      Attr.on_click(export_chat),
                      Attr.title("Export Messages (JSON)"),
                    ],
                    [Icons.export],
                  ),
                  div(
                    ~attrs=[
                      clss(["chat-action-button", "icon"]),
                      Attr.on_click(copy_chat),
                      Attr.title("Copy Chat (Human-readable)"),
                    ],
                    [Icons.copy],
                  ),
                ],
              ),
            ],
          ),
      if (current_chat.pending_send_queue != []) {
        div(
          ~attrs=[clss(["chat-send-queue-panel"])],
          [
            div(
              ~attrs=[clss(["chat-send-queue-header"])],
              [text("Queue")],
            ),
            div(
              ~attrs=[clss(["chat-send-queue-body"])],
              [
                text(String.concat("\n\n", current_chat.pending_send_queue)),
              ],
            ),
          ],
        );
      } else {
        div(~attrs=[], []);
      },
      {
        let mode = globals.settings.agent_globals.session_mode;
        let label = AgentGlobals.session_mode_label(mode);
        let mode_class =
          switch (mode) {
          | Converse => "session-mode-converse"
          | Edit => "session-mode-edit"
          | Plan => "session-mode-plan"
          };
        let mode_explanation =
          switch (mode) {
          | Converse => "Converse mode: pure conversation. Edits, overlay placement, and workbench tasks are disabled. Only view tools (expand / collapse) are allowed. Use this to discuss ideas, ask questions, and clarify intent without changing any state."
          | Edit => "Edit mode: full latitude. All tools are enabled (subject to your per-tool toggles). The agent may plan, converse, place overlays, manage workbench tasks, and apply program edits."
          | Plan => "Plan mode: read-only with respect to program code. Edit tools are disabled; the agent is encouraged to inspect the codebase, converse with you in markdown, and build an explicit plan using the workbench (create / order tasks and subtasks). Switch to edit mode when ready to execute."
          };
        let cycle = _ =>
          Effect.Many([
            globals.inject_global(
              Globals.Action.SetAgentGlobals(
                AgentGlobals.Update.CycleSessionMode,
              ),
            ),
            Effect.Stop_propagation,
          ]);
        div(
          ~attrs=[clss(["chat-input-top-bar"])],
          [
            div(
              ~attrs=[clss(["chat-input-top-bar-left"])],
              [
                top_bar_toggle_button,
                span(
                  ~attrs=[clss(["session-mode-info"])],
                  [
                    span(
                      ~attrs=[clss(["session-mode-info-icon"])],
                      [Icons.info],
                    ),
                    div(
                      ~attrs=[clss(["session-mode-info-tooltip"])],
                      [text(mode_explanation)],
                    ),
                  ],
                ),
                span(
                  ~attrs=[
                    clss(["session-mode-toggle", mode_class]),
                    Attr.on_click(cycle),
                    Attr.title(
                      "Click to cycle session mode (edit → converse → plan)",
                    ),
                  ],
                  [text(label)],
                ),
              ],
            ),
            div(
              ~attrs=[clss(["chat-input-top-bar-right"])],
              List.filter_map(
                x => x,
                [cache_indicator_label, model_name_label],
              ),
            ),
          ],
        );
      },
      div(
        ~attrs=[clss(["chat-message-input-container"])],
        [
          switch (slash_menu) {
          | None => div(~attrs=[], [])
          | Some(sm) =>
            let cmds = ChatSlashCommands.filtered(sm.filter);
            div(
              ~attrs=[clss(["chat-slash-menu"])],
              List.mapi(
                (i, (name, desc)) =>
                  div(
                    ~attrs=[
                      clss(
                        ["chat-slash-menu-item"]
                        @ (sm.selected_index == i ? ["selected"] : []),
                      ),
                      Attr.on_mousedown(_ =>
                        Effect.Many([
                          effect_run_slash_command(name),
                          Effect.Prevent_default,
                        ])
                      ),
                    ],
                    [
                      span(
                        ~attrs=[clss(["chat-slash-cmd"])],
                        [text("/" ++ name)],
                      ),
                      span(
                        ~attrs=[clss(["chat-slash-desc"])],
                        [text(desc)],
                      ),
                    ],
                  ),
                cmds,
              ),
            );
          },
          textarea(
            ~attrs=[
              clss(["chat-message-input"]),
              Attr.id("chat-message-input"),
              Attr.placeholder(
                is_compacting
                  ? "Compacting… Type a message to add to queue..."
                  : is_awaiting_assistant
                      ? "Type a message to add to queue..."
                      : "Type your message...",
              ),
              Attr.property("autocomplete", Js.Unsafe.inject("off")),
              Attr.on_focus(_ => {
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
                    Editors.View.MakeActive(Editors.Selection.Assistant),
                  ),
                  Effect.Stop_propagation,
                ]);
              }),
              Attr.on_blur(_ => {
                JsUtil.delay(0.0, () =>
                  autosize_textarea("chat-message-input")
                );
                Effect.Stop_propagation;
              }),
              Attr.on_input(handle_textarea_input),
              Attr.on_keydown(event => {
                let key = Js.Optdef.to_option(Js.Unsafe.get(event, "key"));
                let shift_pressed = Key.shift_held(event);
                switch (key) {
                | Some("ArrowDown") =>
                  switch (slash_menu) {
                  | Some(_) =>
                    Effect.Many([
                      agent_inject(
                        Agent.Update.Action.ChatSystemAction(
                          ChatSystem.Update.Action.SlashMenuAdjustSelection(
                            1,
                          ),
                        ),
                      ),
                      Effect.Prevent_default,
                      Effect.Stop_propagation,
                    ])
                  | None => Effect.Stop_propagation
                  }
                | Some("ArrowUp") =>
                  switch (slash_menu) {
                  | Some(_) =>
                    Effect.Many([
                      agent_inject(
                        Agent.Update.Action.ChatSystemAction(
                          ChatSystem.Update.Action.SlashMenuAdjustSelection(
                            -1,
                          ),
                        ),
                      ),
                      Effect.Prevent_default,
                      Effect.Stop_propagation,
                    ])
                  | None => Effect.Stop_propagation
                  }
                | Some("Escape") =>
                  switch (slash_menu) {
                  | Some(_) =>
                    Effect.Many([
                      agent_inject(
                        Agent.Update.Action.ChatSystemAction(
                          ChatSystem.Update.Action.SaveTextBoxContent(""),
                        ),
                      ),
                      Effect.Prevent_default,
                      Effect.Stop_propagation,
                    ])
                  | None => Effect.Stop_propagation
                  }
                | Some("Enter") when !shift_pressed =>
                  switch (slash_menu) {
                  | Some(sm) =>
                    let cmds = ChatSlashCommands.filtered(sm.filter);
                    switch (List.nth_opt(cmds, sm.selected_index)) {
                    | Some((name, _)) =>
                      Js.Opt.iter(
                        Dom_html.document##getElementById(
                          Js.string("chat-message-input"),
                        ),
                        el => {
                          let textarea = Js.Unsafe.coerce(el);
                          textarea##blur();
                        },
                      );
                      Effect.Many([
                        effect_run_slash_command(name),
                        Effect.Prevent_default,
                        Effect.Stop_propagation,
                      ]);
                    | None =>
                      Js.Opt.iter(
                        Dom_html.document##getElementById(
                          Js.string("chat-message-input"),
                        ),
                        el => {
                          let textarea = Js.Unsafe.coerce(el);
                          textarea##blur();
                        },
                      );
                      Effect.Many([
                        send_message(),
                        Effect.Prevent_default,
                        Effect.Stop_propagation,
                      ]);
                    };
                  | None =>
                    Js.Opt.iter(
                      Dom_html.document##getElementById(
                        Js.string("chat-message-input"),
                      ),
                      el => {
                        let textarea = Js.Unsafe.coerce(el);
                        textarea##blur();
                      },
                    );
                    Effect.Many([
                      send_message(),
                      Effect.Prevent_default,
                      Effect.Stop_propagation,
                    ]);
                  }
                | Some("Enter") => Effect.Stop_propagation
                | _ => Effect.Stop_propagation
                };
              }),
              Attr.on_copy(_ => Effect.Stop_propagation),
              Attr.on_paste(_ => {
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
          {
            // Thin in-composer bottom bar: reasoning dropup left, send/stop right.
            let queue_button =
              if (agent_busy && String.length(String.trim(current_text)) > 0) {
                div(
                  ~attrs=[
                    clss([
                      "send-button",
                      "icon",
                      "chat-message-queue-send-button",
                    ]),
                    Attr.on_click(send_message),
                    Attr.title("Add to queue (same as Enter)"),
                  ],
                  [Icons.send],
                );
              } else {
                Node.none;
              };
            let primary_button =
              if (agent_busy) {
                div(
                  ~attrs=[
                    clss(["send-button", "icon", "chat-message-stop-button"]),
                    Attr.on_click(stop_agent),
                    Attr.title(
                      "Stop — ignore the in-flight response (click only; no keyboard shortcut)",
                    ),
                  ],
                  [Icons.stop_square],
                );
              } else if (String.length(String.trim(current_text)) > 0) {
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
              };
            div(
              ~attrs=[clss(["chat-input-bottom-bar"])],
              [
                div(
                  ~attrs=[clss(["chat-input-bottom-bar-left"])],
                  [reasoning_effort_dropup, change_model_button],
                ),
                div(
                  ~attrs=[clss(["chat-input-bottom-bar-right"])],
                  [queue_button, primary_button],
                ),
              ],
            );
          },
        ],
      ),
    ],
  );
};
