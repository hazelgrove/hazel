open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Js_of_ocaml;

open JsUtil;

// Shared bottom bar component for Chat and Workbench views
let view =
    (
      ~globals as _: Globals.t,
      ~agent_model: Agent.Agent.Model.t,
      ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
      ~signal: Editors.View.signal => Effect.t(unit),
      ~chunked_chat: Agent.ChunkedUIChat.Model.t,
      ~current_chat_id: Id.t,
    )
    : Node.t => {
  let chat_system = agent_model.chat_system;
  let current_chat =
    Agent.ChatSystem.Utils.find_chat(current_chat_id, chat_system);

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
      let user_message = Agent.Message.Utils.mk_user_message(message_content);
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

  // Handler functions for icon buttons
  let switch_to_prompt = _ => {
    Effect.Many([
      agent_inject(
        Agent.Agent.Update.Action.ChatSystemAction(
          Agent.ChatSystem.Update.Action.ChatAction(
            Agent.Chat.Update.Action.SwitchView(Agent.Chat.Model.Prompt),
            current_chat_id,
          ),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  let switch_to_dev_notes = _ => {
    Effect.Many([
      agent_inject(
        Agent.Agent.Update.Action.ChatSystemAction(
          Agent.ChatSystem.Update.Action.ChatAction(
            Agent.Chat.Update.Action.SwitchView(
              Agent.Chat.Model.DeveloperNotes,
            ),
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
        Agent.Agent.Update.Action.ChatSystemAction(
          Agent.ChatSystem.Update.Action.ChatAction(
            Agent.Chat.Update.Action.SwitchView(
              Agent.Chat.Model.AgentEditorView,
            ),
            current_chat_id,
          ),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  // Export OpenRouter messages function
  let export_chat = _ => {
    let messages = Agent.Chat.Utils.get(current_chat);
    let api_messages = Agent.Chat.Utils.api_messages_of_messages(messages);
    let messages_json =
      `List(
        List.map(OpenRouter.Message.Utils.json_of_message, api_messages),
      );
    let filename =
      StringUtil.sanitize_filename(current_chat.title)
      ++ "_openrouter_"
      ++ string_of_float(current_chat.created_at);
    download_json(filename, messages_json);
    Effect.Stop_propagation;
  };

  // Copy chat as human-readable text function with toast notification
  let copy_chat = _ => {
    let messages = Agent.Chat.Utils.get(current_chat);
    let user_facing_messages =
      List.filter(
        (msg: Agent.Message.Model.t) =>
          switch (msg.role) {
          | Agent.Message.Model.System(_) => false
          | _ => true
          },
        messages,
      );
    let format_message = (msg: Agent.Message.Model.t): string => {
      switch (msg.role) {
      | Agent.Message.Model.User => "User: " ++ msg.content ++ "\n\n"
      | Agent.Message.Model.Agent => "LLM: " ++ msg.content ++ "\n\n"
      | Agent.Message.Model.ToolResult(tool_call, success) =>
        "Tool Call: "
        ++ tool_call.name
        ++ " "
        ++ (success ? "[success]" : "[failure]")
        ++ "\n\n"
      | Agent.Message.Model.System(_) => ""
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

  // Input area at bottom with buttons above
  div(
    ~attrs=[clss(["chat-input-container"])],
    [
      // Action buttons row - above input, left side buttons and right side export
      div(
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
              // Dev Notes button
              if (chunked_chat.developer_notes != "") {
                div(
                  ~attrs=[
                    clss(["chat-action-button", "icon"]),
                    Attr.on_click(switch_to_dev_notes),
                    Attr.title("View Developer Notes"),
                  ],
                  [Icons.wrench],
                );
              } else {
                div(~attrs=[], []);
              },
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
          // Right side export and copy buttons
          div(
            ~attrs=[clss(["chat-action-buttons-right"])],
            [
              div(
                ~attrs=[
                  clss(["chat-action-button", "icon"]),
                  Attr.on_click(export_chat),
                  Attr.title("Export OpenRouter Messages (JSON)"),
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
                | Some("Enter") when !shift_pressed =>
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
  );
};
