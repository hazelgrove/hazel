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
  let chunked_chat = Agent.ChunkedUIChat.Utils.mk(current_chat);

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
  // Note: Branch navigation may need to be reworked for chunked UI model
  // For now, we'll skip it or implement it differently if needed

  // Render a chunk from the chunked UI model
  let num_chunks = List.length(chunked_chat.log);
  let render_chunk = (index: int, chunk: Agent.ChunkedUIChat.Model.chunk) => {
    switch (chunk) {
    | Agent.ChunkedUIChat.Model.UserMessage(user_msg) =>
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
                      Attr.value(user_msg.content),
                      Attr.property("autocomplete", Js.Unsafe.inject("off")),
                      Attr.on_focus(_ => {
                        Js.Opt.iter(
                          Dom_html.document##getElementById(
                            Js.string(unique_id),
                          ),
                          el => {
                            let textarea = Js.Unsafe.coerce(el);
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
            ],
          ),
        ],
      );
    | Agent.ChunkedUIChat.Model.AgentResponseChunk(agent_chunk) =>
      // Agent response chunk - display all content in a single block
      let is_last_chunk = index == num_chunks - 1;
      let all_content_empty =
        List.for_all(
          (content: string) => content == "",
          agent_chunk.agent_content,
        );
      let agent_content_display =
        if (all_content_empty && is_last_chunk) {
          // Show loading dots if last chunk and all content is empty
          div(
            ~attrs=[clss(["agent-message-loading-dots"])],
            [
              div(~attrs=[clss(["dot", "dot1"])], []),
              div(~attrs=[clss(["dot", "dot2"])], []),
              div(~attrs=[clss(["dot"])], []),
            ],
          );
        } else {
          // Join all agent content strings with double line breaks
          let combined_content =
            agent_chunk.agent_content
            |> List.filter((s: string) => s != "")
            |> String.concat("\n\n");
          div(~attrs=[clss(["agent-message"])], [text(combined_content)]);
        };

      // Render tool calls - separated by double line breaks
      let tool_calls_display =
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
          div(
            ~attrs=[clss(["agent-message-wrapper"])],
            [agent_content_display, tool_calls_display],
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

  div(
    ~attrs=[clss(["chat-messages-view"])],
    [
      // Prompt and Developer Notes at top
      if (chunked_chat.prompt != "") {
        div(
          ~attrs=[clss(["system-message-container", "prompt-display"])],
          [
            div(
              ~attrs=[clss(["system-message"])],
              [text(chunked_chat.prompt)],
            ),
          ],
        );
      } else {
        div(~attrs=[], []);
      },
      if (chunked_chat.developer_notes != "") {
        div(
          ~attrs=[
            clss(["system-message-container", "developer-notes-display"]),
          ],
          [
            div(
              ~attrs=[clss(["system-message"])],
              [text(chunked_chat.developer_notes)],
            ),
          ],
        );
      } else {
        div(~attrs=[], []);
      },
      // Chunks display area
      div(
        ~attrs=[clss(["chat-messages-container"])],
        List.mapi(render_chunk, chunked_chat.log),
      ),
      // Editor view and static errors at bottom
      if (chunked_chat.editor_view != "") {
        div(
          ~attrs=[clss(["system-message-container", "editor-view-display"])],
          [
            div(
              ~attrs=[clss(["system-message"])],
              [text(chunked_chat.editor_view)],
            ),
          ],
        );
      } else {
        div(~attrs=[], []);
      },
      if (chunked_chat.static_errors != "") {
        div(
          ~attrs=[
            clss(["system-message-container", "static-errors-display"]),
          ],
          [
            div(
              ~attrs=[clss(["system-message"])],
              [text(chunked_chat.static_errors)],
            ),
          ],
        );
      } else {
        div(~attrs=[], []);
      },
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
