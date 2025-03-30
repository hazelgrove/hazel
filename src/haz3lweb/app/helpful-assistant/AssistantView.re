module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Js_of_ocaml;

type selection =
  | MakeActive(Selection.t);

type event =
  | MakeActive(ScratchMode.Selection.t);

let llm_toggle = (~globals: Globals.t): Node.t => {
  let tooltip = "Toggle Manual LLM";
  let toggle_llm = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Assistant(ToggleLLM))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["llm-button"])],
    [
      text("Manual LLM: "),
      Widgets.toggle(
        ~tooltip,
        "🔎",
        globals.settings.assistant.llm,
        toggle_llm,
      ),
    ],
  );
};

let lsp_toggle = (~globals: Globals.t): Node.t => {
  let tooltip = "Toggle Manual LSP";
  let toggle_lsp = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Assistant(ToggleLSP))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["lsp-button"])],
    [
      text("Manual LSP: "),
      Widgets.toggle(
        ~tooltip,
        "💬",
        globals.settings.assistant.lsp,
        toggle_lsp,
      ),
    ],
  );
};

let begin_chat_button = (~globals: Globals.t, ~inject): Node.t => {
  let tooltip = "New Chat";
  let begin_chat = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Assistant(UpdateChatStatus))),
      inject(Assistant.Update.NewChat),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["chat-button"]), Attr.on_click(begin_chat)],
    [Widgets.button_named(~tooltip, None, begin_chat)],
  );
};

let resume_chat_button = (~globals: Globals.t): Node.t => {
  let tooltip = "Confirm and Chat";
  let resume_chat = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Assistant(UpdateChatStatus))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["chat-button"]), Attr.on_click(resume_chat)],
    [Widgets.button_named(~tooltip, None, resume_chat)],
  );
};

let end_chat_button = (~globals: Globals.t): Node.t => {
  let tooltip = "Settings";
  let end_chat = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Assistant(UpdateChatStatus))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["chat-button"]), Attr.on_click(end_chat)],
    [
      Widgets.button_named(~tooltip, None, _ => Virtual_dom.Vdom.Effect.Ignore),
    ],
  );
};

let new_chat_button = (~globals: Globals.t, ~inject): Node.t => {
  let tooltip = "New Chat";
  let new_chat = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject(Assistant.Update.NewChat),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["add-button"]), Attr.on_click(new_chat)],
    [
      Widgets.button(~tooltip, Icons.add, _ => Virtual_dom.Vdom.Effect.Ignore),
    ],
  );
};

let history_button = (~globals: Globals.t, ~inject): Node.t => {
  let tooltip = "Past Chats";
  let history = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject(Assistant.Update.History),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["history-button"]), Attr.on_click(history)],
    [
      Widgets.button(~tooltip, Icons.history, _ =>
        Virtual_dom.Vdom.Effect.Ignore
      ),
    ],
  );
};

let select_llm = (~inject, ~assistantModel: Assistant.Model.t): Node.t => {
  let handle_change = (event, _) => {
    let value = Js.to_string(Js.Unsafe.coerce(event)##.target##.value);
    let selected_llm =
      switch (value) {
      | "Gemini_Flash_Lite_2_0" => OpenRouter.Gemini_Flash_Lite_2_0
      | "Gemini_Experimental_1206" => OpenRouter.Gemini_Experimental_1206
      | "Llama_3_1_Nemo" => OpenRouter.Llama_3_1_Nemo
      | "DeepSeek_V3" => OpenRouter.DeepSeek_V3
      | _ => OpenRouter.Gemini_Flash_Lite_2_0
      };
    Virtual_dom.Vdom.Effect.Many([
      inject(Assistant.Update.SelectLLM(selected_llm)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };

  // Helper function to determine if an option should be selected
  let is_selected =
      (llm: OpenRouter.chat_models, current_llm: OpenRouter.chat_models) => {
    llm == current_llm;
  };

  div(
    ~attrs=[clss(["llm-selector"])],
    [
      label(~attrs=[clss(["llm-label"])], [text("Select LLM Model: ")]),
      select(
        ~attrs=[Attr.on_change(handle_change), clss(["llm-dropdown"])],
        [
          option(
            ~attrs=[
              Attr.value("Gemini_Flash_Lite_2_0"),
              is_selected(
                OpenRouter.Gemini_Flash_Lite_2_0,
                assistantModel.llm,
              )
                ? Attr.selected : Attr.empty,
            ],
            [text("Gemini Flash Lite 2.0")],
          ),
          option(
            ~attrs=[
              Attr.value("Gemini_Experimental_1206"),
              is_selected(
                OpenRouter.Gemini_Experimental_1206,
                assistantModel.llm,
              )
                ? Attr.selected : Attr.empty,
            ],
            [text("Gemini Experimental 1206")],
          ),
          option(
            ~attrs=[
              Attr.value("Llama_3_1_Nemo"),
              is_selected(OpenRouter.Llama_3_1_Nemo, assistantModel.llm)
                ? Attr.selected : Attr.empty,
            ],
            [text("Llama 3.1 Nemotron 70B")],
          ),
          option(
            ~attrs=[
              Attr.value("DeepSeek_V3"),
              is_selected(OpenRouter.DeepSeek_V3, assistantModel.llm)
                ? Attr.selected : Attr.empty,
            ],
            [text("DeepSeek V3")],
          ),
        ],
      ),
    ],
  );
};

let settings_box = (~globals: Globals.t, ~inject): Node.t => {
  div(
    ~attrs=[clss(["settings-box"])],
    [
      // llm_toggle(~globals),
      // lsp_toggle(~globals),
      // begin_chat_button(~globals, ~inject),
      resume_chat_button(~globals),
    ],
  );
};

let api_input =
    (
      ~signal,
      ~inject,
      ~assistantModel: Assistant.Model.t,
      ~settings: AssistantSettings.t,
    )
    : Node.t => {
  let handle_submission = (api_key: string) => {
    JsUtil.log("Your API key for this session has been set: " ++ api_key);
    Virtual_dom.Vdom.Effect.Many([
      inject(Assistant.Update.SetKey(api_key)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };

  let toggle_visibility = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject(Assistant.Update.ToggleAPIVisibility),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);

  let submit_key = _ => {
    let message =
      Js.Opt.case(
        Dom_html.document##getElementById(Js.string("api-input")),
        () => "",
        el =>
          switch (Js.Unsafe.coerce(el)) {
          | input => Js.to_string(input##.value)
          },
      );
    Js.Opt.case(
      Dom_html.document##getElementById(Js.string("api-input")),
      () => (),
      el => Js.Unsafe.coerce(el)##.value := Js.string(""),
    );
    handle_submission(message);
  };

  let handle_keydown = event => {
    let key = Js.Optdef.to_option(Js.Unsafe.get(event, "key"));
    switch (key) {
    | Some("Enter") => submit_key()
    | _ => Virtual_dom.Vdom.Effect.Ignore
    };
  };

  div(
    ~attrs=[clss(["api-key-container"])],
    [
      input(
        ~attrs=[
          Attr.id("api-input"),
          Attr.placeholder("Enter your API key..."),
          Attr.type_("password"),
          Attr.property("autocomplete", Js.Unsafe.inject("off")),
          Attr.on_focus(_ =>
            signal(MakeActive(ScratchMode.Selection.TextBox))
          ),
          Attr.on_keydown(handle_keydown),
          clss(["api-input"]),
          Attr.on_copy(_ => {Effect.Stop_propagation}),
          Attr.on_paste(_ => {Effect.Stop_propagation}),
          Attr.on_cut(_ => {Effect.Stop_propagation}),
        ],
        (),
      ),
      div(
        ~attrs=[clss(["chat-button"]), Attr.on_click(submit_key)],
        [Widgets.button_named(~tooltip="Update API Key", None, submit_key)],
      ),
      div(
        ~attrs=[clss(["chat-button"]), Attr.on_click(toggle_visibility)],
        [
          Widgets.button_named(
            ~tooltip="Show/Hide Key",
            None,
            toggle_visibility,
          ),
        ],
      ),
      div(~attrs=[clss(["text-display"])], [text("Current API Key:\n")]),
      div(
        ~attrs=[clss(["api-key-display"]), Attr.id("api-key-display")],
        [
          text(
            switch (Store.Generic.load("API")) {
            | Some(key) when String.length(key) > 0 =>
              assistantModel.show_api_key
                ? key : String.make(String.length(key), '*')
            | _ => "No API key set"
            },
          ),
        ],
      ),
    ],
  );
};

let message_input =
    (
      ~signal,
      ~inject,
      ~assistantModel: Assistant.Model.t,
      ~settings: AssistantSettings.t,
    )
    : Node.t => {
  let handle_send = (message: string) => {
    let message: Assistant.Model.message = {
      party: LS,
      code: None,
      content: message,
      collapsed: String.length(message) >= 200,
    };
    JsUtil.log("Message sent: " ++ message.content);
    Virtual_dom.Vdom.Effect.Many([
      inject(Assistant.Update.SendTextMessage(message)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };
  let (past_chats, curr_chat) =
    Assistant.Update.get_mode_info(settings.mode, assistantModel);
  let curr_messages = Id.Map.find(curr_chat.id, past_chats).messages;
  let send_message = _ => {
    let message =
      Js.Opt.case(
        Dom_html.document##getElementById(Js.string("message-input")),
        () => "",
        el =>
          switch (Js.Unsafe.coerce(el)) {
          | input => Js.to_string(input##.value)
          },
      );
    Js.Opt.case(
      Dom_html.document##getElementById(Js.string("message-input")),
      () => (),
      el => Js.Unsafe.coerce(el)##.value := Js.string(""),
    );
    handle_send(message);
  };
  let handle_keydown = event => {
    let key = Js.Optdef.to_option(Js.Unsafe.get(event, "key"));
    switch (key, ListUtil.last_opt(curr_messages)) {
    | (_, Some({party: LLM, code: None, content: "...", collapsed: false})) => Virtual_dom.Vdom.Effect.Ignore
    | (Some("Enter"), _) => send_message()
    | _ => Virtual_dom.Vdom.Effect.Ignore
    };
  };
  div(
    ~attrs=[clss(["input-container"])],
    [
      input(
        ~attrs=[
          Attr.id("message-input"),
          Attr.placeholder(
            switch (settings.mode) {
            | HazelTutor => "Ask a question about Hazel or anything..."
            | CodeSuggestion => "Followup with a question..."
            | TaskCompletion => "Type a task completion..."
            },
          ),
          Attr.type_("text"),
          Attr.property("autocomplete", Js.Unsafe.inject("off")),
          Attr.on_focus(_ =>
            signal(MakeActive(ScratchMode.Selection.TextBox))
          ),
          Attr.on_copy(_ => {Effect.Stop_propagation}),
          Attr.on_paste(_ => {Effect.Stop_propagation}),
          Attr.on_cut(_ => {Effect.Stop_propagation}),
          Attr.on_keydown(handle_keydown),
          clss(["message-input"]),
        ],
        (),
      ),
      switch (ListUtil.last_opt(curr_messages)) {
      | Some({party: LLM, code: None, content: "...", collapsed: false}) =>
        div(
          ~attrs=[
            clss(["send-button-disabled", "icon"]),
            Attr.title("Submitting Message Disabled"),
          ],
          [Icons.send],
        )
      | _ =>
        div(
          ~attrs=[
            clss(["send-button", "icon"]),
            Attr.on_click(send_message),
            Attr.title("Submit Message"),
          ],
          [Icons.send],
        )
      },
    ],
  );
};

// For aesthetic purposes only :)
let loading_dots = () => {
  div(
    ~attrs=[clss(["loading-dots"])],
    [
      div(~attrs=[clss(["dot", "dot1"])], []),
      div(~attrs=[clss(["dot", "dot2"])], []),
      div(~attrs=[clss(["dot", "dot3"])], []),
    ],
  );
};

let message_display =
    (
      ~inject,
      ~globals: Globals.t,
      ~assistantModel: Assistant.Model.t,
      ~settings: AssistantSettings.t,
    )
    : Node.t => {
  let toggle_collapse = index => {
    // Create an action to toggle the collapsed state of a specific message
    Virtual_dom.Vdom.Effect.Many([
      inject(Assistant.Update.ToggleCollapse(index)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };
  let (past_chats, curr_chat) =
    Assistant.Update.get_mode_info(settings.mode, assistantModel);
  let curr_messages = Id.Map.find(curr_chat.id, past_chats).messages;
  let message_nodes =
    List.flatten(
      List.mapi(
        (index: int, message: Assistant.Model.message) => {
          switch (message.code) {
          | Some((sketch, tileId)) =>
            message.content == "..." && message.party == LLM
              ? [loading_dots()]
              : [
                div(
                  ~attrs=[
                    clss([
                      "message-container",
                      switch (message.party) {
                      | LS => "ls"
                      | LLM => "llm"
                      | System => "system"
                      },
                    ]),
                  ],
                  [
                    div(
                      ~attrs=[clss(["message-identifier"])],
                      [
                        text(
                          switch (message.party) {
                          | LS => "User"
                          | LLM => "Assistant"
                          | System => "System"
                          },
                        ),
                      ],
                    ),
                    div(
                      ~attrs=[
                        clss([
                          switch (message.party) {
                          | LS => "ls-message"
                          | LLM => "llm-message"
                          | System => "system-message"
                          },
                        ]),
                        Attr.on_copy(_ => {Effect.Stop_propagation}),
                        Attr.on_paste(_ => {Effect.Stop_propagation}),
                        Attr.on_cut(_ => {Effect.Stop_propagation}),
                      ],
                      [
                        message.collapsed
                        && String.length(message.content) >= 200
                          ? text(
                              String.concat(
                                "",
                                [
                                  String.sub(
                                    message.content,
                                    0,
                                    min(String.length(message.content), 200),
                                  ),
                                  "...",
                                ],
                              ),
                            )
                          : text(message.content),
                        div(
                          ~attrs=[
                            clss(["collapse-indicator"]),
                            Attr.on_click(_ => toggle_collapse(index)),
                            String.length(message.content) >= 200
                              ? Attr.empty : Attr.hidden,
                          ],
                          [
                            text(
                              message.collapsed
                                ? "▼ Show more" : "▲ Show less",
                            ),
                          ],
                        ),
                      ],
                    ),
                    message.party == LLM && tileId != None
                      ? div(
                          ~attrs=[
                            clss(["resuggest-button"]),
                            Attr.on_click(_ =>
                              Virtual_dom.Vdom.Effect.Many([
                                inject(
                                  Assistant.Update.Resuggest(
                                    message.content,
                                    Option.get(tileId),
                                  ),
                                ),
                                Virtual_dom.Vdom.Effect.Stop_propagation,
                              ])
                            ),
                            Attr.title("Resuggest"),
                          ],
                          [text("resuggest")],
                        )
                      : None,
                  ],
                ),
                div(
                  ~attrs=[
                    clss([
                      "message-container",
                      "example",
                      message.party == LLM ? "llm" : "ls",
                    ]),
                  ],
                  [
                    CellEditor.View.view(
                      ~globals,
                      ~signal=_ => Ui_effect.Ignore,
                      ~inject=_ => Ui_effect.Ignore,
                      ~selected=None,
                      ~caption=None,
                      ~locked=true,
                      message.party == LLM
                        ? {
                          sketch
                          |> Zipper.unzip
                          |> Editor.Model.mk
                          |> CellEditor.Model.mk;
                        }
                        : {
                          sketch
                          |> Zipper.unzip
                          |> Editor.Model.mk
                          |> CellEditor.Model.mk
                          |> CellEditor.Update.calculate(
                               ~settings=globals.settings.core,
                               ~is_edited=true,
                               ~stitch=x => x,
                               ~queue_worker=None,
                             );
                        },
                    ),
                  ],
                ),
              ]
          | None =>
            message.content == "..." && message.party == LLM
              ? [loading_dots()]
              : [
                div(
                  ~attrs=[
                    clss([
                      "message-container",
                      switch (message.party) {
                      | LS => "ls"
                      | LLM => "llm"
                      | System => "system"
                      },
                    ]),
                  ],
                  [
                    div(
                      ~attrs=[clss(["message-identifier"])],
                      [
                        text(
                          switch (message.party) {
                          | LS => "User"
                          | LLM => "Assistant"
                          | System => "System"
                          },
                        ),
                      ],
                    ),
                    div(
                      ~attrs=[
                        clss([
                          switch (message.party) {
                          | LS => "ls-message"
                          | LLM => "llm-message"
                          | System => "system-message"
                          },
                        ]),
                        Attr.on_copy(_ => {Effect.Stop_propagation}),
                        Attr.on_paste(_ => {Effect.Stop_propagation}),
                        Attr.on_cut(_ => {Effect.Stop_propagation}),
                      ],
                      [
                        message.collapsed
                        && String.length(message.content) >= 200
                          ? text(
                              String.concat(
                                "",
                                [
                                  String.sub(
                                    message.content,
                                    0,
                                    min(String.length(message.content), 200),
                                  ),
                                  "...",
                                ],
                              ),
                            )
                          : text(message.content),
                        div(
                          ~attrs=[
                            clss(["collapse-indicator"]),
                            Attr.on_click(_ => toggle_collapse(index)),
                            String.length(message.content) >= 200
                              ? Attr.empty : Attr.hidden,
                          ],
                          [
                            text(
                              message.collapsed
                                ? "▼ Show more" : "▲ Show less",
                            ),
                          ],
                        ),
                      ],
                    ),
                  ],
                ),
              ]
          }
        },
        curr_messages,
      ),
    );
  div(~attrs=[clss(["message-display-container"])], message_nodes);
};

let mode_buttons = (~globals: Globals.t): Node.t => {
  let mode_button = (mode: AssistantSettings.mode, label: string) => {
    let switch_mode = _ =>
      Virtual_dom.Vdom.Effect.Many([
        globals.inject_global(Set(Assistant(SwitchMode(mode)))),
        Virtual_dom.Vdom.Effect.Stop_propagation,
      ]);
    div(
      ~attrs=[
        clss([
          "mode-button",
          globals.settings.assistant.mode == mode ? "active" : "",
        ]),
        Attr.on_click(switch_mode),
      ],
      [text(label)],
    );
  };

  div(
    ~attrs=[clss(["mode-buttons"])],
    [
      mode_button(HazelTutor, "Tutor"),
      mode_button(CodeSuggestion, "Suggest"),
      mode_button(TaskCompletion, "Compose"),
    ],
  );
};

let history_menu =
    (
      ~assistantModel: Assistant.Model.t,
      ~settings: AssistantSettings.t,
      ~inject,
    )
    : Node.t => {
  let (past_chats, curr_chat) =
    Assistant.Update.get_mode_info(settings.mode, assistantModel);
  let chrono_past_chats = Assistant.Model.sorted_chats(past_chats);
  div(
    ~attrs=[clss(["history-menu"])],
    [
      div(
        ~attrs=[clss(["history-menu-header"])],
        [
          switch (settings.mode) {
          | HazelTutor => text("Tutor History")
          | CodeSuggestion => text("Suggestion History")
          | TaskCompletion => text("Task History")
          },
        ],
      ),
      div(
        ~attrs=[clss(["history-menu-list"])],
        List.map(
          (chat: Assistant.Model.chat) =>
            div(
              ~attrs=[
                chat.id == curr_chat.id
                  ? clss(["history-menu-item", "active"])
                  : clss(["history-menu-item"]),
                Attr.on_click(e => {
                  let target = Js.Unsafe.coerce(e)##.target;
                  let contains_button =
                    Js.to_bool(target##.classList##contains("button"))
                    || Js.to_bool(
                         target##.parentElement##.classList##contains(
                           "button",
                         ),
                       );
                  if (!contains_button) {
                    Virtual_dom.Vdom.Effect.Many([
                      inject(Assistant.Update.SwitchChat(chat.id)),
                      Virtual_dom.Vdom.Effect.Stop_propagation,
                    ]);
                  } else {
                    Virtual_dom.Vdom.Effect.Stop_propagation;
                  };
                }),
              ],
              [
                div(
                  ~attrs=[clss(["history-menu-item-content"])],
                  [
                    text(chat.descriptor == "" ? "New chat" : chat.descriptor),
                  ],
                ),
                div(
                  ~attrs=[clss(["history-menu-item-actions"])],
                  [
                    div(
                      ~attrs=[clss(["history-menu-item-time"])],
                      [
                        text(AssistantUtil.format_time_diff(chat.timestamp)),
                      ],
                    ),
                    div(
                      ~attrs=[
                        clss(["delete-chat-button"]),
                        Attr.on_click(_ =>
                          Virtual_dom.Vdom.Effect.Many([
                            inject(Assistant.Update.DeleteChat(chat.id)),
                            Virtual_dom.Vdom.Effect.Stop_propagation,
                          ])
                        ),
                      ],
                      [
                        Widgets.button(~tooltip="Delete chat", Icons.trash, _ =>
                          Virtual_dom.Vdom.Effect.Ignore
                        ),
                      ],
                    ),
                  ],
                ),
              ],
            ),
          chrono_past_chats,
        ),
      ),
    ],
  );
};

let view =
    (
      ~globals: Globals.t,
      ~signal,
      ~inject,
      ~assistantModel: Assistant.Model.t,
    ) => {
  div(
    ~attrs=[Attr.id("side-bar")],
    [
      div(
        ~attrs=[Attr.id("assistant")],
        [
          div(
            ~attrs=[clss(["header"])],
            [
              div(
                ~attrs=[clss(["header-content"])],
                [
                  globals.settings.assistant.ongoing_chat
                    ? mode_buttons(~globals) : text("Assistant Settings"),
                  div(
                    ~attrs=[clss(["header-actions"])],
                    [
                      globals.settings.assistant.ongoing_chat
                        ? history_button(~globals, ~inject) : None,
                      globals.settings.assistant.ongoing_chat
                        ? new_chat_button(~globals, ~inject) : None,
                      globals.settings.assistant.ongoing_chat
                        ? end_chat_button(~globals) : None,
                    ],
                  ),
                ],
              ),
            ],
          ),
          globals.settings.assistant.ongoing_chat
            ? message_display(
                ~inject,
                ~globals,
                ~assistantModel,
                ~settings=globals.settings.assistant,
              )
            : None,
          globals.settings.assistant.ongoing_chat
            ? message_input(
                ~signal,
                ~inject,
                ~assistantModel,
                ~settings=globals.settings.assistant,
              )
            : None,
          globals.settings.assistant.ongoing_chat
            ? None
            : api_input(
                ~signal,
                ~inject,
                ~assistantModel,
                ~settings=globals.settings.assistant,
              ),
          globals.settings.assistant.ongoing_chat
            ? None : select_llm(~inject, ~assistantModel),
          globals.settings.assistant.ongoing_chat
            ? None : settings_box(~globals, ~inject),
          globals.settings.assistant.ongoing_chat
          && assistantModel.show_history
            ? history_menu(
                ~assistantModel,
                ~settings=globals.settings.assistant,
                ~inject,
              )
            : None,
        ],
      ),
    ],
  );
};
