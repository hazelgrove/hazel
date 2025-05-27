module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Js_of_ocaml;

module Update = AssistantUpdate;

module Model = AssistantModel;

type selection =
  | MakeActive(Selection.t);

type event =
  | MakeActive(ScratchMode.Selection.t);

let resume_chat_button =
    (~inject_global: Globals.Action.t => Ui_effect.t(unit)): Node.t => {
  let tooltip = "Confirm and Chat";
  let resume_chat = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject_global(Set(Assistant(UpdateChatStatus))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["chat-button"]), Attr.on_click(resume_chat)],
    [Widgets.button_named(~tooltip, None, resume_chat)],
  );
};

let settings_button =
    (~inject_global: Globals.Action.t => Ui_effect.t(unit)): Node.t => {
  let tooltip = "Settings";
  let end_chat = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject_global(Set(Assistant(UpdateChatStatus))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["chat-button"]), Attr.on_click(end_chat)],
    [
      Widgets.button_named(~tooltip, None, _ => Virtual_dom.Vdom.Effect.Ignore),
    ],
  );
};

let new_chat_button = (~inject): Node.t => {
  let tooltip = "New Chat";
  let new_chat = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject(Update.ChatAction(NewChat)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["add-button"]), Attr.on_click(new_chat)],
    [
      Widgets.button(~tooltip, Icons.add, _ => Virtual_dom.Vdom.Effect.Ignore),
    ],
  );
};

let history_button =
    (~inject_global: Globals.Action.t => Ui_effect.t(unit)): Node.t => {
  let tooltip = "Past Chats";
  let history = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject_global(Set(Assistant(ToggleHistory))),
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

let select_llm =
    (
      ~inject_global: Globals.Action.t => Ui_effect.t(unit),
      ~settings: AssistantSettings.t,
    )
    : Node.t => {
  let handle_change = (event, _) => {
    let value = Js.to_string(Js.Unsafe.coerce(event)##.target##.value);
    Virtual_dom.Vdom.Effect.Many([
      inject_global(Set(Assistant(SetLLM(value)))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };

  div(
    ~attrs=[clss(["llm-selector"])],
    [
      label(~attrs=[clss(["llm-label"])], [text("Select LLM Model: ")]),
      select(
        ~attrs=[Attr.on_change(handle_change), clss(["llm-dropdown"])],
        List.map(
          (model: OpenRouter.model_info) =>
            option(
              ~attrs=[
                Attr.value(model.id),
                switch (Store.Generic.load("MODEL")) {
                | Some(current_model) when current_model == model.id => Attr.selected
                | _ => Attr.empty
                },
              ],
              [text(model.name)],
            ),
          settings.available_models,
        ),
      ),
    ],
  );
};

let settings_box =
    (~inject_global: Globals.Action.t => Ui_effect.t(unit)): Node.t => {
  div(
    ~attrs=[clss(["settings-box"])],
    [resume_chat_button(~inject_global)],
  );
};

let api_input =
    (
      ~inject_global: Globals.Action.t => Ui_effect.t(unit),
      ~signal,
      ~settings: AssistantSettings.t,
    )
    : Node.t => {
  let handle_submission = (api_key: string) => {
    Virtual_dom.Vdom.Effect.Many([
      inject_global(Set(Assistant(SetAPIKey(api_key)))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };

  let toggle_visibility = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject_global(Set(Assistant(ToggleAPIKeyVisibility))),
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
      div(~attrs=[clss(["title"])], [text("API Key")]),
      div(
        ~attrs=[clss(["assistant-info-container"])],
        [
          text("You can find or create an OpenRouter API key "),
          a(
            ~attrs=[
              Attr.href("https://openrouter.ai/settings/keys"),
              Attr.target("_blank"),
            ],
            [text("here")],
          ),
        ],
      ),
      input(
        ~attrs=[
          Attr.id("api-input"),
          Attr.placeholder("Enter your OpenRouter API key..."),
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
      div(~attrs=[clss(["text-display"])], [text("Current API Key:\n")]),
      div(
        ~attrs=[clss(["api-key-row"])],
        [
          div(
            ~attrs=[clss(["api-key-display"]), Attr.id("api-key-display")],
            [
              text(
                switch (Store.Generic.load("API")) {
                | Some(key) when String.length(key) > 0 =>
                  settings.show_api_key
                    ? key : String.make(String.length(key), '*')
                | _ => "No API key set"
                },
              ),
            ],
          ),
          div(
            ~attrs=[clss(["toggle-show-button"])],
            [
              Widgets.button(
                ~tooltip="Show/Hide Key",
                settings.show_api_key ? Icons.visible : Icons.invisible,
                toggle_visibility,
              ),
            ],
          ),
        ],
      ),
    ],
  );
};

let llm_model_id_input =
    (
      ~inject_global: Globals.Action.t => Ui_effect.t(unit),
      ~signal,
      ~settings: AssistantSettings.t,
    )
    : Node.t => {
  let format_price_per_million = (price: string): string => {
    // Convert string to float, multiply by 1000 to get per million tokens
    // The API provides price per 1K tokens
    switch (float_of_string_opt(price)) {
    | Some(p) =>
      let per_million = p *. 1000000.0;
      if (per_million == 0.0) {
        "Free";
      } else {
        "$" ++ Printf.sprintf("%.4f", per_million);
      };
    | None => "Unknown"
    };
  };

  let handle_submission = (llm_model: string) => {
    Virtual_dom.Vdom.Effect.Many([
      inject_global(Set(Assistant(SetLLM(llm_model)))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };

  let submit_key = _ => {
    let message =
      Js.Opt.case(
        Dom_html.document##getElementById(Js.string("llm-model-id-input")),
        () => "",
        el =>
          switch (Js.Unsafe.coerce(el)) {
          | input => Js.to_string(input##.value)
          },
      );
    Js.Opt.case(
      Dom_html.document##getElementById(Js.string("llm-model-id-input")),
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
      div(~attrs=[clss(["title"])], [text("Model Selection")]),
      div(
        ~attrs=[clss(["assistant-info-container"])],
        [
          text("You can find a comprehensive list of OpenRouter models "),
          a(
            ~attrs=[
              Attr.href("https://openrouter.ai/models"),
              Attr.target("_blank"),
            ],
            [text("here")],
          ),
        ],
      ),
      select_llm(~inject_global, ~settings),
      div(
        ~attrs=[clss(["llm-label"])],
        [text("Or Enter Model ID Manually:")],
      ),
      input(
        ~attrs=[
          Attr.id("llm-model-id-input"),
          Attr.placeholder(
            "Enter the ID of the OpenRouter model you wish to use...",
          ),
          Attr.type_("text"),
          Attr.property("autocomplete", Js.Unsafe.inject("off")),
          Attr.on_focus(_ =>
            signal(MakeActive(ScratchMode.Selection.TextBox))
          ),
          Attr.on_keydown(handle_keydown),
          clss(["llm-model-id-input"]),
          Attr.on_copy(_ => {Effect.Stop_propagation}),
          Attr.on_paste(_ => {Effect.Stop_propagation}),
          Attr.on_cut(_ => {Effect.Stop_propagation}),
        ],
        (),
      ),
      div(
        ~attrs=[clss(["chat-button"]), Attr.on_click(submit_key)],
        [Widgets.button_named(~tooltip="Update Model ID", None, submit_key)],
      ),
      div(~attrs=[clss(["text-display"])], [text("Current Model ID:\n")]),
      div(
        ~attrs=[clss(["api-key-display"]), Attr.id("api-key-display")],
        [
          text(
            switch (Store.Generic.load("MODEL")) {
            | Some(model_id) when String.length(model_id) > 0 => model_id
            | _ => "No model ID set"
            },
          ),
        ],
      ),
      div(
        ~attrs=[clss(["text-display"])],
        [text("Model Pricing (per million tokens):\n")],
      ),
      div(
        ~attrs=[clss(["api-key-display"])],
        [
          text(
            switch (Store.Generic.load("MODEL")) {
            | Some(model_id) when String.length(model_id) > 0 =>
              let selected_model =
                List.find_opt(
                  (model: OpenRouter.model_info) => model.id == model_id,
                  settings.available_models,
                );
              switch (selected_model) {
              | Some(model) =>
                "Prompt: "
                ++ format_price_per_million(model.pricing.prompt)
                ++ " / Completion: "
                ++ format_price_per_million(model.pricing.completion)
              | None => "Pricing information not available"
              };
            | _ => "No model selected"
            },
          ),
        ],
      ),
    ],
  );
};

let message_input =
    (~signal, ~inject, ~model: Model.t, ~settings: AssistantSettings.t)
    : Node.t => {
  let handle_send = (content: string) => {
    let message: Model.message = {
      party: User,
      content,
      displayable_content: Update.parse_blocks(content),
      collapsed: String.length(content) >= 200,
    };
    JsUtil.log("Message sent: " ++ message.content);
    Virtual_dom.Vdom.Effect.Many([
      inject(Update.SendMessage(Basic(message))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };
  let (past_chats, curr_chat) = Update.get_mode_info(settings.mode, model);
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
    | (
        _,
        Some({
          party: LLM,
          content: "...",
          displayable_content: [Text("...")],
          collapsed: false,
        }),
      ) => Virtual_dom.Vdom.Effect.Ignore
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
            | HazelTutor => "Ask a question about Hazel (or anything)..."
            | CodeSuggestion => "Followup with a question about the agent's code suggestion..."
            | TaskCompletion => "Ask the agent to help clarify, plan, or write code..."
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
      | Some({
          party: LLM,
          content: "...",
          displayable_content: [Text("...")],
          collapsed: false,
        }) =>
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

// Indicates that the LLM is thinking/responding
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

let form_collapse_toggle =
    (
      ~message: Model.message,
      ~toggle_collapse,
      ~index: int,
      ~is_first: bool,
      ~is_last: bool,
    )
    : Node.t =>
  if (message.collapsed && String.length(message.content) >= 200 && is_first) {
    div(
      ~attrs=[
        clss(["collapse-indicator"]),
        Attr.on_click(_ => toggle_collapse(index)),
        String.length(message.content) >= 200 ? Attr.empty : Attr.hidden,
      ],
      [text("▼ Show more")],
    );
  } else if (!message.collapsed
             && String.length(message.content) >= 200
             && is_last) {
    div(
      ~attrs=[
        clss(["collapse-indicator"]),
        Attr.on_click(_ => toggle_collapse(index)),
        String.length(message.content) >= 200 ? Attr.empty : Attr.hidden,
      ],
      [text("▲ Show less")],
    );
  } else {
    None;
  };

let text_block =
    (
      ~message: Model.message,
      ~content: string,
      ~toggle_collapse,
      ~index: int,
      ~is_first: bool,
      ~is_last: bool,
    )
    : Node.t => {
  div(
    ~attrs=[
      clss([
        switch (message.party) {
        | User => "user-message"
        | LLM => "llm-message"
        | System(Prompt) => "system-prompt-message"
        | System(Error) => "system-error-message"
        },
      ]),
      Attr.on_copy(_ => {Effect.Stop_propagation}),
      Attr.on_paste(_ => {Effect.Stop_propagation}),
      Attr.on_cut(_ => {Effect.Stop_propagation}),
    ],
    [
      message.collapsed && String.length(message.content) >= 200
        ? text(
            String.concat(
              "",
              [
                String.sub(content, 0, min(String.length(content), 200)),
                "...",
              ],
            ),
          )
        : text(content),
      form_collapse_toggle(
        ~message,
        ~toggle_collapse,
        ~index,
        ~is_first,
        ~is_last,
      ),
    ],
  );
};

let code_block =
    (
      ~message: Model.message,
      ~content: string,
      ~toggle_collapse,
      ~index: int,
      ~is_first: bool,
      ~is_last: bool,
      ~globals: Globals.t,
    )
    : Node.t => {
  let zipper_of_response = Printer.zipper_of_string(content);
  let sketch =
    switch (zipper_of_response) {
    | Some(z) => Zipper.seg_for_view(z)
    | None =>
      print_endline("Failed to parse content into segment.\n");
      Zipper.seg_for_view(Zipper.init());
    };
  div(
    ~attrs=[
      clss([
        "example",
        switch (message.party) {
        | User => "user"
        | LLM => "llm"
        | System(Prompt) => "system-prompt"
        | System(Error) => "system-error"
        },
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
            sketch |> Zipper.unzip |> Editor.Model.mk |> CellEditor.Model.mk;
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
      form_collapse_toggle(
        ~message,
        ~toggle_collapse,
        ~index,
        ~is_first,
        ~is_last,
      ),
    ],
  );
};

let form_block =
    (
      ~message: Model.message,
      ~block: Model.block_kind,
      ~toggle_collapse,
      ~index: int,
      ~is_first: bool,
      ~is_last: bool,
      ~globals: Globals.t,
    )
    : Node.t => {
  print_endline(
    "Rendering block: "
    ++ (
      switch (block) {
      | Text(_) => "Text"
      | Code(_) => "Code"
      }
    ),
  );
  if (!message.collapsed || message.collapsed && is_first) {
    switch (block) {
    | Text(content) =>
      text_block(
        ~message,
        ~content,
        ~toggle_collapse,
        ~index,
        ~is_first,
        ~is_last,
      )
    | Code(content) =>
      code_block(
        ~message,
        ~content,
        ~toggle_collapse,
        ~index,
        ~is_first,
        ~is_last,
        ~globals,
      )
    };
  } else {
    None;
  };
};

let message_display =
    (
      ~globals: Globals.t,
      ~inject,
      ~model: Model.t,
      ~settings: AssistantSettings.t,
    )
    : Node.t => {
  let toggle_collapse = index => {
    // Create an action to toggle the collapsed state of a specific message
    Virtual_dom.Vdom.Effect.Many([
      inject(Update.ChatAction(CollapseMessage(index))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };
  let (past_chats, curr_chat) = Update.get_mode_info(settings.mode, model);
  let curr_messages = Id.Map.find(curr_chat.id, past_chats).messages;
  let message_nodes =
    List.flatten(
      List.mapi(
        (index: int, message: Model.message) => {
          message.content == "..." && message.party == LLM
            ? [loading_dots()]
            : [
              div(
                ~attrs=[
                  clss([
                    "message-container",
                    switch (message.party) {
                    | User => "user"
                    | LLM => "llm"
                    | System(Prompt) => "system-prompt"
                    | System(Error) => "system-error"
                    },
                  ]),
                ],
                [
                  div(
                    ~attrs=[clss(["message-identifier"])],
                    [
                      switch (message.party) {
                      | User => text("User")
                      | LLM =>
                        div(
                          ~attrs=[clss(["llm-identifier"])],
                          [Icons.hazelnut_agent, text("Assistant")],
                        )
                      | System(Prompt) =>
                        div(
                          ~attrs=[clss(["system-prompt-identifier"])],
                          [text("System")],
                        )
                      | System(Error) =>
                        div(
                          ~attrs=[clss(["system-error-identifier"])],
                          [text("System")],
                        )
                      },
                    ],
                  ),
                ]
                @ {
                  let parsed_blocks = message.displayable_content;
                  List.mapi(
                    (idx, block: Model.block_kind) =>
                      form_block(
                        ~message,
                        ~block,
                        ~toggle_collapse,
                        ~index,
                        ~is_first=idx == 0,
                        ~is_last=idx == List.length(parsed_blocks) - 1,
                        ~globals,
                      ),
                    parsed_blocks,
                  );
                },
              ),
            ]
        },
        curr_messages,
      ),
    );
  div(~attrs=[clss(["message-display-container"])], message_nodes);
};

let mode_buttons =
    (
      ~inject_global: Globals.Action.t => Ui_effect.t(unit),
      ~settings: AssistantSettings.t,
    )
    : Node.t => {
  let mode_button = (mode: AssistantSettings.mode, label: string) => {
    let switch_mode = _ =>
      Virtual_dom.Vdom.Effect.Many([
        inject_global(Set(Assistant(SwitchMode(mode)))),
        Virtual_dom.Vdom.Effect.Stop_propagation,
      ]);
    div(
      ~attrs=[
        clss(["mode-button", settings.mode == mode ? "active" : ""]),
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
    (~model: Model.t, ~settings: AssistantSettings.t, ~inject): Node.t => {
  let (past_chats, curr_chat) = Update.get_mode_info(settings.mode, model);
  let chronologically_sorted_past_chats = Model.sorted_chats(past_chats);
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
          (chat: Model.chat) =>
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
                      inject(Update.ChatAction(SwitchChat(chat.id))),
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
                            inject(Update.ChatAction(DeleteChat(chat.id))),
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
          chronologically_sorted_past_chats,
        ),
      ),
    ],
  );
};

let view =
    (
      ~globals: Globals.t,
      ~signal,
      ~inject: Update.t => Ui_effect.t(unit),
      ~model: Model.t,
    ) => {
  let settings = globals.settings.assistant;
  let inject_global = globals.inject_global;
  div(
    ~attrs=[Attr.id("side-bar"), Attr.tabindex(1)],
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
                  settings.ongoing_chat
                    ? mode_buttons(~inject_global, ~settings)
                    : div(
                        ~attrs=[clss(["main-title"])],
                        [text("Assistant Settings")],
                      ),
                  div(
                    ~attrs=[clss(["header-actions"])],
                    [
                      settings.ongoing_chat
                        ? history_button(~inject_global) : None,
                      settings.ongoing_chat ? new_chat_button(~inject) : None,
                      settings.ongoing_chat
                        ? settings_button(~inject_global)
                        : resume_chat_button(~inject_global),
                    ],
                  ),
                ],
              ),
            ],
          ),
          settings.ongoing_chat
            ? message_display(~globals, ~inject, ~model, ~settings) : None,
          settings.ongoing_chat
            ? message_input(~signal, ~inject, ~model, ~settings) : None,
          settings.ongoing_chat
            ? None : api_input(~inject_global, ~signal, ~settings),
          settings.ongoing_chat
            ? None : llm_model_id_input(~inject_global, ~signal, ~settings),
          settings.ongoing_chat ? None : settings_box(~inject_global),
          settings.ongoing_chat && settings.show_history
            ? history_menu(~model, ~settings, ~inject) : None,
        ],
      ),
    ],
  );
};
