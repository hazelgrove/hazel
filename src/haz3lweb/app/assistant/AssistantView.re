module Sexp = Sexplib.Sexp;
open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Util.OptUtil.Syntax;
open Haz3lcore;

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
      inject(AssistantModel.Update.NewChat),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["chat-button"]), Attr.on_click(begin_chat)],
    [Widgets.button_named(~tooltip, None, begin_chat)],
  );
};

let resume_chat_button = (~globals: Globals.t, ~inject): Node.t => {
  let tooltip = "Previous Chat";
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

let req_button = (~globals: Globals.t, ~inject): Node.t => {
  let tooltip = "??";
  let send_sketch = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject(AssistantModel.Update.SendSketch),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["chat-button"]), Attr.on_click(send_sketch)],
    [Widgets.button_named(~tooltip, None, send_sketch)],
  );
};

let end_chat_button = (~globals: Globals.t, ~inject): Node.t => {
  let tooltip = "End Chat";
  let end_chat = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(Assistant(UpdateChatStatus))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["chat-button"]), Attr.on_click(end_chat)],
    [Widgets.button_named(~tooltip, None, end_chat)],
  );
};

let select_llm =
    (
      ~signal,
      ~inject,
      ~globals: Globals.t,
      ~assistantModel: AssistantModel.Model.t,
    )
    : Node.t => {
  let handle_change = (event, _) => {
    let value = Js.to_string(Js.Unsafe.coerce(event)##.target##.value);
    let selected_llm =
      switch (value) {
      | "Gemini Flash Lite 2.0" => OpenRouter.Gemini_Flash_Lite
      | "Llama 3.1 Nemotron 70B" => OpenRouter.Llama_3_1_Nemo
      | _ => OpenRouter.Gemini_Flash_Lite
      };
    Virtual_dom.Vdom.Effect.Many([
      inject(AssistantModel.Update.SelectLLM(selected_llm)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };

  div(
    ~attrs=[clss(["llm-selector"])],
    [
      label(~attrs=[clss(["llm-label"])], [text("Select LLM Model: ")]),
      select(
        ~attrs=[Attr.on_change(handle_change), clss(["llm-dropdown"])],
        [
          option(
            ~attrs=[Attr.value("Gemini_Flash_Lite")],
            [text("Gemini Flash Lite 2.0")],
          ),
          option(
            ~attrs=[Attr.value("Llama 3.1 Nemotron 70B")],
            [text("Llama 3.1 Nemotron 70B")],
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
      begin_chat_button(~globals, ~inject),
      resume_chat_button(~globals, ~inject),
    ],
  );
};

let api_input =
    (
      ~signal,
      ~inject,
      ~globals: Globals.t,
      ~assistantModel: AssistantModel.Model.t,
    )
    : Node.t => {
  let handle_submission = (api_key: string) => {
    JsUtil.log("Your API key for this session has been set: " ++ api_key);
    Virtual_dom.Vdom.Effect.Many([
      inject(AssistantModel.Update.SetKey(api_key)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };
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
    switch (key, ListUtil.last_opt(assistantModel.chat)) {
    | (_, Some({party: LLM, code: None, content: "...", collapsed: false})) => Virtual_dom.Vdom.Effect.Ignore
    | (Some("Enter"), _) => submit_key()
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
          Attr.type_("text"),
          Attr.property("autocomplete", Js.Unsafe.inject("off")),
          Attr.on_focus(_ =>
            signal(MakeActive(ScratchMode.Selection.TextBox))
          ),
          Attr.on_keydown(handle_keydown),
          clss(["api-input"]),
        ],
        (),
      ),
      div(
        ~attrs=[clss(["chat-button"]), Attr.on_click(submit_key)],
        [Widgets.button_named(~tooltip="Update API Key", None, submit_key)],
      ),
      div(~attrs=[clss(["text-display"])], [text("Current API Key:\n")]),
      div(
        ~attrs=[clss(["api-key-display"])],
        [
          text(
            Option.value(
              Store.Generic.load("API"),
              ~default="No API key set",
            ),
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
      ~globals: Globals.t,
      ~assistantModel: AssistantModel.Model.t,
    )
    : Node.t => {
  let handle_send = (message: string) => {
    let message: AssistantModel.Model.message = {
      party: assistantModel.currSender,
      code: None,
      content: message,
      collapsed: String.length(message) >= 200,
    };
    JsUtil.log("Message sent: " ++ message.content);
    Virtual_dom.Vdom.Effect.Many([
      inject(AssistantModel.Update.SendMessage(message)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };

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
    switch (key, ListUtil.last_opt(assistantModel.chat)) {
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
          Attr.placeholder("Type a message..."),
          Attr.type_("text"),
          Attr.property("autocomplete", Js.Unsafe.inject("off")),
          Attr.on_focus(_ =>
            signal(MakeActive(ScratchMode.Selection.TextBox))
          ),
          Attr.on_keydown(handle_keydown),
          clss(["message-input"]),
        ],
        (),
      ),
      switch (ListUtil.last_opt(assistantModel.chat)) {
      | Some({party: LLM, code: None, content: "...", collapsed: false}) =>
        div(
          ~attrs=[
            clss(["send-button-disabled", "icon"]),
            Attr.title("Submitting Message Disabled"),
          ],
          [Icons.thin_x],
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
      ~signal,
      ~inject,
      ~globals: Globals.t,
      ~assistantModel: AssistantModel.Model.t,
    )
    : Node.t => {
  let toggle_collapse = index => {
    // Create an action to toggle the collapsed state of a specific message
    Virtual_dom.Vdom.Effect.Many([
      inject(AssistantModel.Update.ToggleCollapse(index)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };

  let message_nodes =
    List.flatten(
      List.mapi(
        (index: int, message: AssistantModel.Model.message) => {
          switch (message.code) {
          | Some(sketch) =>
            message.content == "..." && message.party == LLM
              ? [loading_dots()]
              : [
                div(
                  ~attrs=[
                    clss([
                      "message-container",
                      message.party == LLM ? "llm" : "ls",
                    ]),
                    Attr.on_click(_ => toggle_collapse(index)),
                  ],
                  [
                    div(
                      ~attrs=[
                        clss([
                          message.party == LLM ? "llm-message" : "ls-message",
                        ]),
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
                      ],
                    ),
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
                      {
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
                      message.party == LLM ? "llm" : "ls",
                    ]),
                    Attr.on_click(_ => toggle_collapse(index)),
                  ],
                  [
                    div(
                      ~attrs=[
                        clss([
                          message.party == LLM ? "llm-message" : "ls-message",
                        ]),
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
                      ],
                    ),
                  ],
                ),
              ]
          }
        },
        assistantModel.chat,
      ),
    );
  div(~attrs=[clss(["message-display-container"])], message_nodes);
};

let view =
    (
      ~globals: Globals.t,
      ~signal,
      ~inject,
      ~assistantModel: AssistantModel.Model.t,
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
                ~attrs=[clss(["title"])],
                [text("Agentic Assistant Chat")],
              ),
              globals.settings.assistant.ongoing_chat
                ? req_button(~globals, ~inject) : None,
              globals.settings.assistant.ongoing_chat
                ? end_chat_button(~globals, ~inject) : None,
            ],
          ),
          globals.settings.assistant.ongoing_chat
            ? message_display(~signal, ~inject, ~globals, ~assistantModel)
            : None,
          globals.settings.assistant.ongoing_chat
            ? message_input(~signal, ~inject, ~globals, ~assistantModel)
            : None,
          globals.settings.assistant.ongoing_chat
            ? None : api_input(~signal, ~inject, ~globals, ~assistantModel),
          globals.settings.assistant.ongoing_chat
            ? None : select_llm(~signal, ~inject, ~globals, ~assistantModel),
          globals.settings.assistant.ongoing_chat
            ? None : settings_box(~globals, ~inject),
        ],
      ),
    ],
  );
};
