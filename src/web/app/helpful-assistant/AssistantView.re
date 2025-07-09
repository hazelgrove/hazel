open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Js_of_ocaml;
open Key;

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

let new_chat_button = (~inject, ~model: Model.t): Node.t => {
  let tooltip = "New Chat";
  let new_chat_action =
    model.current_chats.curr_tutor_chat == Id.invalid
      ? [] : [inject(Update.ChatAction(NewChat))];
  let new_chat = _ =>
    Virtual_dom.Vdom.Effect.Many(
      new_chat_action @ [Virtual_dom.Vdom.Effect.Stop_propagation],
    );
  div(
    ~attrs=[clss(["add-button"]), Attr.on_click(new_chat)],
    [
      Widgets.button(~tooltip, Icons.add, _ => Virtual_dom.Vdom.Effect.Ignore),
    ],
  );
};

let history_button =
    (~inject, ~inject_global: Globals.Action.t => Ui_effect.t(unit)): Node.t => {
  let tooltip = "Past Chats";
  let history = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject(Update.ChatAction(CollapseMessage(-1))), // Hacky way to collapse any active prompt display
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
    (~inject: Update.t => Ui_effect.t(unit), ~model: Model.t): Node.t => {
  let handle_change = (event, _) => {
    let value = Js.to_string(Js.Unsafe.coerce(event)##.target##.value);
    Virtual_dom.Vdom.Effect.Many([
      inject(ExternalAPIAction(SetLLM(value))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };

  div(
    ~attrs=[clss(["llm-selector"])],
    [
      label(~attrs=[clss(["llm-label"])], [text("LLM Model")]),
      select(
        ~attrs=[Attr.on_change(handle_change), clss(["llm-dropdown"])],
        List.map(
          (open_router_model: OpenRouter.model_info) =>
            option(
              ~attrs=[
                Attr.value(open_router_model.id),
                switch (model.external_api_info.set_model) {
                | "" => Attr.empty
                | current_model =>
                  if (current_model == open_router_model.id) {
                    Attr.selected;
                  } else {
                    Attr.empty;
                  }
                },
              ],
              [text(open_router_model.name)],
            ),
          model.external_api_info.available_models,
        ),
      ),
    ],
  );
};

let api_input =
    (
      ~inject: Update.t => Ui_effect.t(unit),
      ~inject_global: Globals.Action.t => Ui_effect.t(unit),
      ~signal,
      ~model: Model.t,
      ~settings: AssistantSettings.t,
    )
    : Node.t => {
  let init_assistant_model =
    model.current_chats.curr_tutor_chat == Id.invalid
      ? [inject(Update.InitializeAssistant)] : [];
  let handle_submission = (api_key: string) => {
    Virtual_dom.Vdom.Effect.Many(
      init_assistant_model
      @ [
        inject(ExternalAPIAction(SetAPIKey(api_key))),
        Virtual_dom.Vdom.Effect.Stop_propagation,
      ],
    );
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
          text("Get an OpenRouter API key "),
          a(
            ~attrs=[
              Attr.href("https://openrouter.ai/settings/keys"),
              Attr.target("_blank"),
            ],
            [text("here")],
          ),
          text("."),
        ],
      ),
      div(
        ~attrs=[clss(["llm-selector"])],
        [
          label(~attrs=[clss(["llm-label"])], [text("New API Key")]),
          input(
            ~attrs=[
              Attr.id("api-input"),
              Attr.placeholder("Click to enter your OpenRouter API key"),
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
        ],
      ),
      div(
        ~attrs=[clss(["chat-button"]), Attr.on_click(submit_key)],
        [Widgets.button_named(~tooltip="Update API Key", None, submit_key)],
      ),
      div(
        ~attrs=[clss(["llm-selector"])],
        [
          label(
            ~attrs=[clss(["llm-label"])],
            [
              text("Current API Key\n"),
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
          div(
            ~attrs=[clss(["api-key-row"])],
            [
              div(
                ~attrs=[
                  clss(["api-key-display"]),
                  Attr.id("api-key-display"),
                ],
                [
                  text(
                    switch (model.external_api_info.api_key) {
                    | "" => "No API key set"
                    | key =>
                      settings.show_api_key
                        ? key : String.make(String.length(key), '*')
                    },
                  ),
                ],
              ),
            ],
          ),
        ],
      ),
    ],
  );
};

let llm_model_id_input =
    (~inject: Update.t => Ui_effect.t(unit), ~signal, ~model: Model.t)
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
      inject(ExternalAPIAction(SetLLM(llm_model))),
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
          text("See available OpenRouter models "),
          a(
            ~attrs=[
              Attr.href("https://openrouter.ai/models"),
              Attr.target("_blank"),
            ],
            [text("here")],
          ),
          text("."),
        ],
      ),
      select_llm(~inject, ~model),
      div(
        ~attrs=[clss(["llm-selector"])],
        [
          div(
            ~attrs=[clss(["llm-label"])],
            [text("Or Enter Model ID Manually")],
          ),
          input(
            ~attrs=[
              Attr.id("llm-model-id-input"),
              Attr.placeholder("Enter the ID of an OpenRouter model"),
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
        ],
      ),
      div(
        ~attrs=[clss(["chat-button"]), Attr.on_click(submit_key)],
        [Widgets.button_named(~tooltip="Update Model", None, submit_key)],
      ),
      div(
        ~attrs=[clss(["llm-selector"])],
        [
          label(~attrs=[clss(["llm-label"])], [text("Current Model\n")]),
          div(
            ~attrs=[clss(["api-key-display"]), Attr.id("api-key-display")],
            [
              text(
                switch (model.external_api_info.set_model) {
                | "" => "No model ID set"
                | model_id => model_id
                },
              ),
            ],
          ),
        ],
      ),
      div(
        ~attrs=[clss(["llm-selector"])],
        [
          label(
            ~attrs=[clss(["llm-label"])],
            [text("Model Pricing (per million tokens)\n")],
          ),
          div(
            ~attrs=[clss(["api-key-display"])],
            [
              text(
                switch (model.external_api_info.set_model) {
                | "" => "No model selected"
                | model_id =>
                  let selected_model =
                    List.find_opt(
                      (model: OpenRouter.model_info) => model.id == model_id,
                      model.external_api_info.available_models,
                    );
                  switch (selected_model) {
                  | Some(model) =>
                    "Prompt: "
                    ++ format_price_per_million(model.pricing.prompt)
                    ++ " / Completion: "
                    ++ format_price_per_million(model.pricing.completion)
                  | None => "Pricing information not available"
                  };
                },
              ),
            ],
          ),
        ],
      ),
    ],
  );
};

let mk_input_handlers =
    (
      settings: AssistantSettings.t,
      model: Model.t,
      index: option(int),
      inject: Update.t => Ui_effect.t(unit),
      which_input: string,
    ) => {
  let mode = settings.mode;
  let curr_chat_id =
    switch (mode) {
    | CodeSuggestion => model.current_chats.curr_suggestion_chat
    | TaskCompletion => model.current_chats.curr_composition_chat
    | HazelTutor => model.current_chats.curr_tutor_chat
    };
  let curr_messages =
    switch (mode) {
    | CodeSuggestion =>
      Id.Map.find(curr_chat_id, model.chat_history.past_suggestion_chats).
        messages
    | TaskCompletion =>
      Id.Map.find(curr_chat_id, model.chat_history.past_composition_chats).
        messages
    | HazelTutor =>
      Id.Map.find(curr_chat_id, model.chat_history.past_tutor_chats).messages
    };
  let editor_opt =
    switch (index) {
    | Some(idx) => List.nth(curr_messages, idx).sketch_snapshot
    | None => None
    };
  let handle_send = (content: string) => {
    Js_of_ocaml.Firebug.console##log(
      Js_of_ocaml.Js.string("Message sent: " ++ content),
    );
    Virtual_dom.Vdom.Effect.Many([
      switch (index) {
      | Some(index) => inject(Update.ChatAction(Lop(index)))
      | _ => Virtual_dom.Vdom.Effect.Ignore
      },
      switch (mode) {
      | HazelTutor =>
        inject(
          Update.SendMessage(
            Tutor(content),
            editor_opt,
            model.current_chats.curr_tutor_chat,
          ),
        )
      | CodeSuggestion =>
        inject(
          Update.SendMessage(
            Completion(Query(content)),
            editor_opt,
            model.current_chats.curr_suggestion_chat,
          ),
        )
      | TaskCompletion =>
        inject(
          Update.SendMessage(
            Composition(Request(content)),
            editor_opt,
            model.current_chats.curr_composition_chat,
          ),
        )
      },
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  };
  let (past_chats, curr_chat) = Update.get_mode_info(settings.mode, model);
  let curr_messages = Id.Map.find(curr_chat.id, past_chats).messages;
  let send_message = _ => {
    let message =
      Js.Opt.case(
        Dom_html.document##getElementById(Js.string(which_input)),
        () => "",
        el =>
          switch (Js.Unsafe.coerce(el)) {
          | input => Js.to_string(input##.value)
          },
      );
    switch (index) {
    | Some(_) =>
      Js.Opt.case(
        Dom_html.document##getElementById(Js.string(which_input)),
        () => (),
        el => {Js.Unsafe.coerce(el)##blur()},
      )
    | _ =>
      Js.Opt.case(
        Dom_html.document##getElementById(Js.string(which_input)),
        () => (),
        el => {
          Js.Unsafe.coerce(el)##.value := Js.string("");
          Js.Unsafe.coerce(el)##blur();
        },
      )
    };
    handle_send(message);
  };
  let handle_keydown = event => {
    let key = Js.Optdef.to_option(Js.Unsafe.get(event, "key"));
    let shift_pressed = shift_held(event);
    switch (key, ListUtil.last_opt(curr_messages)) {
    | (Some("Enter"), _) when !shift_pressed => send_message()
    | _ => Virtual_dom.Vdom.Effect.Ignore
    };
  };
  (send_message, handle_keydown);
};

let message_input =
    (~signal, ~inject, ~model: Model.t, ~settings: AssistantSettings.t)
    : Node.t => {
  let (send_message, handle_keydown) =
    mk_input_handlers(settings, model, None, inject, "message-input");
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
      switch (
        ListUtil.last_opt(
          [] /*curr_messages*/ /* todo: change. temporarily repressed.*/,
        )
      ) {
      // todo: change, maybe to allowing for stop button to cancel request
      | Some(
          {displayable_content: [Text("...")], collapsed: false}: Model.display,
        ) =>
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

/*
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
 */

let form_collapse_toggle =
    (
      ~message: Model.message,
      ~toggle_collapse,
      ~index: int,
      ~is_first: bool,
      ~is_last: bool,
    )
    : Node.t =>
  if (message.display.collapsed && is_first) {
    div(
      ~attrs=[
        clss(["collapse-indicator"]),
        Attr.on_click(_ => toggle_collapse(index)),
        String.length(message.content.content) >= Model.max_collapsed_length
          ? Attr.empty : Attr.hidden,
      ],
      [text("▼ Show more")],
    );
  } else if (!message.display.collapsed
             && String.length(message.content.content)
             >= Model.max_collapsed_length
             && is_last) {
    div(
      ~attrs=[
        clss(["collapse-indicator"]),
        Attr.on_click(_ => toggle_collapse(index)),
        String.length(message.content.content) >= Model.max_collapsed_length
          ? Attr.empty : Attr.hidden,
      ],
      [text("▲ Show less")],
    );
  } else {
    None;
  };

let mk_translation = (~text: string): list(Node.t) => {
  let omd = Omd.of_string(text);
  //print_markdown(omd);

  let rec translate_inline =
          (inline: Omd.inline(_), msg: list(Node.t)): list(Node.t) => {
    switch (inline) {
    | Omd.Concat(_, items) =>
      let nodes =
        List.fold_left(
          (msg, item) => {
            let translated_item = translate_inline(item, []);
            List.concat([msg, translated_item]);
          },
          [],
          items,
        );
      List.append(msg, nodes);
    | Omd.Text(_, d) => List.append(msg, [Node.text(d)])
    | Omd.Code(_, d) =>
      List.append(
        msg,
        [
          Node.span(
            ~attrs=[
              Attr.style(
                Css_gen.create(~field="font-family", ~value="monospace"),
              ),
            ],
            [Node.text(d)],
          ),
        ],
      )
    | Omd.Link(_, {label, destination, _}) =>
      let d = translate_inline(label, []);
      let _ =
        switch (Id.of_string(destination)) {
        | Some(id) => id
        | None => Id.invalid
        };
      let inner_msg = Node.span(~attrs=[], d);
      List.append(msg, [inner_msg]);
    | Omd.Emph(_, d) =>
      let d = translate_inline(d, []);
      List.append(
        msg,
        [
          Node.span(
            ~attrs=[
              Attr.style(
                Css_gen.create(~field="font-style", ~value="italic"),
              ),
            ],
            d,
          ),
        ],
      );
    | Omd.Strong(_, d) =>
      let d = translate_inline(d, []);
      List.append(
        msg,
        [
          Node.span(
            ~attrs=[
              Attr.style(
                Css_gen.create(~field="font-weight", ~value="bold"),
              ),
            ],
            d,
          ),
        ],
      );
    | Omd.Soft_break(_) => List.append(msg, [Node.br()])
    | Omd.Hard_break(_) => List.append(msg, [Node.br(), Node.br()])
    | _ => msg
    };
  };

  let rec translate_block = (doc: Omd.doc): list(Node.t) => {
    List.fold_left(
      (msg, elem) => {
        switch (elem) {
        | Omd.Paragraph(_, d) => translate_inline(d, msg)
        | Omd.List(_, _, _, items) =>
          let bullets =
            List.fold_left(
              (nodes, d) => {
                let n = translate_block(d);
                List.append(nodes, [Node.li(n)]);
              },
              [],
              items,
            );
          List.append(msg, [Node.ul(bullets)]);
        | Omd.Blockquote(_, d) =>
          List.append(msg, [Node.blockquote(translate_block(d))])
        | Omd.Heading(_, _, d) =>
          List.append(msg, [Node.h1(translate_inline(d, []))])
        | Omd.Thematic_break(_) => List.append(msg, [Node.hr()])
        | Omd.Code_block(_, _, d) =>
          // Todo: Potentially remove parse_blocks and favor pure markdown instead.
          // This will require a bit of rewiring, however.
          List.append(msg, [Node.text(d)])
        | Omd.Html_block(_, d) => List.append(msg, [Node.text(d)])
        | _ => msg
        }
      },
      [],
      doc,
    );
  };
  [Node.br()] @ translate_block(omd);
};

let text_block =
    (
      ~message: Model.message,
      ~content: string,
      ~toggle_collapse,
      ~index: int,
      ~signal,
      ~settings: AssistantSettings.t,
      ~model: Model.t,
      ~inject: Update.t => Ui_effect.t(unit),
      ~is_first: bool,
      ~is_last: bool,
    )
    : Node.t =>
  if (message.role == User) {
    let unique_id = "user-message-input-" ++ string_of_int(index);
    let (send_message, handle_keydown) =
      mk_input_handlers(settings, model, Some(index), inject, unique_id);

    // Auto-resize textarea when first rendered with content
    JsUtil.delay(0.0, () => JsUtil.autosize_textarea(unique_id));
    div(
      ~attrs=[clss(["user-message-input-container"])],
      [
        textarea(
          ~attrs=[
            clss(["user-message-input"]),
            Attr.id(unique_id),
            Attr.value(content),
            Attr.placeholder(
              switch (settings.mode) {
              | HazelTutor => "Ask a question about Hazel (or anything)..."
              | CodeSuggestion => "Followup with a question about the agent's code suggestion..."
              | TaskCompletion => "Ask the agent to help clarify, plan, or write code..."
              },
            ),
            Attr.property("autocomplete", Js.Unsafe.inject("off")),
            Attr.on_focus(_ => {
              JsUtil.autosize_textarea(unique_id);
              signal(MakeActive(ScratchMode.Selection.TextBox));
            }),
            Attr.on_input(_ => {
              JsUtil.autosize_textarea(unique_id);
              _ => Virtual_dom.Vdom.Effect.Ignore;
            }),
            Attr.on_copy(_ => {Effect.Stop_propagation}),
            Attr.on_paste(_ => {
              JsUtil.delay(0.0, () => JsUtil.autosize_textarea(unique_id));
              Effect.Stop_propagation;
            }),
            Attr.on_cut(_ => {Effect.Stop_propagation}),
            Attr.on_keydown(handle_keydown),
          ],
          [text(content)],
        ),
        div(
          ~attrs=[
            clss(["user-send-button", "icon"]),
            Attr.on_click(send_message),
            Attr.title("Submit Message"),
          ],
          [Icons.send],
        ),
      ],
    );
  } else {
    div(
      ~attrs=[
        clss([
          switch (message.role) {
          | User => "user-message"
          | Assistant => "llm-message"
          | System(AssistantPrompt) => "system-prompt-message"
          | System(InternalError) => "system-error-message"
          | Tool => "tool-message"
          },
        ]),
        Attr.on_copy(_ => {Effect.Stop_propagation}),
        Attr.on_paste(_ => {Effect.Stop_propagation}),
        Attr.on_cut(_ => {Effect.Stop_propagation}),
        Attr.on_click(_ => {Effect.Stop_propagation}),
      ],
      {
        let content =
          message.display.collapsed
          && String.length(message.content.content)
          >= Model.max_collapsed_length
            ? String.concat(
                "",
                [
                  String.sub(
                    content,
                    0,
                    min(String.length(content), Model.max_collapsed_length),
                  ),
                  "...",
                ],
              )
            : content;
        mk_translation(~text=content)
        @ [
          form_collapse_toggle(
            ~message,
            ~toggle_collapse,
            ~index,
            ~is_first,
            ~is_last,
          ),
        ];
      },
    );
  };

let code_block =
    (
      ~message: Model.message,
      ~sketch: Segment.t,
      ~toggle_collapse,
      ~index: int,
      ~is_first: bool,
      ~is_last: bool,
      ~globals: Globals.t,
    )
    : Node.t => {
  div(
    ~attrs=[
      clss([
        "example",
        switch (message.role) {
        | User => "user"
        | Assistant => "llm"
        | System(AssistantPrompt) => "system-prompt"
        | System(InternalError) => "system-error"
        | Tool => "tool"
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
        message.role == Assistant
          ? {
            sketch |> Zipper.unzip |> Editor.Model.mk |> CellEditor.Model.mk;
          }
          : {
            sketch
            |> Zipper.unzip
            |> Editor.Model.mk
            |> CellEditor.Model.mk
            |> CellEditor.Update.calculate(
                 ~settings={
                   statics: false,
                   elaborate: false,
                   assist: false,
                   dynamics: false,
                   flip_animations: false,
                   evaluation: {
                     show_case_clauses: false,
                     show_fn_bodies: false,
                     show_fixpoints: false,
                     show_ascription_steps: false,
                     show_lookup_steps: false,
                     show_stepper_filters: false,
                     stepper_history: false,
                     show_settings: false,
                     show_hidden_steps: false,
                     enable_proof: false,
                   },
                 },
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
      ~settings: AssistantSettings.t,
      ~signal,
      ~model: Model.t,
      ~inject: Update.t => Ui_effect.t(unit),
    )
    : Node.t =>
  if (!message.display.collapsed || message.display.collapsed && is_first) {
    switch (block) {
    | Text(content) =>
      text_block(
        ~message,
        ~content,
        ~toggle_collapse,
        ~index,
        ~is_first,
        ~is_last,
        ~settings,
        ~signal,
        ~model,
        ~inject,
      )
    | Code(sketch) =>
      code_block(
        ~message,
        ~sketch,
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

let initial_display =
    (~model: Model.t, ~settings: AssistantSettings.t): Node.t => {
  let (past_chats, curr_chat) = Update.get_mode_info(settings.mode, model);
  let curr_messages = Id.Map.find(curr_chat.id, past_chats).messages;
  List.length(curr_messages) <= 1
    ? div(
        ~attrs=[clss(["initial-display"])],
        [
          Icons.hazelnut_agent,
          div(
            ~attrs=[clss(["initial-display-text"])],
            [
              text(
                switch (settings.mode) {
                | HazelTutor => "Hi, I'm Hazelbot! Ask me anything about Hazel."
                | CodeSuggestion => "Hi, I'm Hazelbot! Ask me for code suggestions."
                | TaskCompletion => "Hi, I'm Hazelbot! Let's work on your task together."
                },
              ),
            ],
          ),
          div(
            ~attrs=[clss(["disclaimer-display-text"])],
            [
              text(
                "AI-based technologies, such as the Hazel Assistant, are prone to making mistakes. Always verify critical information independently.",
              ),
            ],
          ),
        ],
      )
    : None;
};

let message_display =
    (
      ~globals: Globals.t,
      ~inject,
      ~model: Model.t,
      ~settings: AssistantSettings.t,
      ~signal,
    )
    : Node.t => {
  let toggle_collapse = (is_system_prompt, index) => {
    // Create an action to toggle the collapsed state of a specific message
    Virtual_dom.Vdom.Effect.Many(
      if (is_system_prompt && settings.show_history) {
        [globals.inject_global(Set(Assistant(ToggleHistory)))];
      } else {
        {
          [];
        }
        @ [
          inject(Update.ChatAction(CollapseMessage(index))),
          Virtual_dom.Vdom.Effect.Stop_propagation,
        ];
      },
    );
  };
  let (past_chats, curr_chat) = Update.get_mode_info(settings.mode, model);
  let curr_messages = Id.Map.find(curr_chat.id, past_chats).messages;
  let message_nodes =
    List.flatten(
      List.mapi(
        (index: int, message: Model.message) => {
          [
            div(
              ~attrs=[
                clss([
                  "message-container",
                  switch (message.role) {
                  | User => "user"
                  | Assistant => "llm"
                  | System(AssistantPrompt) => "system-prompt"
                  | System(InternalError) => "system-error"
                  | Tool => "tool"
                  },
                ]),
              ],
              [
                div(
                  ~attrs=[clss(["message-identifier-row"])],
                  [
                    div(
                      ~attrs=[clss(["message-identifier"])],
                      [
                        switch (message.role) {
                        | User => text("User")
                        | Assistant =>
                          switch (settings.mode) {
                          | CodeSuggestion =>
                            div(
                              ~attrs=[clss(["llm-identifier"])],
                              [Icons.hazelnut_agent, text("Assistant")],
                            )
                          | TaskCompletion =>
                            div(
                              ~attrs=[clss(["llm-identifier"])],
                              [Icons.hazelnut_agent, text("Agent")],
                            )
                          | HazelTutor =>
                            div(
                              ~attrs=[clss(["llm-identifier"])],
                              [Icons.hazelnut_agent, text("Tutor")],
                            )
                          }

                        | System(AssistantPrompt) =>
                          div(
                            ~attrs=[clss(["system-prompt-identifier"])],
                            [text("System")],
                          )
                        | System(InternalError) =>
                          div(
                            ~attrs=[clss(["system-error-identifier"])],
                            [text("System")],
                          )
                        | Tool =>
                          div(
                            ~attrs=[clss(["tool-identifier"])],
                            [text("Tool")],
                          )
                        },
                      ],
                    ),
                    message.role == System(AssistantPrompt)
                      ? div(
                          ~attrs=[clss(["show-prompt-button"])],
                          [
                            Widgets.button(~tooltip="Show Prompt", Icons.doc, _ =>
                              toggle_collapse(true, index)
                            ),
                          ],
                        )
                      : None,
                  ],
                ),
              ]
              @ {
                message.role == System(AssistantPrompt)
                  ? [None]
                  : {
                    let parsed_blocks = message.display.displayable_content;
                    List.mapi(
                      (idx, block: Model.block_kind) =>
                        form_block(
                          ~message,
                          ~block,
                          ~toggle_collapse=toggle_collapse(false),
                          ~index,
                          ~is_first=idx == 0,
                          ~is_last=idx == List.length(parsed_blocks) - 1,
                          ~globals,
                          ~settings,
                          ~signal,
                          ~model,
                          ~inject,
                        ),
                      parsed_blocks,
                    );
                  };
              },
            ),
          ]
        },
        curr_messages,
      ),
    );
  div(
    ~attrs=[clss(["message-display-container"])],
    message_nodes @ [initial_display(~model, ~settings)],
  );
};

let get_sidebar_width = () => {
  let sidebar =
    Js.Unsafe.coerce(Dom_html.document)##getElementById("side-bar");
  if (Js.Opt.test(sidebar)) {
    let width_str = Js.to_string(sidebar##.style##.width);
    if (String.length(width_str) >= 2
        && String.sub(width_str, String.length(width_str) - 2, 2) == "px") {
      try(
        int_of_string(
          String.sub(width_str, 0, String.length(width_str) - 2),
        )
      ) {
      | Invalid_argument(_) => 400 // default width on error
      };
    } else {
      400; // default width if no 'px' suffix
    };
  } else {
    400; // default width
  };
};

let prompt_display =
    (
      ~globals: Globals.t,
      ~model: Model.t,
      ~settings: AssistantSettings.t,
      ~signal,
      ~inject,
    )
    : Node.t => {
  let (past_chats, curr_chat) = Update.get_mode_info(settings.mode, model);
  let curr_messages = Id.Map.find(curr_chat.id, past_chats).messages;
  let display =
    List.find_mapi(
      (index: int, message: Model.message) => {
        message.role == System(AssistantPrompt) && !message.display.collapsed
          ? Some(
              div(
                ~attrs=[
                  Attr.id("prompt-display-container"),
                  Attr.create(
                    "style",
                    "right: "
                    ++ string_of_int(get_sidebar_width() + 20)
                    ++ "px",
                  ),
                ],
                {
                  let parsed_blocks = message.display.displayable_content;
                  List.map(
                    (block: Model.block_kind) =>
                      form_block(
                        ~message,
                        ~block,
                        ~toggle_collapse=_ => {Virtual_dom.Vdom.Effect.Ignore},
                        ~index,
                        ~is_first=false,
                        ~is_last=false,
                        ~globals,
                        ~settings,
                        ~signal,
                        ~model,
                        ~inject,
                      ),
                    parsed_blocks,
                  );
                },
              ),
            )
          : None
      },
      curr_messages,
    );
  switch (display) {
  | Some(node) => node
  | None => None
  };
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
    ~attrs=[
      Attr.id("history-menu"),
      Attr.create(
        "style",
        "right: " ++ string_of_int(get_sidebar_width() + 20) ++ "px",
      ),
    ],
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
                      [text(TimeUtil.format_time_diff(chat.timestamp))],
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
  let settings = globals.settings;
  let inject_global = globals.inject_global;
  /* For debugging: Uncomment to view chat history
     let curr_chat =
       Id.Map.find(
         model.current_chats.curr_composition_chat,
         model.chat_history.past_composition_chats,
       );
     let concat_text =
       String.concat(
         "\n",
         List.map(
           (message: Model.display) =>
             "<"
             ++ Model.string_of_role(message.role)
             ++ ">"
             ++ message.original_content
             ++ "</"
             ++ Model.string_of_role(message.role)
             ++ ">\n",
           curr_chat.message_displays,
         ),
       );
     print_endline(concat_text);
     Js_of_ocaml.Firebug.console##log(Js_of_ocaml.Js.string(concat_text));
       */
  let view =
    div(
      ~attrs=[Attr.id("assistant")],
      [
        div(
          ~attrs=[clss(["header"])],
          [
            settings.assistant.ongoing_chat
              ? mode_buttons(~inject_global, ~settings=settings.assistant)
              : div(
                  ~attrs=[clss(["main-title"])],
                  [text("Assistant Settings")],
                ),
            settings.assistant.ongoing_chat
              ? history_button(~inject, ~inject_global) : None,
            settings.assistant.ongoing_chat
              ? new_chat_button(~inject, ~model) : None,
            settings.assistant.ongoing_chat
              ? settings_button(~inject_global)
              : resume_chat_button(~inject_global),
          ],
        ),
        settings.assistant.ongoing_chat
          ? message_display(
              ~globals,
              ~inject,
              ~model,
              ~settings=settings.assistant,
              ~signal,
            )
          : None,
        settings.assistant.ongoing_chat
          ? message_input(
              ~signal,
              ~inject,
              ~model,
              ~settings=settings.assistant,
            )
          : None,
        settings.assistant.ongoing_chat
          ? None
          : api_input(
              ~inject,
              ~inject_global,
              ~signal,
              ~model,
              ~settings=settings.assistant,
            ),
        settings.assistant.ongoing_chat
          ? None : llm_model_id_input(~inject, ~model, ~signal),
        settings.assistant.ongoing_chat && settings.assistant.show_history
          ? history_menu(~model, ~settings=settings.assistant, ~inject) : None,
        prompt_display(
          ~globals,
          ~model,
          ~settings=settings.assistant,
          ~signal,
          ~inject,
        ),
      ],
    );
  view;
};
