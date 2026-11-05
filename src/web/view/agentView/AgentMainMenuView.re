open Util;
open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Js_of_ocaml;
open Widgets;

/* the agent's preferences, its view's switches (for ablations; each
   changes what a request sends) and its edit checks */
let settings_group = (~globals: Globals.t): Node.t => {
  let g = globals.settings.agent_globals;
  let set = (a: AgentGlobals.Update.action, _) =>
    Effect.Many([
      globals.inject_global(Globals.Action.SetAgentGlobals(a)),
      Effect.Stop_propagation,
    ]);
  let row = (name, tooltip, control) =>
    div(
      ~attrs=[clss(["agent-setting-row"]), Attr.title(tooltip)],
      [
        div(~attrs=[clss(["agent-setting-name"])], [text(name)]),
        control,
      ],
    );
  let switch_ = (name, tooltip, on, a) =>
    row(name, tooltip, toggle(~tooltip, "", on, set(a)));
  let choice = (name, tooltip, options) =>
    row(
      name,
      tooltip,
      div(
        ~attrs=[clss(["segmented-control"])],
        List.map(
          ((label, active, a)) =>
            div(
              ~attrs=[
                clss(["segment"] @ (active ? ["active"] : [])),
                Attr.on_click(set(a)),
              ],
              [text(label)],
            ),
          options,
        ),
      ),
    );
  let subhead = label =>
    div(~attrs=[clss(["agent-setting-subhead"])], [text(label)]);
  let effort = (label, e: option(OpenRouter.Payload.Model.effort_level)) => (
    label,
    g.reasoning_effort == e,
    AgentGlobals.Update.SetReasoningEffort(e),
  );
  let depth = d => (
    string_of_int(d),
    g.map_module_depth == d,
    AgentGlobals.Update.SetMapModuleDepth(d),
  );
  div(
    ~attrs=[clss(["agent-main-menu-section", "agent-settings"])],
    [
      div(~attrs=[clss(["agent-main-menu-label"])], [text("Settings")]),
      choice(
        "Reasoning effort",
        "Sent only to models that support it",
        [
          effort("Off", None),
          effort("Low", Some(Low)),
          effort("Medium", Some(Medium)),
          effort("High", Some(High)),
        ],
      ),
      switch_(
        "Show thinking",
        "Show the model's reasoning in the chat",
        g.show_thinking,
        ToggleShowThinking,
      ),
      subhead("The map"),
      switch_(
        "Use counts",
        "Each definition's uses",
        g.view_uses,
        ToggleView(Uses),
      ),
      switch_(
        "Errors",
        "An entry's errors and the first one's message",
        g.view_errors,
        ToggleView(Errors),
      ),
      switch_(
        "Test status",
        "Each test's result, and the tally",
        g.view_test_status,
        ToggleView(TestStatus),
      ),
      switch_(
        "Program value",
        "The program's value on its result",
        g.view_value,
        ToggleView(Value),
      ),
      choice(
        "Module depth",
        "How many levels of modules list their members",
        [depth(0), depth(1), depth(2), depth(3)],
      ),
      switch_(
        "Types in full",
        "Type definitions in full, not folded",
        g.map_types_in_full,
        ToggleView(TypesInFull),
      ),
      subhead("Open entries"),
      switch_(
        "Error marks",
        "Errors underlined under their lines",
        g.view_carets,
        ToggleView(Carets),
      ),
      switch_(
        "Probe values",
        "The values of placed probes",
        g.view_probes,
        ToggleView(Probes),
      ),
      switch_(
        "Test marks",
        "Each test's result at its line",
        g.view_test_marks,
        ToggleView(TestMarks),
      ),
      subhead("Edit checks"),
      switch_(
        "No new errors",
        "Refuse a reply's edits that add static errors",
        g.check_no_new_errors,
        ToggleCheckNoNewErrors,
      ),
      switch_(
        "No dangling uses",
        "Refuse a delete that leaves uses behind",
        g.check_dangling_uses,
        ToggleCheckDanglingUses,
      ),
    ],
  );
};

let view =
    (~globals: Globals.t, ~signal: Editors.View.signal => Effect.t(unit))
    : Node.t => {
  let agent_globals = globals.settings.agent_globals;

  let format_price_per_million = (price: string): string => {
    // OpenRouter provides price per 1K tokens; scale to per million for readability
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

  // API Key submit button action
  let submit_api_key = _ => {
    let api_key_input =
      Js.Opt.case(
        Dom_html.document##getElementById(Js.string("agent-api-key-input")),
        () => "",
        el =>
          switch (Js.Unsafe.coerce(el)) {
          | input => Js.to_string(input##.value)
          },
      );
    if (String.length(api_key_input) > 0) {
      let set_api_key_action =
        Globals.Action.SetAgentGlobals(
          AgentGlobals.Update.SetApiKey(api_key_input),
        );
      Effect.Many([
        globals.inject_global(set_api_key_action),
        Effect.Stop_propagation,
      ]);
    } else {
      Effect.Ignore;
    };
  };

  // API Key keydown handler (submit on Enter)
  let handle_api_key_keydown = event => {
    let key = Js.Optdef.to_option(Js.Unsafe.get(event, "key"));
    switch (key) {
    | Some("Enter") =>
      // Blur the input after submission
      Js.Opt.iter(
        Dom_html.document##getElementById(Js.string("agent-api-key-input")),
        el => {
          let _ = Js.Unsafe.coerce(el)##blur();
          ();
        },
      );
      submit_api_key();
    | _ => Effect.Ignore
    };
  };

  // LLM selection handler — also routes back to the chat so the picker
  // doubles as the "change model" target from the in-chat shortcut.
  let set_active_llm = (llm_info: OpenRouter.AvailableLLMs.Model.llm_info) => {
    Effect.Many([
      globals.inject_global(
        Globals.Action.SetAgentGlobals(
          AgentGlobals.Update.SetActiveLlm(llm_info),
        ),
      ),
      globals.inject_global(
        Globals.Action.SetAgentGlobals(
          AgentGlobals.Update.SwitchInterface(
            AgentGlobals.Model.AgentChatInterface,
          ),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  // Model search filter handler
  let set_model_filter = (value: string) =>
    globals.inject_global(
      Globals.Action.SetAgentGlobals(
        AgentGlobals.Update.SetModelFilter(value),
      ),
    );

  // "Only free" toggle handler
  let set_only_free = (value: bool) =>
    globals.inject_global(
      Globals.Action.SetAgentGlobals(
        AgentGlobals.Update.SetOnlyFreeModels(value),
      ),
    );

  // Switch to chat interface handler
  let switch_to_chat = _ => {
    let switch_interface_action =
      Globals.Action.SetAgentGlobals(
        AgentGlobals.Update.SwitchInterface(
          AgentGlobals.Model.AgentChatInterface,
        ),
      );
    Effect.Many([
      globals.inject_global(switch_interface_action),
      Effect.Stop_propagation,
    ]);
  };

  // Current selected LLM info
  let (current_llm_name, current_llm_pricing) =
    switch (agent_globals.active_llm) {
    | Some(llm) =>
      let prompt = format_price_per_million(llm.pricing.prompt);
      let completion = format_price_per_million(llm.pricing.completion);
      (
        llm.name,
        "Prompt: " ++ prompt ++ " /M, Completion: " ++ completion ++ " /M",
      );
    | None => ("None selected", "Pricing: N/A")
    };

  div(
    ~attrs=[clss(["agent-main-menu"])],
    [
      // API Key Section
      div(
        ~attrs=[clss(["agent-main-menu-section"])],
        [
          div(~attrs=[clss(["agent-main-menu-label"])], [text("API Key")]),
          div(
            ~attrs=[clss(["agent-main-menu-info"])],
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
            ~attrs=[clss(["agent-api-key-container"])],
            [
              input(
                ~attrs=[
                  Attr.id("agent-api-key-input"),
                  clss(["agent-api-key-input"]),
                  Attr.placeholder("Enter your OpenRouter API key"),
                  Attr.type_("password"),
                  Attr.property("autocomplete", Js.Unsafe.inject("off")),
                  Attr.on_focus(_ => {
                    Effect.Many([
                      signal(
                        Editors.View.MakeActive(Editors.Selection.Assistant),
                      ),
                      Effect.Stop_propagation,
                    ])
                  }),
                  Attr.on_keydown(handle_api_key_keydown),
                  Attr.on_copy(_ => Effect.Stop_propagation),
                  Attr.on_paste(_ => Effect.Stop_propagation),
                  Attr.on_cut(_ => Effect.Stop_propagation),
                  Attr.create(
                    "value",
                    switch (agent_globals.api_key) {
                    | Some(key) => key
                    | None => ""
                    },
                  ),
                ],
                (),
              ),
              div(
                ~attrs=[
                  clss(["named-menu-item"]),
                  Attr.on_click(submit_api_key),
                  Attr.create("data-testid", "update-api-key-btn"),
                ],
                [button(None, _ => Effect.Ignore), div([text("Enter")])],
              ),
            ],
          ),
        ],
      ),
      // LLM Model Selection Section
      div(
        ~attrs=[clss(["agent-main-menu-section"])],
        [
          div(
            ~attrs=[clss(["agent-main-menu-label"])],
            [text("LLM Model")],
          ),
          div(
            ~attrs=[clss(["agent-main-menu-info"])],
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
          {
            let render_llm_item =
                (
                  ~tagline: option(string)=?,
                  llm: OpenRouter.AvailableLLMs.Model.llm_info,
                ) => {
              let is_active =
                switch (agent_globals.active_llm) {
                | Some(active) => active.id == llm.id
                | None => false
                };
              let classes =
                ["llm-item"]
                @ (is_active ? ["active"] : [])
                @ (Option.is_some(tagline) ? ["llm-item-recommended"] : []);
              let prompt = format_price_per_million(llm.pricing.prompt);
              let completion =
                format_price_per_million(llm.pricing.completion);
              let name_children =
                OpenRouter.AvailableLLMs.is_free(llm)
                  ? [
                    text(llm.name),
                    Node.span(
                      ~attrs=[clss(["llm-free-marker"])],
                      [text(" *")],
                    ),
                  ]
                  : [text(llm.name)];
              let base_children = [
                div(
                  ~attrs=[clss(["llm-id"]), Attr.hidden],
                  [text(llm.id)],
                ),
                div(~attrs=[clss(["llm-name"])], name_children),
                div(
                  ~attrs=[clss(["llm-pricing"])],
                  [
                    text(
                      "Prompt: "
                      ++ prompt
                      ++ " /M, Completion: "
                      ++ completion
                      ++ " /M",
                    ),
                  ],
                ),
              ];
              let children =
                switch (tagline) {
                | Some(t) =>
                  base_children
                  @ [div(~attrs=[clss(["llm-tagline"])], [text(t)])]
                | None => base_children
                };
              div(
                ~attrs=[
                  clss(classes),
                  Attr.on_click(_ => set_active_llm(llm)),
                ],
                children,
              );
            };

            if (List.length(agent_globals.available_llms) == 0) {
              div(
                ~attrs=[clss(["llm-list-container"])],
                [
                  div(
                    ~attrs=[clss(["llm-empty"])],
                    [text("No models available - set API key first")],
                  ),
                ],
              );
            } else {
              // Preserve the curated declaration order (most capable → cheapest).
              let recommended =
                List.filter_map(
                  ((id, _tagline)) =>
                    List.find_opt(
                      (llm: OpenRouter.AvailableLLMs.Model.llm_info) =>
                        llm.id == id,
                      agent_globals.available_llms,
                    ),
                  OpenRouter.AvailableLLMs.recommended_entries,
                );
              let master_sorted =
                List.sort(
                  (
                    a: OpenRouter.AvailableLLMs.Model.llm_info,
                    b: OpenRouter.AvailableLLMs.Model.llm_info,
                  ) =>
                    String.compare(a.name, b.name),
                  agent_globals.available_llms,
                );
              let filter = agent_globals.model_filter;
              let master_filtered =
                List.filter(
                  (llm: OpenRouter.AvailableLLMs.Model.llm_info) => {
                    let name_match =
                      String.length(filter) == 0
                      || StringUtil.subseq_search(llm.name, filter)
                      || StringUtil.subseq_search(llm.id, filter);
                    let free_match =
                      !agent_globals.only_free_models
                      || OpenRouter.AvailableLLMs.is_free(llm);
                    name_match && free_match;
                  },
                  master_sorted,
                );
              let section_header = (label: string) =>
                div(~attrs=[clss(["llm-section-header"])], [text(label)]);
              div(
                ~attrs=[clss(["llm-sections"])],
                (
                  List.length(recommended) == 0
                    ? []
                    : [
                      section_header("Recommended by the FP Lab"),
                      div(
                        ~attrs=[
                          clss([
                            "llm-list-container",
                            "llm-list-container-recommended",
                          ]),
                        ],
                        [
                          div(
                            ~attrs=[clss(["llm-list"])],
                            List.map(
                              (llm: OpenRouter.AvailableLLMs.Model.llm_info) =>
                                render_llm_item(
                                  ~tagline=?
                                    OpenRouter.AvailableLLMs.recommended_tagline(
                                      llm,
                                    ),
                                  llm,
                                ),
                              recommended,
                            ),
                          ),
                        ],
                      ),
                    ]
                )
                @ [
                  section_header("All Models"),
                  div(
                    ~attrs=[clss(["llm-controls"])],
                    [
                      input(
                        ~attrs=[
                          Attr.id("agent-model-search-input"),
                          clss(["llm-search-input"]),
                          Attr.placeholder("Search models..."),
                          Attr.type_("text"),
                          Attr.property(
                            "autocomplete",
                            Js.Unsafe.inject("off"),
                          ),
                          Attr.value(agent_globals.model_filter),
                          Attr.on_focus(_ =>
                            Effect.Many([
                              signal(
                                Editors.View.MakeActive(
                                  Editors.Selection.Assistant,
                                ),
                              ),
                              Effect.Stop_propagation,
                            ])
                          ),
                          Attr.on_input((_, v) => set_model_filter(v)),
                          Attr.on_copy(_ => Effect.Stop_propagation),
                          Attr.on_paste(_ => Effect.Stop_propagation),
                          Attr.on_cut(_ => Effect.Stop_propagation),
                        ],
                        (),
                      ),
                      div(
                        ~attrs=[clss(["llm-only-free-toggle"])],
                        [
                          Node.span(
                            ~attrs=[clss(["llm-only-free-label"])],
                            [
                              text("Free"),
                              Node.span(
                                ~attrs=[clss(["llm-free-marker"])],
                                [text(" *")],
                              ),
                            ],
                          ),
                          toggle(
                            ~tooltip="Show only free models",
                            "",
                            agent_globals.only_free_models,
                            _ =>
                            set_only_free(!agent_globals.only_free_models)
                          ),
                        ],
                      ),
                    ],
                  ),
                  div(
                    ~attrs=[
                      clss([
                        "llm-list-container",
                        "llm-list-container-master",
                      ]),
                    ],
                    [
                      List.length(master_filtered) == 0
                        ? div(
                            ~attrs=[clss(["llm-empty"])],
                            [
                              text(
                                agent_globals.only_free_models
                                && String.length(filter) == 0
                                  ? "No free models available"
                                  : "No models match your search",
                              ),
                            ],
                          )
                        : div(
                            ~attrs=[clss(["llm-list"])],
                            List.map(render_llm_item, master_filtered),
                          ),
                    ],
                  ),
                  div(
                    ~attrs=[clss(["llm-free-footnote"])],
                    [
                      text(
                        "* Free models tend to be heavily rate-limited and often don't support tool calling, which is required by the Hazel coding agent.",
                      ),
                    ],
                  ),
                ],
              );
            };
          },
          div(
            ~attrs=[clss(["llm-current"])],
            [
              div(
                ~attrs=[clss(["llm-current-label"])],
                [text("Current model")],
              ),
              div(
                ~attrs=[clss(["llm-current-name"])],
                [text(current_llm_name)],
              ),
              div(
                ~attrs=[clss(["llm-current-pricing"])],
                [text(current_llm_pricing)],
              ),
            ],
          ),
        ],
      ),
      settings_group(~globals),
      // Confirm Settings Button
      div(
        ~attrs=[clss(["confirm-settings-button-container"])],
        [
          div(
            ~attrs=[
              clss(["confirm-settings-button"]),
              Attr.on_click(switch_to_chat),
            ],
            [text("Confirm Settings")],
          ),
        ],
      ),
    ],
  );
};
