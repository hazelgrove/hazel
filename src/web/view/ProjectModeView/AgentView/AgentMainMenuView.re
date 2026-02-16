open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Js_of_ocaml;
open Widgets;
open Haz3lcore;
// open Icons;

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
        Globals.Action.AgentGlobals(
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

  // LLM selection handler
  let set_active_llm = (llm_info: OpenRouter.AvailableLLMs.Model.llm_info) => {
    let set_active_llm_action =
      Globals.Action.AgentGlobals(
        AgentGlobals.Update.SetActiveLlm(llm_info),
      );
    Effect.Many([
      globals.inject_global(set_active_llm_action),
      Effect.Stop_propagation,
    ]);
  };

  // Switch to chat interface handler
  let switch_to_chat = _ => {
    let switch_interface_action =
      Globals.Action.AgentGlobals(
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
                        Editors.View.MakeActive(
                          Editors.Selection.Projects(
                            ProjectMode.Selection.TextBox,
                          ),
                        ),
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
          div(
            ~attrs=[clss(["llm-list-container"])],
            [
              if (List.length(agent_globals.available_llms) == 0) {
                div(
                  ~attrs=[clss(["llm-empty"])],
                  [text("No models available - set API key first")],
                );
              } else {
                div(
                  ~attrs=[clss(["llm-list"])],
                  List.map(
                    (llm: OpenRouter.AvailableLLMs.Model.llm_info) => {
                      let is_active =
                        switch (agent_globals.active_llm) {
                        | Some(active) => active.id == llm.id
                        | None => false
                        };
                      let classes =
                        ["llm-item"] @ (is_active ? ["active"] : []);
                      div(
                        ~attrs=[
                          clss(classes),
                          Attr.on_click(_ => set_active_llm(llm)),
                        ],
                        {
                          let prompt =
                            format_price_per_million(llm.pricing.prompt);
                          let completion =
                            format_price_per_million(llm.pricing.completion);
                          [
                            div(
                              ~attrs=[clss(["llm-id"]), Attr.hidden],
                              [text(llm.id)],
                            ),
                            div(
                              ~attrs=[clss(["llm-name"])],
                              [text(llm.name)],
                            ),
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
                        },
                      );
                    },
                    agent_globals.available_llms,
                  ),
                );
              },
            ],
          ),
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
