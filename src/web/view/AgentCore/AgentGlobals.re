open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type screen =
    | MainMenu
    | AgentChatInterface;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    active_screen: screen,
    api_key: option(string),
    active_llm: option(OpenRouter.AvailableLLMs.Model.llm_info),
    available_llms: OpenRouter.AvailableLLMs.Model.t,
  };
};

let init = (): Model.t => {
  active_screen: MainMenu,
  api_key: None,
  active_llm: None,
  available_llms: [],
};

let get_active_llm_id = (model: Model.t): option(string) => {
  switch (model.active_llm) {
  | Some(llm) => Some(llm.id)
  | None => None
  };
};

/** Context window from OpenRouter model metadata, or from [available_llms] if active llm omits it. */
let context_length_for_active = (model: Model.t): option(int) => {
  let from_catalog = (id: string): option(int) =>
    switch (
      List.find_opt(
        (m: OpenRouter.AvailableLLMs.Model.llm_info) => m.id == id,
        model.available_llms,
      )
    ) {
    | Some(m) => m.context_length
    | None => None
    };
  switch (model.active_llm) {
  | Some(llm) =>
    switch (llm.context_length) {
    | Some(_) as known => known
    | None => from_catalog(llm.id)
    }
  | None => None
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetApiKey(string)
    | SetActiveLlm(OpenRouter.AvailableLLMs.Model.llm_info)
    | SetAvailableLLMs(OpenRouter.AvailableLLMs.Model.t)
    | SwitchInterface(Model.screen);

  let update =
      (action: action, model: Model.t, schedule_action: action => unit)
      : Model.t => {
    switch (action) {
    | SetApiKey(api_key) =>
      OpenRouter.AvailableLLMs.Utils.get_models(
        ~key=api_key, ~handler=response => {
        switch (response) {
        | Some(json) =>
          switch (
            OpenRouter.AvailableLLMs.Utils.parse_available_models_response(
              json,
            )
          ) {
          | Some(available_llms) =>
            schedule_action(SetAvailableLLMs(available_llms))
          | None => ()
          }
        | None => ()
        }
      });
      {
        ...model,
        api_key: Some(api_key),
      };
    | SetActiveLlm(active_llm) => {
        ...model,
        active_llm: Some(active_llm),
      }
    | SetAvailableLLMs(available_llms) => {
        ...model,
        available_llms,
      }
    | SwitchInterface(screen) => {
        ...model,
        active_screen: screen,
      }
    };
  };
};
