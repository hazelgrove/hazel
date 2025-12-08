open Util;
open Haz3lcore;

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
          | None =>
            print_endline("Assistant: failed to parse models response")
          }
        | None =>
          print_endline("Assistant: no response received from OpenRouter API")
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
