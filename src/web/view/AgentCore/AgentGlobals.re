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
    [@yojson.default ""] [@sexp.default ""]
    model_filter: string,
    [@yojson.default false] [@sexp.default false]
    only_free_models: bool,
    [@yojson.default None] [@sexp.default None]
    reasoning_effort: option(OpenRouter.Payload.Model.effort_level),
  };
};

let init = (): Model.t => {
  active_screen: MainMenu,
  api_key: None,
  active_llm: None,
  available_llms: [],
  model_filter: "",
  only_free_models: false,
  reasoning_effort: None,
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

/** Default ceiling for [[effective_context_meter_limit]] (tokens). */
let default_context_meter_max_tokens = 100_000;

/** For the context meter: 80% of the provider's context window, round **down** to a multiple of 1000 tokens (headroom for summarization), clamp to at least 1000, then cap at [[default_context_meter_max_tokens]]. E.g. 131072 → 104000 before cap → 100000; 200000 → 160000 → 100000; smaller models stay under the cap (e.g. 100000 raw → 80000). */
let effective_context_meter_limit = (raw_context_length: int): int => {
  let scaled = float_of_int(raw_context_length) *. 0.8;
  let rounded =
    max(1000, int_of_float(Float.floor(scaled /. 1000.0)) * 1000);
  min(default_context_meter_max_tokens, rounded);
};

/** Like [context_length_for_active], but capped for UI / budgeting (see [effective_context_meter_limit]). */
let context_meter_limit_for_active = (model: Model.t): option(int) =>
  Option.map(
    effective_context_meter_limit,
    context_length_for_active(model),
  );

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetApiKey(string)
    | SetActiveLlm(OpenRouter.AvailableLLMs.Model.llm_info)
    | SetAvailableLLMs(OpenRouter.AvailableLLMs.Model.t)
    | SetModelFilter(string)
    | SetOnlyFreeModels(bool)
    | SetReasoningEffort(option(OpenRouter.Payload.Model.effort_level))
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
    | SetModelFilter(model_filter) => {
        ...model,
        model_filter,
      }
    | SetOnlyFreeModels(only_free_models) => {
        ...model,
        only_free_models,
      }
    | SetReasoningEffort(reasoning_effort) => {
        ...model,
        reasoning_effort,
      }
    | SwitchInterface(screen) => {
        ...model,
        active_screen: screen,
      }
    };
  };
};
