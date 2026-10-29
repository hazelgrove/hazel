open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type screen =
    | MainMenu
    | AgentChatInterface;

  /** Cycle: Edit → Converse → Plan → Edit. See [[next_session_mode]]. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type session_mode =
    | Converse
    | Edit
    | Plan;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type local_key_status =
    | BrowserOnly
    | LocalReady
    | LocalSaved
    | LocalOtherKey
    | LocalBusy
    | LocalError;

  /* Persisted via yojson/sexp (see Settings). Later-added fields carry
     [@default] so older persisted blobs still deserialize; the four
     undefaulted fields are original. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    active_screen: screen,
    [@opaque]
    api_key: option(string),
    [@yojson.default false] [@sexp.default false]
    remember_browser_key: bool,
    [@yojson.default false] [@sexp.default false]
    browser_key_error: bool,
    [@yojson.default false] [@sexp.default false]
    browser_key_busy: bool,
    [@yojson.default false] [@sexp.default false]
    connecting: bool,
    [@yojson.default None] [@sexp.default None]
    connection_error: option(string),
    [@yojson.default BrowserOnly] [@sexp.default BrowserOnly]
    local_key_status,
    [@yojson.default false] [@sexp.default false]
    remember_local_key: bool,
    active_llm: option(OpenRouter.AvailableLLMs.Model.llm_info),
    available_llms: OpenRouter.AvailableLLMs.Model.t,
    [@yojson.default ""] [@sexp.default ""]
    model_filter: string,
    [@yojson.default false] [@sexp.default false]
    only_free_models: bool,
    [@yojson.default None] [@sexp.default None]
    reasoning_effort: option(OpenRouter.Payload.Model.effort_level),
    [@yojson.default true] [@sexp.default true]
    show_thinking: bool,
    [@yojson.default Edit] [@sexp.default Edit]
    session_mode,
    [@yojson.default false] [@sexp.default false]
    collapse_top_bar: bool,
  };

  /* Runtime credentials must never enter settings, exported snapshots, or
     history serialization. Keep the generated readers for legacy migration. */
  let without_credentials = (model: t): t => {
    ...model,
    api_key: None,
    remember_browser_key: false,
    browser_key_error: false,
    browser_key_busy: false,
    connecting: false,
    connection_error: None,
    remember_local_key: false,
    local_key_status: BrowserOnly,
  };
  let with_credentials = (~from: t, model: t): t => {
    ...model,
    api_key: from.api_key,
    remember_browser_key: from.remember_browser_key,
    browser_key_error: from.browser_key_error,
    browser_key_busy: from.browser_key_busy,
    connecting: from.connecting,
    connection_error: from.connection_error,
    remember_local_key: from.remember_local_key,
    local_key_status: from.local_key_status,
  };
  let sexp_of_t = model => sexp_of_t(without_credentials(model));
  let yojson_of_t = model => yojson_of_t(without_credentials(model));
};

let session_mode_label = (m: Model.session_mode): string =>
  switch (m) {
  | Converse => "converse"
  | Edit => "edit"
  | Plan => "plan"
  };

let next_session_mode = (m: Model.session_mode): Model.session_mode =>
  switch (m) {
  | Edit => Converse
  | Converse => Plan
  | Plan => Edit
  };

let init = (): Model.t => {
  active_screen: MainMenu,
  api_key: None,
  remember_browser_key: false,
  browser_key_error: false,
  browser_key_busy: false,
  connecting: false,
  connection_error: None,
  local_key_status: BrowserOnly,
  remember_local_key: false,
  active_llm: None,
  available_llms: [],
  model_filter: "",
  only_free_models: false,
  reasoning_effort: None,
  show_thinking: true,
  session_mode: Edit,
  collapse_top_bar: false,
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

/** True iff active model supports the OpenRouter [reasoning] parameter.
    Prefers the freshly-fetched catalog over [active_llm] (which may be a persisted
    snapshot saved before [supports_reasoning] existed and would default to [false]). */
let active_supports_reasoning = (model: Model.t): bool => {
  switch (model.active_llm) {
  | None => false
  | Some(llm) =>
    switch (
      List.find_opt(
        (m: OpenRouter.AvailableLLMs.Model.llm_info) => m.id == llm.id,
        model.available_llms,
      )
    ) {
    | Some(m) => m.supports_reasoning
    | None => llm.supports_reasoning
    }
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | CredentialEvent
    | SetApiKey(string)
    | LoadBrowserApiKey
    | RestoreBrowserApiKey(AgentAuth.browser_key)
    | SetRememberBrowserKey(bool)
    | PersistBrowserApiKey(bool)
    | BrowserKeySaved(bool, bool)
    | ConnectOpenRouter
    | ConnectionFailed(string)
    | FinishOpenRouterConnection
    | OpenRouterConnected(option(AgentAuth.connection))
    | RefreshModels
    | SetRememberLocalKey(bool)
    | SaveLocalApiKey
    | DeleteLocalApiKey
    | LocalKeyMemoryRemoved(bool)
    | LoadLocalApiKey
    | RestoreLocalApiKey(option(option(string)))
    | SetLocalKeyStatus(Model.local_key_status)
    | ForgetLocalApiKey
    | LocalKeyForgotten(bool)
    | SetActiveLlm(OpenRouter.AvailableLLMs.Model.llm_info)
    | SetAvailableLLMs(OpenRouter.AvailableLLMs.Model.t)
    | SetModelFilter(string)
    | SetOnlyFreeModels(bool)
    | SetReasoningEffort(option(OpenRouter.Payload.Model.effort_level))
    | ToggleShowThinking
    | ToggleCollapseTopBar
    | CycleSessionMode
    | SwitchInterface(Model.screen);

  let is_credential_action =
    fun
    | CredentialEvent => true
    | SetApiKey(_)
    | LoadBrowserApiKey
    | RestoreBrowserApiKey(_)
    | SetRememberBrowserKey(_)
    | PersistBrowserApiKey(_)
    | BrowserKeySaved(_, _)
    | ConnectOpenRouter
    | ConnectionFailed(_)
    | FinishOpenRouterConnection
    | OpenRouterConnected(_)
    | LoadLocalApiKey
    | RestoreLocalApiKey(_)
    | SetRememberLocalKey(_)
    | SaveLocalApiKey
    | DeleteLocalApiKey
    | LocalKeyMemoryRemoved(_)
    | SetLocalKeyStatus(_)
    | ForgetLocalApiKey
    | LocalKeyForgotten(_) => true
    | _ => false;

  let without_credentials = action =>
    is_credential_action(action) ? CredentialEvent : action;
  let sexp_of_action = action => sexp_of_action(without_credentials(action));
  let yojson_of_action = action =>
    yojson_of_action(without_credentials(action));
  let show_action = action => show_action(without_credentials(action));
  let pp_action = (formatter, action) =>
    pp_action(formatter, without_credentials(action));
  let action_of_sexp = sexp => without_credentials(action_of_sexp(sexp));
  let action_of_yojson = json => without_credentials(action_of_yojson(json));

  let key_busy = (model: Model.t) =>
    model.local_key_status == LocalBusy
    || model.browser_key_busy
    || model.connecting;

  let update =
      (action: action, model: Model.t, schedule_action: action => unit)
      : Model.t => {
    switch (action) {
    | CredentialEvent => model
    | LoadBrowserApiKey =>
      schedule_action(
        RestoreBrowserApiKey(AgentAuth.load_browser(model.api_key)),
      );
      model;
    | RestoreBrowserApiKey(result) =>
      schedule_action(LoadLocalApiKey);
      {
        ...model,
        api_key: result.key,
        remember_browser_key: result.remember,
        browser_key_error: result.error,
        browser_key_busy: false,
        connecting: false,
        connection_error: None,
      };
    | SetRememberBrowserKey(remember) =>
      if (key_busy(model)) {
        model;
      } else {
        schedule_action(PersistBrowserApiKey(model.remember_browser_key));
        {
          ...model,
          remember_browser_key: remember,
          browser_key_busy: true,
        };
      }
    | PersistBrowserApiKey(previous) =>
      let ok =
        AgentAuth.save_browser(model.api_key, model.remember_browser_key);
      schedule_action(BrowserKeySaved(ok, previous));
      model;
    | BrowserKeySaved(ok, previous) => {
        ...model,
        browser_key_busy: false,
        browser_key_error: !ok,
        remember_browser_key: ok ? model.remember_browser_key : previous,
      }
    | ConnectOpenRouter =>
      if (key_busy(model)) {
        model;
      } else {
        HazelDB.flush(
          ~callback=
            ok =>
              if (ok) {
                AgentAuth.begin_connection(
                  model.remember_browser_key, model.remember_local_key, error =>
                  schedule_action(ConnectionFailed(error))
                );
              } else {
                schedule_action(
                  ConnectionFailed(
                    "Could not save editor data before connecting. Please retry.",
                  ),
                );
              },
          (),
        );
        {
          ...model,
          connecting: true,
          connection_error: None,
        };
      }
    | ConnectionFailed(error) => {
        ...model,
        connecting: false,
        connection_error: Some(error),
        active_screen: MainMenu,
      }
    | FinishOpenRouterConnection =>
      AgentAuth.finish_connection(result =>
        schedule_action(OpenRouterConnected(result))
      );
      model;
    | OpenRouterConnected(None) => model
    | OpenRouterConnected(Some(result)) =>
      switch (result.key, result.error) {
      | (Some(key), None) =>
        schedule_action(SetApiKey(key));
        {
          ...model,
          remember_browser_key: result.remember_browser,
          remember_local_key:
            result.remember_local && model.local_key_status != BrowserOnly,
          connecting: false,
          connection_error: None,
          active_screen: MainMenu,
        };
      | (_, error) => {
          ...model,
          connecting: false,
          connection_error: error,
          active_screen: MainMenu,
        }
      }
    | LoadLocalApiKey =>
      AgentLocalKey.load(result =>
        schedule_action(RestoreLocalApiKey(result))
      );
      {
        ...model,
        local_key_status: BrowserOnly,
        remember_local_key: false,
      };
    | RestoreLocalApiKey(result) =>
      /* Loading never writes a credential. The file itself records a prior
         opt-in, including across ports; browser-only keys are not migrated. */
      let saved_key = Option.join(result);
      let api_key =
        switch (model.api_key) {
        | Some(_) as key => key
        | None => saved_key
        };
      let remembered = Option.is_some(saved_key) && saved_key == api_key;
      Option.iter(_ => schedule_action(RefreshModels), api_key);
      schedule_action(FinishOpenRouterConnection);
      {
        ...model,
        api_key,
        remember_local_key: remembered,
        local_key_status:
          switch (result) {
          | None => BrowserOnly
          | Some(None) => LocalReady
          | Some(Some(_)) => remembered ? LocalSaved : LocalOtherKey
          },
      };
    | SetRememberLocalKey(remember) =>
      if (model.local_key_status == BrowserOnly || key_busy(model)) {
        model;
      } else if (remember) {
        let has_key = Option.is_some(model.api_key);
        if (has_key) {
          schedule_action(SaveLocalApiKey);
        };
        {
          ...model,
          remember_local_key: true,
          local_key_status: has_key ? LocalBusy : LocalReady,
        };
      } else {
        schedule_action(DeleteLocalApiKey);
        {
          ...model,
          remember_local_key: false,
          local_key_status: LocalBusy,
        };
      }
    | SaveLocalApiKey =>
      Option.iter(
        key =>
          AgentLocalKey.save(key, ok =>
            schedule_action(SetLocalKeyStatus(ok ? LocalSaved : LocalError))
          ),
        model.api_key,
      );
      model;
    | DeleteLocalApiKey =>
      AgentLocalKey.forget(ok => schedule_action(LocalKeyMemoryRemoved(ok)));
      model;
    | LocalKeyMemoryRemoved(true) => {
        ...model,
        remember_local_key: false,
        local_key_status: LocalReady,
      }
    | LocalKeyMemoryRemoved(false) => {
        ...model,
        remember_local_key: true,
        local_key_status: LocalError,
      }
    | SetLocalKeyStatus(local_key_status) => {
        ...model,
        local_key_status,
      }
    | ForgetLocalApiKey =>
      if (key_busy(model)) {
        model;
      } else if (!AgentAuth.save_browser(None, false)) {
        {
          ...model,
          browser_key_error: true,
        };
      } else if (model.local_key_status == BrowserOnly) {
        {
          ...model,
          api_key: None,
          remember_browser_key: false,
          remember_local_key: false,
          browser_key_error: false,
          connection_error: None,
        };
      } else {
        AgentLocalKey.forget(ok => schedule_action(LocalKeyForgotten(ok)));
        {
          ...model,
          remember_browser_key: false,
          browser_key_error: false,
          local_key_status: LocalBusy,
        };
      }
    | LocalKeyForgotten(true) => {
        ...model,
        api_key: None,
        remember_browser_key: false,
        browser_key_error: false,
        connection_error: None,
        remember_local_key: false,
        local_key_status: LocalReady,
      }
    | LocalKeyForgotten(false) => {
        ...model,
        local_key_status: LocalError,
      }
    | SetApiKey(api_key) =>
      let api_key = String.trim(api_key);
      if (api_key == "" || key_busy(model)) {
        model;
      } else {
        let remember =
          model.remember_local_key && model.local_key_status != BrowserOnly;
        if (remember) {
          schedule_action(SaveLocalApiKey);
        };
        schedule_action(PersistBrowserApiKey(model.remember_browser_key));
        schedule_action(RefreshModels);
        {
          ...model,
          browser_key_busy: true,
          connection_error: None,
          api_key: Some(api_key),
          local_key_status: remember ? LocalBusy : model.local_key_status,
        };
      };
    | RefreshModels =>
      Option.iter(
        api_key => {
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
          })
        },
        model.api_key,
      );
      model;
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
    | ToggleShowThinking => {
        ...model,
        show_thinking: !model.show_thinking,
      }
    | ToggleCollapseTopBar => {
        ...model,
        collapse_top_bar: !model.collapse_top_bar,
      }
    | CycleSessionMode => {
        ...model,
        session_mode: next_session_mode(model.session_mode),
      }
    | SwitchInterface(screen) => {
        ...model,
        active_screen: screen,
      }
    };
  };
};
