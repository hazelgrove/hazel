open Util_web;
open Haz3lcore;
open Ppx_yojson_conv_lib.Yojson_conv;

[@deriving (show({with_path: false}), sexp, yojson)]
type llm_error_origin =
  | MainRequest(int)
  | CompactionRequest(int);

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type prompting = {
    // Note: We allow the user/developer to edit these in-app
    // but the new changes will only appear when a new chat is created
    // after edits have been made.
    system_prompt: string,
    dev_notes: string,
    tools: list(API.Json.t),
    disabled_tool_names: list(string),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    chat_system: ChatSystem.Model.t,
    prompting,
    active_timeline_node: option(int),
    awaiting_response: option(Id.t),
    restore_editor_state: option(Segment.t),
    last_empty_retry_attempt: option(int),
    last_active_task_nudge_attempt: option(int),
    tools_view_expanded: list(string),
    [@yojson.default None]
    compaction_in_progress: option(Id.t),
    [@yojson.default None]
    compaction_method_override: option(string),
    main_llm_seq: int,
    compaction_llm_seq: int,
    [@yojson.default None]
    pending_ignore_main_reply_seq: option(int),
    [@yojson.default None]
    pending_ignore_compaction_reply_seq: option(int),
    /* Chat whose just-appended user send (SendMessage, phase 1) still awaits
       its deferred context/payload dispatch (DispatchSend, phase 2). Treated
       as busy; cleared by DispatchSend and StopAgenticLoop. */
    [@yojson.default None]
    pending_dispatch_send: option(Id.t),
    /* Accumulated SSE deltas for the in-flight main reply. Cleared on
       HandleLLMResponse, ApiErrorResponse, and StopAgenticLoop. The XHR
       handle itself is not serializable and lives in a module-level ref
       ([pending_main_stream_handle] inside Update). */
    [@yojson.default ""] [@sexp.default ""]
    pending_assistant_content: string,
    [@yojson.default ""] [@sexp.default ""]
    pending_assistant_reasoning: string,
  };

  /* Single source of truth for transient (per-session) field defaults; used
     by [Persistent.persist]/[unpersist] and [Utils.init]. */
  let reset_transients = (m: t): t => {
    ...m,
    restore_editor_state: None,
    last_empty_retry_attempt: None,
    last_active_task_nudge_attempt: None,
    tools_view_expanded: [],
    compaction_in_progress: None,
    compaction_method_override: None,
    main_llm_seq: 0,
    compaction_llm_seq: 0,
    pending_ignore_main_reply_seq: None,
    pending_ignore_compaction_reply_seq: None,
    pending_dispatch_send: None,
    pending_assistant_content: "",
    pending_assistant_reasoning: "",
  };
};

module Persistent = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Model.t;

  let persist = (model: Model.t): t => {
    {
      ...Model.reset_transients(model),
      prompting: {
        ...model.prompting,
        tools: CompositionUtils.Public.tools,
      },
    };
  };

  let unpersist = (p: t): Model.t => {
    {
      ...Model.reset_transients(p),
      prompting: {
        ...p.prompting,
        /* Always use the in-code tool registry so new tools (probes, statics, …)
           appear after upgrades; disabled_tool_names still applies per name. */
        tools: CompositionUtils.Public.tools,
      },
      awaiting_response: None,
    };
  };
};
