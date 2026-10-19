open Util;
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

  /* The composed system prompt is ~47KB and a fresh agent carries THREE
     copies (prompting field, each chat's root message, that message's
     api_message) — ~150KB of rebuildable text per slide per save. Like
     the tool registry, swap it for a sentinel at persist and restamp
     from code at load. The swap is equality-guarded, so a prompt the
     user modified persists verbatim; the restamp also means resumed
     chats pick up prompt upgrades (same policy as tools). */
  let prompt_sentinel = "[[HAZEL:SYSTEM_PROMPT]]";
  let current_prompt = (): string =>
    Haz3lcore.CompositionPrompt.self |> String.concat("\n");

  let map_prompt_messages =
      (f: Message.Model.t => Message.Model.t, cs: ChatSystem.Model.t)
      : ChatSystem.Model.t => {
    ...cs,
    chat_map:
      Id.Map.map(
        (chat: Chat.Model.t) =>
          {
            ...chat,
            message_map:
              Id.Map.map(
                (msg: Message.Model.t) =>
                  switch (msg.role) {
                  | System(Prompt) => f(msg)
                  | _ => msg
                  },
                chat.message_map,
              ),
          },
        cs.chat_map,
      ),
  };

  let persist = (model: Model.t): t => {
    let cur = current_prompt();
    /* chat root messages hold the TRIMMED prompt (mk_prompt_message) */
    let cur_msg = String.trim(cur);
    let swap_out = (msg: Message.Model.t) =>
      msg.content == cur_msg
        ? {
          ...msg,
          content: prompt_sentinel,
          api_message: None,
        }
        : msg;
    {
      ...Model.reset_transients(model),
      chat_system: map_prompt_messages(swap_out, model.chat_system),
      prompting: {
        ...model.prompting,
        system_prompt:
          model.prompting.system_prompt == cur
            ? prompt_sentinel : model.prompting.system_prompt,
        /* Never persist the tool registry: unpersist restamps it from
           code, and the JSON runs to ~40KB per slide of dead weight. */
        tools: [],
      },
    };
  };

  let unpersist = (p: t): Model.t => {
    let cur = current_prompt();
    let cur_msg = String.trim(cur);
    let swap_in = (msg: Message.Model.t) =>
      msg.content == prompt_sentinel
        ? {
          ...msg,
          content: cur_msg,
          api_message: Some(OpenRouter.Message.Utils.mk_system_msg(cur_msg)),
        }
        : msg;
    {
      ...Model.reset_transients(p),
      chat_system: map_prompt_messages(swap_in, p.chat_system),
      prompting: {
        ...p.prompting,
        system_prompt:
          p.prompting.system_prompt == prompt_sentinel
            ? cur : p.prompting.system_prompt,
        /* Always use the in-code tool registry so new tools (probes, statics, …)
           appear after upgrades; disabled_tool_names still applies per name. */
        tools: CompositionUtils.Public.tools,
      },
      awaiting_response: None,
    };
  };
};
