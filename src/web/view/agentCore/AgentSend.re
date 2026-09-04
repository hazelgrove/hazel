open Util_web;
open Haz3lcore;
open AgentModel;

module ToolUtils = AgentToolUtils;
module Utils = AgentUtils;
module Action = AgentAction;

let max_api_retries = 3;
let is_retryable_api_error = (code: int): bool =>
  code == 429 || code == 500 || code == 502 || code == 503;

let format_api_error_content = (~code: int, ~message: string): string =>
  "Code: " ++ string_of_int(code) ++ "\nError: " ++ message;

/* In-flight main stream handle. Not part of Model.t because
   [API.streaming_handle] holds a closure and is not serializable; strictly
   transient and meaningless across reloads. Cleared in the stream's
   [on_done] and on Stop. */
let pending_main_stream_handle: ref(option(API.streaming_handle)) =
  ref(None);

/* Deferral for phase 2 of a send: a 0ms timeout is a macrotask, so the
   browser paints the just-appended user message before the expensive
   context/payload work runs (a bare schedule_action could batch into the
   same frame). Tests override this to run the thunk synchronously. */
let defer_dispatch_send: ref((unit => unit) => unit) =
  ref(thunk => JsUtil.delay(0.0, thunk));

let abort_main_stream_handle = (): unit =>
  switch (pending_main_stream_handle^) {
  | Some(h) =>
    pending_main_stream_handle := None;
    h.abort();
  | None => ()
  };

/** Max retries when the assistant returns an empty reply with no tool calls. */
let max_empty_retries = 2;

/** True iff [name] is allowed in [mode]. Edit allows everything; Plan
    blocks edit tools; Converse blocks edit + workbench + overlay tools
    (only ViewTools like expand/collapse remain). The name lists derive
    from [[AgentToolUtils.registry]]. */
let tool_allowed_in_mode =
    (mode: AgentGlobals.Model.session_mode, name: string): bool =>
  switch (mode) {
  | Edit => true
  | Plan => !List.mem(name, ToolUtils.edit_tool_names)
  | Converse =>
    !List.mem(name, ToolUtils.edit_tool_names)
    && !List.mem(name, ToolUtils.workbench_tool_names)
    && !List.mem(name, ToolUtils.overlay_tool_names)
  };

let enabled_tools =
    (~mode: AgentGlobals.Model.session_mode, prompting: Model.prompting)
    : list(API.Json.t) =>
  List.filter(
    (tool: API.Json.t) =>
      switch (ToolUtils.get_name(tool)) {
      | Some(name) =>
        !List.mem(name, prompting.disabled_tool_names)
        && tool_allowed_in_mode(mode, name)
      | None => true
      },
    CompositionUtils.Public.tools,
  );
// Exponential backoff
let backoff_ms = (attempt: int): float => 1000.0 *. 2.0 ** float(attempt);

let chat_naming_model_id = "google/gemini-3.1-flash-lite";

let request_chat_name =
    (
      ~api_key: string,
      ~user_message: string,
      ~schedule_action: Action.t => unit,
      ~chat_id: Id.t,
    )
    : unit => {
  let prompt = "Generate a short, concise chat title (3-6 words max) that captures the essence of what the user is asking or working on. Respond with ONLY the title text, nothing else. No quotes, no punctuation at the end, no explanation.";
  let payload =
    OpenRouter.Payload.Utils.mk_default(
      ~model_id=chat_naming_model_id,
      ~messages=[
        OpenRouter.Message.Utils.mk_system_msg(prompt),
        OpenRouter.Message.Utils.mk_user_msg(user_message),
      ],
      ~tools=[],
      (),
    );
  let handler = (response: option(API.Json.t)): unit => {
    switch (OpenRouter.Utils.handle_chat(response)) {
    | Some(OpenRouter.Model.Reply(reply)) =>
      let title = String.trim(reply.content);
      if (String.length(title) > 0 && String.length(title) < 80) {
        schedule_action(Action.HandleChatNamingResponse(title, chat_id));
      };
    | _ => ()
    };
  };
  OpenRouter.Utils.start_chat(~key=api_key, ~payload, ~handler);
};

let send_llm_request =
    (
      ~api_key: string,
      ~payload: OpenRouter.Payload.Model.t,
      ~schedule_action: Action.t => unit,
      ~chat_id: Id.t,
      ~retry_attempt: int,
      ~main_flight_seq: int,
    )
    : unit => {
  let send_started_at = JsUtil.timestamp();
  let acc = OpenRouter.Utils.StreamAccumulator.create();
  let on_chunk = (chunk: API.Json.t): unit => {
    let {content_delta, reasoning_delta}: OpenRouter.Utils.StreamAccumulator.delta =
      OpenRouter.Utils.StreamAccumulator.feed(acc, chunk);
    /* Suppress empty deltas (many providers emit role-only or
       keepalive-style chunks). StreamDelta gating by flight_seq still
       happens in the reducer. */
    if (content_delta != "" || reasoning_delta != "") {
      schedule_action(
        Action.StreamDelta(
          chat_id,
          main_flight_seq,
          content_delta,
          reasoning_delta,
        ),
      );
    };
  };
  let on_done = (): unit => {
    pending_main_stream_handle := None;
    let elapsed_ms = int_of_float(JsUtil.timestamp() -. send_started_at);
    switch (OpenRouter.Utils.StreamAccumulator.finalize(acc)) {
    | OpenRouter.Model.Reply(reply) =>
      /* Stamp the requested model id onto usage so the per-message
         metadata UI and the footer cache indicator can tell which
         model produced this reply (Anthropic caches don't carry
         across models). */
      let reply = {
        ...reply,
        usage:
          Option.map(
            (u: OpenRouter.Reply.Model.usage) =>
              {
                ...u,
                model_id: Some(payload.model_id),
              },
            reply.usage,
          ),
      };
      schedule_action(
        Action.HandleLLMResponse(reply, chat_id, main_flight_seq, elapsed_ms),
      );
    | OpenRouter.Model.Error({message, code}) =>
      if (is_retryable_api_error(code) && retry_attempt < max_api_retries) {
        schedule_action(Action.RetryApiError(chat_id, retry_attempt));
      } else {
        let api_error_content = format_api_error_content(~code, ~message);
        let api_error_message =
          Message.Utils.mk_api_failure_message(api_error_content);
        schedule_action(
          Action.ApiErrorResponse(
            chat_id,
            api_error_message,
            MainRequest(main_flight_seq),
          ),
        );
      }
    };
  };
  let handle =
    OpenRouter.Utils.start_streaming_chat(
      ~payload,
      ~key=api_key,
      ~on_chunk,
      ~on_done,
    );
  pending_main_stream_handle := Some(handle);
};

/** True while a send/response cycle or compaction is active, including the
    phase-1 → phase-2 dispatch gap. New user sends queue instead of firing. */
let busy_for_send = (model: Model.t): bool =>
  Option.is_some(model.compaction_in_progress)
  || Option.is_some(model.awaiting_response)
  || Option.is_some(model.pending_dispatch_send);

let enqueue_while_busy =
    (model: Model.t, chat_id: Id.t, text: string): Model.t => {
  let trimmed = String.trim(text);
  if (trimmed == "") {
    model;
  } else {
    let chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
    let chat' = {
      ...chat,
      pending_send_queue: chat.pending_send_queue @ [trimmed],
    };
    {
      ...model,
      chat_system: ChatSystem.Utils.update_chat(chat', model.chat_system),
    };
  };
};

/** Phase 2 of a send: the message is already on the chat (appended by
    SendMessage), so this only builds the payload from the chat history and
    fires the request — no append here. */
let dispatch_send =
    (
      ~api_key: option(string),
      ~llm_id: option(string),
      ~reasoning_effort: option(OpenRouter.Payload.Model.effort_level),
      ~session_mode: AgentGlobals.Model.session_mode,
      chat_id: Id.t,
      model: Model.t,
      schedule_action: Action.t => unit,
    )
    : Model.t => {
  switch (api_key, llm_id) {
  | (None, _) =>
    Utils.append_message(
      ~chat_id,
      Message.Utils.mk_api_failure_message(
        "An API key is required. Please set an API key in the settings.",
      ),
      model,
    )
  | (_, None) =>
    Utils.append_message(
      ~chat_id,
      Message.Utils.mk_api_failure_message(
        "LLM ID is required. Please select an LLM in the settings.",
      ),
      model,
    )
  | (Some(api_key), Some(llm_id)) =>
    let main_flight_seq = model.main_llm_seq + 1;
    send_llm_request(
      ~api_key,
      ~payload=
        OpenRouter.Payload.Utils.mk_default(
          ~model_id=llm_id,
          ~messages=
            Chat.Utils.api_messages_for_openrouter(
              ChatSystem.Utils.find_chat(chat_id, model.chat_system),
            ),
          ~session_id=Some(Id.to_string(chat_id)),
          ~tools=enabled_tools(~mode=session_mode, model.prompting),
          ~reasoning=?
            Option.map(
              e => OpenRouter.Payload.Model.Effort(e),
              reasoning_effort,
            ),
          (),
        ),
      ~schedule_action,
      ~chat_id,
      ~retry_attempt=0,
      ~main_flight_seq,
    );
    let current_chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
    let tail = Chat.Utils.current_tail(current_chat);
    if (current_chat.title == "New Chat" && tail.role == User) {
      if (Option.is_none(model.awaiting_response)
          && Option.is_none(model.compaction_in_progress)) {
        request_chat_name(
          ~api_key,
          ~user_message=tail.content,
          ~schedule_action,
          ~chat_id,
        );
      };
    };
    {
      ...model,
      awaiting_response: Some(chat_id),
      main_llm_seq: main_flight_seq,
    };
  };
};

/** Start the next model turn after tool result messages are already on the chat (no extra append). */
let dispatch_follow_up_llm =
    (
      model: Model.t,
      chat_id: Id.t,
      settings: Settings.t,
      schedule_action: Action.t => unit,
    )
    : Model.t => {
  switch (model.compaction_in_progress) {
  | Some(_) => model
  | None =>
    switch (
      settings.agent_globals.api_key,
      AgentGlobals.get_active_llm_id(settings.agent_globals),
    ) {
    | (None, _) => {
        ...
          Utils.append_message(
            ~chat_id,
            Message.Utils.mk_api_failure_message(
              "An API key is required. Please set an API key in the settings.",
            ),
            model,
          ),
        awaiting_response: None,
      }
    | (_, None) => {
        ...
          Utils.append_message(
            ~chat_id,
            Message.Utils.mk_api_failure_message(
              "LLM ID is required. Please select an LLM in the settings.",
            ),
            model,
          ),
        awaiting_response: None,
      }
    | (Some(api_key), Some(llm_id)) =>
      let main_flight_seq = model.main_llm_seq + 1;
      send_llm_request(
        ~api_key,
        ~payload=
          OpenRouter.Payload.Utils.mk_default(
            ~model_id=llm_id,
            ~messages=
              Chat.Utils.api_messages_for_openrouter(
                ChatSystem.Utils.find_chat(chat_id, model.chat_system),
              ),
            ~session_id=Some(Id.to_string(chat_id)),
            ~tools=
              enabled_tools(
                ~mode=settings.agent_globals.session_mode,
                model.prompting,
              ),
            ~reasoning=?
              Option.map(
                e => OpenRouter.Payload.Model.Effort(e),
                settings.agent_globals.reasoning_effort,
              ),
            (),
          ),
        ~schedule_action,
        ~chat_id,
        ~retry_attempt=0,
        ~main_flight_seq,
      );
      {
        ...model,
        awaiting_response: Some(chat_id),
        main_llm_seq: main_flight_seq,
      };
    }
  };
};

let schedule_flush_pending_if_idle_for_chat =
    (model_after: Model.t, chat_id: Id.t, schedule_action: Action.t => unit)
    : unit =>
  if (busy_for_send(model_after)) {
    ();
  } else {
    let chat = ChatSystem.Utils.find_chat(chat_id, model_after.chat_system);
    if (chat.pending_send_queue != []) {
      schedule_action(Action.FlushPendingSend(chat_id));
    };
  };

/* -- Update-arm bodies (see [AgentUpdate.update] dispatcher) ------------ */

/** Phase 1 of a send (SendMessage): append (or queue) the user message. */
let send_message =
    (
      message: Message.Model.t,
      chat_id: Id.t,
      model: Model.t,
      editor: CellEditor.Model.t,
      schedule_action: Action.t => unit,
    )
    : (Model.t, Updated.t(CellEditor.Model.t)) => {
  let user_trying_new_round =
    switch (message.role) {
    | User => true
    | _ => false
    };
  if (user_trying_new_round && busy_for_send(model)) {
    (
      enqueue_while_busy(model, chat_id, message.content),
      editor |> Updated.return,
    );
  } else {
    /* Phase 1: append only, so the message paints this frame; DispatchSend
       picks up the context refresh and payload work a macrotask later. */
    let model = {
      ...Utils.append_message(~chat_id, message, model),
      pending_dispatch_send: Some(chat_id),
    };
    defer_dispatch_send^(() =>
      schedule_action(Action.DispatchSend(chat_id))
    );
    (model, editor |> Updated.return);
  };
};

/** Phase 2 of a send (DispatchSend): context refresh + payload + request. */
let handle_dispatch_send =
    (
      chat_id: Id.t,
      model: Model.t,
      editor: CellEditor.Model.t,
      settings: Settings.t,
      schedule_action: Action.t => unit,
    )
    : (Model.t, Updated.t(CellEditor.Model.t)) =>
  switch (model.pending_dispatch_send) {
  | Some(pending_chat_id) when pending_chat_id == chat_id =>
    let model = {
      ...model,
      pending_dispatch_send: None,
    };
    let model =
      Utils.update_context(
        ~session_mode=settings.agent_globals.session_mode,
        ~test_results=?EvalResult.Model.test_results(editor.result),
        model,
        editor.editor,
        chat_id,
      );
    (
      dispatch_send(
        ~api_key=settings.agent_globals.api_key,
        ~llm_id=AgentGlobals.get_active_llm_id(settings.agent_globals),
        ~reasoning_effort=settings.agent_globals.reasoning_effort,
        ~session_mode=settings.agent_globals.session_mode,
        chat_id,
        model,
        schedule_action,
      ),
      editor |> Updated.return,
    );
  | _ =>
    /* Stale: Stop (or a competing dispatch) already consumed the flag. */
    (model, editor |> Updated.return_quiet)
  };

let stop_agentic_loop =
    (
      model: Model.t,
      editor: CellEditor.Model.t,
      schedule_action: Action.t => unit,
    )
    : (Model.t, Updated.t(CellEditor.Model.t)) => {
  let (m, e) =
    switch (model.awaiting_response, model.compaction_in_progress) {
    | (Some(awaiting_chat_id), _) =>
      /* Abort the in-flight XHR so the user stops paying for generation.
         The seq gate still catches any delta that slipped through before
         [abort] took effect. */
      abort_main_stream_handle();
      /* Append cancel immediately so it stays above e.g. a flushed
         queued user message (late HTTP still uses pending_ignore_*). */
      let cancelled =
        Message.Utils.mk_response_cancelled_message(
          ~content="Agent response cancelled.",
        );
      (
        Utils.clear_pending_assistant_stream({
          ...
            Utils.append_message(~chat_id=awaiting_chat_id, cancelled, model),
          awaiting_response: None,
          last_empty_retry_attempt: None,
          last_active_task_nudge_attempt: None,
          pending_ignore_main_reply_seq: Some(model.main_llm_seq),
        }),
        editor |> Updated.return,
      );
    | (None, Some(compaction_chat_id)) =>
      AgentCompaction.abort_compaction_stream_handle();
      let cancelled =
        Message.Utils.mk_response_cancelled_message(
          ~content="Compaction cancelled.",
        );
      (
        {
          ...
            Utils.append_message(
              ~chat_id=compaction_chat_id,
              cancelled,
              model,
            ),
          compaction_in_progress: None,
          compaction_method_override: None,
          pending_ignore_compaction_reply_seq: Some(model.compaction_llm_seq),
        },
        editor |> Updated.return,
      );
    | (None, None) =>
      switch (model.pending_dispatch_send) {
      | Some(pending_chat_id) =>
        /* Stop landed in the phase-1 → phase-2 gap: drop the pending
           dispatch so DispatchSend no-ops. */
        let cancelled =
          Message.Utils.mk_response_cancelled_message(
            ~content="Agent response cancelled.",
          );
        (
          {
            ...
              Utils.append_message(~chat_id=pending_chat_id, cancelled, model),
            pending_dispatch_send: None,
          },
          editor |> Updated.return,
        );
      | None => (model, editor |> Updated.return)
      }
    };
  schedule_flush_pending_if_idle_for_chat(
    m,
    m.chat_system.current,
    schedule_action,
  );
  (m, e);
};

let flush_pending_send =
    (
      chat_id: Id.t,
      model: Model.t,
      editor: CellEditor.Model.t,
      schedule_action: Action.t => unit,
    )
    : (Model.t, Updated.t(CellEditor.Model.t)) =>
  if (busy_for_send(model)) {
    (model, editor |> Updated.return);
  } else {
    let chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
    switch (chat.pending_send_queue) {
    | [] => (model, editor |> Updated.return)
    | [first, ...rest] =>
      let chat' = {
        ...chat,
        pending_send_queue: rest,
      };
      let model' = {
        ...model,
        chat_system: ChatSystem.Utils.update_chat(chat', model.chat_system),
      };
      schedule_action(
        Action.SendMessage(Message.Utils.mk_user_message(first), chat_id),
      );
      (model', editor |> Updated.return);
    };
  };

let handle_api_error_response =
    (
      chat_id: Id.t,
      api_error_message: Message.Model.t,
      origin: llm_error_origin,
      model: Model.t,
      editor: CellEditor.Model.t,
      schedule_action: Action.t => unit,
    )
    : (Model.t, Updated.t(CellEditor.Model.t)) => {
  /* Any partial stream is now moot; clear it before branching so every
     return path below is safe. */
  let model =
    switch (origin) {
    | MainRequest(_) => Utils.clear_pending_assistant_stream(model)
    | CompactionRequest(_) => model
    };
  let (m, e) =
    switch (origin) {
    | MainRequest(seq) =>
      switch (model.pending_ignore_main_reply_seq) {
      | Some(ig) when ig == seq => (
          {
            ...model,
            pending_ignore_main_reply_seq: None,
            awaiting_response:
              model.main_llm_seq > seq ? model.awaiting_response : None,
          },
          editor |> Updated.return,
        )
      | _ => (
          {
            ...Utils.append_message(~chat_id, api_error_message, model),
            awaiting_response: None,
          },
          editor |> Updated.return,
        )
      }
    | CompactionRequest(seq) =>
      switch (model.pending_ignore_compaction_reply_seq) {
      | Some(ig) when ig == seq => (
          {
            ...model,
            pending_ignore_compaction_reply_seq: None,
            compaction_in_progress:
              model.compaction_llm_seq > seq
                ? model.compaction_in_progress : None,
            compaction_method_override:
              model.compaction_llm_seq > seq
                ? model.compaction_method_override : None,
          },
          editor |> Updated.return,
        )
      | _ => (
          {
            ...Utils.append_message(~chat_id, api_error_message, model),
            awaiting_response: None,
            compaction_in_progress:
              switch (model.compaction_in_progress) {
              | Some(id) when id == chat_id => None
              | c => c
              },
            compaction_method_override:
              switch (model.compaction_in_progress) {
              | Some(id) when id == chat_id => None
              | _ => model.compaction_method_override
              },
          },
          editor |> Updated.return,
        )
      }
    };
  schedule_flush_pending_if_idle_for_chat(m, chat_id, schedule_action);
  (m, e);
};

let retry_api_error =
    (
      chat_id: Id.t,
      attempt: int,
      model: Model.t,
      editor: CellEditor.Model.t,
      schedule_action: Action.t => unit,
    )
    : (Model.t, Updated.t(CellEditor.Model.t)) => {
  let delay_s = int_of_float(backoff_ms(attempt) /. 1000.0);
  let retry_note =
    Message.Utils.mk_retry_note_message(
      ~content=
        "[API retry "
        ++ string_of_int(attempt + 2)
        ++ "/"
        ++ string_of_int(max_api_retries + 1)
        ++ "] Server/rate limit error. Retrying in "
        ++ string_of_int(delay_s)
        ++ "s...",
      ~sent_to_api=false,
      ~deliver_as_user_on_api=false,
    );
  let delay_ms = backoff_ms(attempt);
  JsUtil.delay(delay_ms, () =>
    schedule_action(Action.DoRetryApiSend(chat_id, attempt))
  );
  (
    Utils.append_message(~chat_id, retry_note, model),
    editor |> Updated.return,
  );
};

let do_retry_api_send =
    (
      chat_id: Id.t,
      attempt: int,
      model: Model.t,
      editor: CellEditor.Model.t,
      settings: Settings.t,
      schedule_action: Action.t => unit,
    )
    : (Model.t, Updated.t(CellEditor.Model.t)) => {
  let model =
    Utils.update_context(
      ~session_mode=settings.agent_globals.session_mode,
      ~test_results=?EvalResult.Model.test_results(editor.result),
      model,
      editor.editor,
      chat_id,
    );
  if (Option.is_some(model.pending_ignore_main_reply_seq)) {
    (
      {
        ...model,
        pending_ignore_main_reply_seq: None,
        awaiting_response: None,
      },
      editor |> Updated.return,
    );
  } else {
    let main_flight_seq = model.main_llm_seq + 1;
    let chat_system = model.chat_system;
    switch (
      settings.agent_globals.api_key,
      AgentGlobals.get_active_llm_id(settings.agent_globals),
    ) {
    | (Some(api_key), Some(llm_id)) =>
      send_llm_request(
        ~api_key,
        ~payload=
          OpenRouter.Payload.Utils.mk_default(
            ~model_id=llm_id,
            ~messages=
              Chat.Utils.api_messages_for_openrouter(
                ChatSystem.Utils.find_chat(chat_id, chat_system),
              ),
            ~session_id=Some(Id.to_string(chat_id)),
            ~tools=
              enabled_tools(
                ~mode=settings.agent_globals.session_mode,
                model.prompting,
              ),
            ~reasoning=?
              Option.map(
                e => OpenRouter.Payload.Model.Effort(e),
                settings.agent_globals.reasoning_effort,
              ),
            (),
          ),
        ~schedule_action,
        ~chat_id,
        ~retry_attempt=attempt + 1,
        ~main_flight_seq,
      );
      (
        {
          ...model,
          main_llm_seq: main_flight_seq,
        },
        editor |> Updated.return,
      );
    | _ =>
      let api_failure_message =
        Message.Utils.mk_api_failure_message(
          "API key or LLM not configured. Cannot retry.",
        );
      (
        {
          ...Utils.append_message(~chat_id, api_failure_message, model),
          awaiting_response: None,
        },
        editor |> Updated.return,
      );
    };
  };
};

let retry_empty_response =
    (
      chat_id: Id.t,
      attempt: int,
      model: Model.t,
      editor: CellEditor.Model.t,
      settings: Settings.t,
      schedule_action: Action.t => unit,
    )
    : (Model.t, Updated.t(CellEditor.Model.t)) =>
  if (Option.is_some(model.pending_ignore_main_reply_seq)) {
    (
      {
        ...model,
        pending_ignore_main_reply_seq: None,
        awaiting_response: None,
        last_empty_retry_attempt: None,
      },
      editor |> Updated.return,
    );
  } else {
    let retry_msg =
      "[Retry "
      ++ string_of_int(attempt + 1)
      ++ "/"
      ++ string_of_int(max_empty_retries)
      ++ "] Your previous assistant message had no visible text (empty or whitespace-only). That is not allowed.\n"
      ++ "MANDATORY: Reply now with at least one full sentence addressed to the user — acknowledge what they asked for, or summarize what you changed or attempted. Do not send an empty message again.\n"
      ++ "If you started a workbench plan and it is still open, update or close it when it matches your intent (e.g. mark_active_task_complete, mark_active_task_failed, or subtask tools). In the same turn or the next, write the required user-facing sentence.\n"
      ++ "Never end with both empty text and no tool calls.";
    let retry_message =
      Message.Utils.mk_retry_note_message(
        ~content=retry_msg,
        ~sent_to_api=true,
        ~deliver_as_user_on_api=true,
      );
    let model = Utils.append_message(~chat_id, retry_message, model);
    switch (
      settings.agent_globals.api_key,
      AgentGlobals.get_active_llm_id(settings.agent_globals),
    ) {
    | (Some(api_key), Some(llm_id)) =>
      let main_flight_seq = model.main_llm_seq + 1;
      send_llm_request(
        ~api_key,
        ~payload=
          OpenRouter.Payload.Utils.mk_default(
            ~model_id=llm_id,
            ~messages=
              Chat.Utils.api_messages_for_openrouter(
                ChatSystem.Utils.find_chat(chat_id, model.chat_system),
              ),
            ~session_id=Some(Id.to_string(chat_id)),
            ~tools=
              enabled_tools(
                ~mode=settings.agent_globals.session_mode,
                model.prompting,
              ),
            ~reasoning=?
              Option.map(
                e => OpenRouter.Payload.Model.Effort(e),
                settings.agent_globals.reasoning_effort,
              ),
            (),
          ),
        ~schedule_action,
        ~chat_id,
        ~retry_attempt=0,
        ~main_flight_seq,
      );
      (
        {
          ...model,
          main_llm_seq: main_flight_seq,
        },
        editor |> Updated.return,
      );
    | _ =>
      let api_failure_message =
        Message.Utils.mk_api_failure_message(
          "API key or LLM not configured. Cannot retry.",
        );
      (
        {
          ...Utils.append_message(~chat_id, api_failure_message, model),
          awaiting_response: None,
          last_empty_retry_attempt: None,
        },
        editor |> Updated.return,
      );
    };
  };
