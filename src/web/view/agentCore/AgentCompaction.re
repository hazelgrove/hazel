open Util_web;
open Haz3lcore;
open AgentModel;

module Utils = AgentUtils;
module Action = AgentAction;

/* In-flight compaction stream handle. Not part of Model.t because
   [API.streaming_handle] holds a closure and is not serializable; strictly
   transient and meaningless across reloads. Cleared in the stream's
   [on_done] and on Stop. */
let pending_compaction_stream_handle: ref(option(API.streaming_handle)) =
  ref(None);

let abort_compaction_stream_handle = (): unit =>
  switch (pending_compaction_stream_handle^) {
  | Some(h) =>
    pending_compaction_stream_handle := None;
    h.abort();
  | None => ()
  };

/** Same [[context]] payload the main agent sees ([[mk_context_message]]), built from the
    live editor and [[agent_view]] — appended last to the compaction API so the summarizer
    has current program text, errors, tests, and workbench (without changing UI state). */
let compaction_context_snapshot_message =
    (
      ~session_mode: AgentGlobals.Model.session_mode,
      model: Model.t,
      cell_editor: CellEditor.Model.t,
      chat_id: Id.t,
    )
    : Message.Model.t => {
  let curr_chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
  Message.Utils.mk_context_system_message(
    Utils.llm_context_snapshot_text(
      ~session_mode,
      ~cell_result=cell_editor.result,
      cell_editor.editor,
      curr_chat,
    ),
  );
};

let compaction_summary_method_label = "Model-generated summary";

let send_compaction_request =
    (
      ~api_key: string,
      ~llm_id: string,
      ~messages: list(OpenRouter.Message.Model.t),
      ~schedule_action: Action.t => unit,
      ~chat_id: Id.t,
      ~compaction_flight_seq: int,
    )
    : unit => {
  let payload =
    OpenRouter.Payload.Utils.mk_default(
      ~model_id=llm_id,
      ~messages,
      ~tools=[],
      ~session_id=Some(Id.to_string(chat_id)),
      (),
    );
  let acc = OpenRouter.Utils.StreamAccumulator.create();
  let on_chunk = (chunk: API.Json.t): unit => {
    let _delta = OpenRouter.Utils.StreamAccumulator.feed(acc, chunk);
    /* Compaction surfaces nothing mid-stream; deltas are silently buffered
       and delivered all at once via HandleCompactionLLMReply on done. */
    ();
  };
  let on_done = (): unit => {
    pending_compaction_stream_handle := None;
    switch (OpenRouter.Utils.StreamAccumulator.finalize(acc)) {
    | OpenRouter.Model.Reply(reply) =>
      schedule_action(
        Action.HandleCompactionLLMReply(
          reply,
          chat_id,
          compaction_flight_seq,
        ),
      )
    | OpenRouter.Model.Error({message, code}) =>
      let api_error_content =
        "Compaction failed (code " ++ string_of_int(code) ++ "): " ++ message;
      let api_error_message =
        Message.Utils.mk_api_failure_message(api_error_content);
      schedule_action(
        Action.ApiErrorResponse(
          chat_id,
          api_error_message,
          CompactionRequest(compaction_flight_seq),
        ),
      );
    };
  };
  let handle =
    OpenRouter.Utils.start_streaming_chat(
      ~payload,
      ~key=api_key,
      ~on_chunk,
      ~on_done,
    );
  pending_compaction_stream_handle := Some(handle);
};

/** On the manual (/compact) path, surface [content] as a failure message;
    auto compaction stays silent. */
let compaction_unavailable =
    (~manual: bool, ~chat_id: Id.t, content: string, model: Model.t): Model.t =>
  manual
    ? Utils.append_message(
        ~chat_id,
        Message.Utils.mk_api_failure_message(content),
        model,
      )
    : model;

/** Shared auto (token limit) and manual (/compact) compaction kickoff. */
let maybe_start_compaction =
    (
      ~manual: bool,
      ~model: Model.t,
      ~chat_id: Id.t,
      ~settings: Settings.t,
      ~schedule_action: Action.t => unit,
      ~cell_editor: CellEditor.Model.t,
    )
    : Model.t =>
  if (Option.is_some(model.compaction_in_progress)) {
    compaction_unavailable(
      ~manual,
      ~chat_id,
      "Compaction is already in progress.",
      model,
    );
  } else if (Option.is_some(model.awaiting_response)
             || Option.is_some(model.pending_dispatch_send)) {
    compaction_unavailable(
      ~manual,
      ~chat_id,
      "Wait for the assistant to finish before compacting.",
      model,
    );
  } else {
    let chat = ChatSystem.Utils.find_chat(chat_id, model.chat_system);
    let dialogue = Chat.Utils.dialogue_slice_for_compaction_summary(chat);
    if (dialogue == []) {
      compaction_unavailable(
        ~manual,
        ~chat_id,
        "Nothing to compact yet.",
        model,
      );
    } else {
      let context_msg =
        compaction_context_snapshot_message(
          ~session_mode=settings.agent_globals.session_mode,
          model,
          cell_editor,
          chat_id,
        );
      /* Send the live program snapshot as a **user** message, not a second
         system message. Some providers return empty assistant text when the
         request ends with [system] after [assistant]. */
      let summary_api_msgs =
        [
          OpenRouter.Message.Utils.mk_system_msg(
            CompactionPrompt.mk_system_prompt(
              ~agent_system_prompt=model.prompting.system_prompt,
              ~dev_notes=model.prompting.dev_notes,
            ),
          ),
        ]
        @ List.filter_map(Message.Utils.api_message_of_message, dialogue)
        @ [
          OpenRouter.Message.Utils.mk_user_msg(
            "[Compaction: current Hazel program / workbench snapshot]\n\n"
            ++ context_msg.content,
          ),
        ];
      switch (
        settings.agent_globals.api_key,
        AgentGlobals.get_active_llm_id(settings.agent_globals),
      ) {
      | (Some(api_key), Some(llm_id)) =>
        let compaction_flight_seq = model.compaction_llm_seq + 1;
        send_compaction_request(
          ~api_key,
          ~llm_id,
          ~messages=summary_api_msgs,
          ~schedule_action,
          ~chat_id,
          ~compaction_flight_seq,
        );
        {
          ...model,
          compaction_in_progress: Some(chat_id),
          compaction_method_override:
            manual ? Some("Slash command (/compact)") : None,
          compaction_llm_seq: compaction_flight_seq,
        };
      | _ =>
        compaction_unavailable(
          ~manual,
          ~chat_id,
          "API key or LLM not configured. Cannot compact.",
          model,
        )
      };
    };
  };

/** Deliver a finished compaction reply: append the summary (or a failure
    note) and clear the in-progress markers. Seq-gated against Stop. */
let handle_compaction_reply =
    (
      reply: OpenRouter.Reply.Model.t,
      chat_id: Id.t,
      flight_seq: int,
      model: Model.t,
      editor: CellEditor.Model.t,
    )
    : (Model.t, Updated.t(CellEditor.Model.t)) =>
  switch (model.pending_ignore_compaction_reply_seq) {
  | Some(ignore_seq) when ignore_seq == flight_seq =>
    /* "Compaction cancelled." was already appended in StopAgenticLoop. */
    (
      {
        ...model,
        pending_ignore_compaction_reply_seq: None,
      },
      editor |> Updated.return,
    )
  | _ =>
    let method_label =
      Option.value(
        ~default=compaction_summary_method_label,
        model.compaction_method_override,
      );
    let model_cleared = {
      ...model,
      compaction_in_progress: None,
      compaction_method_override: None,
    };
    let content = String.trim(reply.content);
    if (content == "" && reply.tool_calls != []) {
      let err =
        Message.Utils.mk_api_failure_message(
          "Compaction returned tool calls instead of a text summary. Try another model, or one that does not emit tools on compaction.",
        );
      (
        Utils.append_message(~chat_id, err, model_cleared),
        editor |> Updated.return,
      );
    } else if (content == "") {
      let err =
        Message.Utils.mk_api_failure_message(
          "Compaction returned an empty summary.",
        );
      (
        Utils.append_message(~chat_id, err, model_cleared),
        editor |> Updated.return,
      );
    } else {
      let summary =
        Message.Utils.mk_compaction_summary(~method=method_label, content);
      (
        Utils.append_message(~chat_id, summary, model_cleared),
        editor |> Updated.return,
      );
    };
  };
