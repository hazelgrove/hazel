open Util;
open Haz3lcore;
open Ppx_yojson_conv_lib.Yojson_conv;
open AgentModel;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | ChatSystemAction(ChatSystem.Update.Action.t)
  | /** Run one edit tool outside the chat loop (canvas authoring): same
        executor, guardrails, and formatting as agent edits, but no chat
        message or tool-result bookkeeping. */
    DirectEdit(
      string,
      API.Json.t,
    )
  | /** Phase 1 of a send: append the message so it paints immediately;
        the expensive context/payload work is deferred to DispatchSend. */
    SendMessage(
      Message.Model.t,
      Id.t,
    )
  | /** Phase 2 of a send, scheduled from SendMessage via a 0ms timeout so
        the browser paints between the phases. */
    DispatchSend(
      Id.t,
    )
  | /** Last [int] is elapsed wall-time (ms) from send to reply,
        used to render "Thought for Ns" on reasoning-bearing turns. */
    HandleLLMResponse(
      OpenRouter.Reply.Model.t,
      Id.t,
      int,
      int,
    )
  | HandleCompactionLLMReply(OpenRouter.Reply.Model.t, Id.t, int)
  | /** Replay a recorded reply's tool calls through the same handler a
        real reply goes through (canvas trajectory replay; no LLM). */
    ReplayToolCalls(
      list(OpenRouter.Reply.Model.tool_call),
    )
  | /** Replay one streamed-reasoning render (canvas trajectory replay). */
    ReplayStreamTick
  | ReplayBegin(string) /* a replayed trajectory opens as a user turn */
  | HandleChatNamingResponse(string, Id.t)
  | ApiErrorResponse(Id.t, Message.Model.t, llm_error_origin)
  | RetryApiError(Id.t, int)
  | DoRetryApiSend(Id.t, int)
  | RetryEmptyResponse(Id.t, int)
  | LoadTimelineSegment(Segment.t, int)
  | RestoreOriginal
  | LoadSegmentIntoEditor(Segment.t)
  | SetActiveTimelineNode(option(int))
  | SetToolEnabled(string, bool)
  | SetToolsInCategoryEnabled(string, bool)
  | ToggleToolsViewExpanded(string)
  | RequestForcedCompaction(Id.t)
  | StopAgenticLoop
  | CatchUpAgent
  | FlushPendingSend(Id.t)
  | RunSlashCommandCost(Id.t)
  | RunSlashCommandHelp(Id.t)
  | RunSlashCommandShowKey(Id.t)
  | RunSlashCommandFetchCredits(Id.t)
  | RunSlashCommandFetchUsage(Id.t)
  | AppendSlashCommandOutput(Id.t, Message.Model.slash_command_payload)
  | /** Incremental SSE delta from the main agent stream. Carries
        (chat_id, flight_seq, content_delta, reasoning_delta). Dropped
        when [pending_ignore_main_reply_seq] matches [flight_seq]. */
    StreamDelta(
      Id.t,
      int,
      string,
      string,
    );

/* Actions that consume the program need the live, spliced editor while
   definitions are focused. Streaming/chat-only actions must stay cheap. */
let uses_program = (action: t): bool =>
  switch (action) {
  | DirectEdit(_)
  | SendMessage(_)
  | DispatchSend(_)
  | HandleLLMResponse(_)
  | HandleCompactionLLMReply(_)
  | ReplayToolCalls(_)
  | ApiErrorResponse(_)
  | RetryApiError(_)
  | DoRetryApiSend(_)
  | RetryEmptyResponse(_)
  | LoadTimelineSegment(_)
  | RestoreOriginal
  | LoadSegmentIntoEditor(_)
  | RequestForcedCompaction(_)
  | StopAgenticLoop
  | CatchUpAgent
  | FlushPendingSend(_) => true
  | ChatSystemAction(_)
  | ReplayStreamTick
  | ReplayBegin(_)
  | HandleChatNamingResponse(_)
  | SetActiveTimelineNode(_)
  | SetToolEnabled(_)
  | SetToolsInCategoryEnabled(_)
  | ToggleToolsViewExpanded(_)
  | RunSlashCommandCost(_)
  | RunSlashCommandHelp(_)
  | RunSlashCommandShowKey(_)
  | RunSlashCommandFetchCredits(_)
  | RunSlashCommandFetchUsage(_)
  | AppendSlashCommandOutput(_)
  | StreamDelta(_) => false
  };
