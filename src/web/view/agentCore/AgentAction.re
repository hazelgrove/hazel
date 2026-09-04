open Util_web;
open Haz3lcore;
open Ppx_yojson_conv_lib.Yojson_conv;
open AgentModel;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | ChatSystemAction(ChatSystem.Update.Action.t)
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
