/* Message-role policy (see [[CompositionPrompt.message_channels]]):
   developer notes serialize as system (no developer role on the wire);
   the context snapshot is one structured message refreshed in place via
   [[Chat.Utils.update_context]] — never per-error system messages;
   protocol nudges stay [[System(RetryNote)]] in the UI but are sent as
   user ([deliver_as_user_on_api]); [api_message: None] marks UI-only. */

include AgentModel;

module ToolUtils = AgentToolUtils;
module Utils = AgentUtils;
module ToolCallHandler = AgentToolCallHandler;
module Update = AgentUpdate;
