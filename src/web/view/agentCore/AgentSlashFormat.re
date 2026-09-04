open Util_web;
open Haz3lcore;

/* Slash-command result formatters and fetchers. Payload builders and their
   plain-text fallbacks live here; the command list itself is in
   [ChatSlashCommands] (which [ChatSystem] depends on, so anything needing
   [AgentAction] must live on this side of that boundary). */

/** Per-token pricing parsed from OpenRouter's [pricing.prompt]/[pricing.completion]
    strings (dollars per token). Returns 0.0 on parse failure — callers note the
    approximation when prices are missing. */
let pricing_per_token =
    (llm: option(OpenRouter.AvailableLLMs.Model.llm_info)): (float, float) =>
  switch (llm) {
  | None => (0.0, 0.0)
  | Some(info) =>
    let parse = s =>
      try(float_of_string(s)) {
      | _ => 0.0
      };
    (parse(info.pricing.prompt), parse(info.pricing.completion));
  };

/** Walk a chat and sum prompt/completion tokens across all Agent messages
    that carry usage. Returns (in_tokens, out_tokens). */
let chat_usage_totals = (chat: Chat.Model.t): (int, int) => {
  let messages = Chat.Utils.get(chat);
  List.fold_left(
    (acc, msg: Message.Model.t) =>
      switch (msg.role) {
      | Agent(Some(usage)) =>
        let (i, o) = acc;
        (i + usage.prompt_tokens, o + usage.completion_tokens);
      | _ => acc
      },
    (0, 0),
    messages,
  );
};

/** Build the typed payload for /cost from the current chat + active model. */
let cost_payload =
    (
      ~chat: Chat.Model.t,
      ~active_llm: option(OpenRouter.AvailableLLMs.Model.llm_info),
    )
    : Message.Model.cost_output => {
  let (in_tok, out_tok) = chat_usage_totals(chat);
  let (price_in, price_out) = pricing_per_token(active_llm);
  let estimated =
    switch (active_llm) {
    | Some(_) =>
      Some(
        float_of_int(in_tok)
        *. price_in
        +. float_of_int(out_tok)
        *. price_out,
      )
    | None => None
    };
  let model =
    switch (active_llm) {
    | Some(info) => info.id
    | None => ""
    };
  {
    cost_model: model,
    cost_input_tokens: in_tok,
    cost_output_tokens: out_tok,
    cost_estimated_usd: estimated,
  };
};

/** Plain-text one-liner used as the message's stored `content` (archival/copy). */
let cost_fallback_text = (p: Message.Model.cost_output): string => {
  let cost_str =
    switch (p.cost_estimated_usd) {
    | None => "(no model)"
    | Some(c) => Printf.sprintf("$%.4f", c)
    };
  let model_str = p.cost_model == "" ? "(no model)" : p.cost_model;
  Printf.sprintf(
    "Session cost: %d in / %d out tokens, est. %s (%s)",
    p.cost_input_tokens,
    p.cost_output_tokens,
    cost_str,
    model_str,
  );
};

let credits_payload =
    (credits: OpenRouter.Credits.Model.t): Message.Model.credits_output => {
  credits_used: credits.total_usage,
  credits_total: credits.total_credits,
};

let credits_fallback_text = (p: Message.Model.credits_output): string =>
  Printf.sprintf(
    "Credits: $%.2f used of $%.2f (~$%.2f remaining)",
    p.credits_used,
    p.credits_total,
    p.credits_total -. p.credits_used,
  );

let usage_payload =
    (k: OpenRouter.KeyInfo.Model.t): Message.Model.usage_output => {
  usage_label: k.label,
  usage_is_free_tier: k.is_free_tier,
  usage_total: k.usage,
  usage_daily: k.usage_daily,
  usage_weekly: k.usage_weekly,
  usage_monthly: k.usage_monthly,
  usage_limit: k.limit,
  usage_remaining: k.limit_remaining,
};

let usage_fallback_text = (p: Message.Model.usage_output): string =>
  Printf.sprintf(
    "Key usage: $%.2f total (%s)",
    p.usage_total,
    p.usage_is_free_tier ? "free" : "paid",
  );

let help_fallback_text = (p: Message.Model.help_output): string => {
  let names =
    List.map(
      (e: Message.Model.help_entry) => "/" ++ e.help_name,
      p.help_entries,
    );
  "Slash commands: " ++ String.concat(", ", names);
};

let key_fallback_text = (key: string): string =>
  key == "" ? "No OpenRouter API key set." : "OpenRouter API key: " ++ key;

/** Fire `/api/v1/credits`; on response (or failure), schedule
    [AppendSlashCommandOutput] so the result appears inline in the chat. */
let fetch_credits_for_slash =
    (
      ~api_key: string,
      ~chat_id: Id.t,
      ~schedule_action: AgentAction.t => unit,
    )
    : unit => {
  let handler = (response: option(API.Json.t)): unit => {
    let payload: Message.Model.slash_command_payload =
      switch (response) {
      | None =>
        SlashError(
          "Couldn't reach OpenRouter — check your network and API key.",
        )
      | Some(json) =>
        switch (OpenRouter.Credits.Utils.parse_credits_response(json)) {
        | Some(credits) => CreditsOutput(credits_payload(credits))
        | None =>
          SlashError(
            "OpenRouter responded but the credits payload was unrecognized.",
          )
        }
      };
    schedule_action(AgentAction.AppendSlashCommandOutput(chat_id, payload));
  };
  OpenRouter.Credits.Utils.get_credits(~key=api_key, ~handler);
};

let fetch_key_for_slash =
    (
      ~api_key: string,
      ~chat_id: Id.t,
      ~schedule_action: AgentAction.t => unit,
    )
    : unit => {
  let handler = (response: option(API.Json.t)): unit => {
    let payload: Message.Model.slash_command_payload =
      switch (response) {
      | None =>
        SlashError(
          "Couldn't reach OpenRouter — check your network and API key.",
        )
      | Some(json) =>
        switch (OpenRouter.KeyInfo.Utils.parse_key_response(json)) {
        | Some(key_info) => UsageOutput(usage_payload(key_info))
        | None =>
          SlashError(
            "OpenRouter responded but the key payload was unrecognized.",
          )
        }
      };
    schedule_action(AgentAction.AppendSlashCommandOutput(chat_id, payload));
  };
  OpenRouter.KeyInfo.Utils.get_key(~key=api_key, ~handler);
};
