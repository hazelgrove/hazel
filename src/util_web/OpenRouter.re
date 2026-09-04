open Util;
open API;
open OptUtil.Syntax;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

module Reply = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type usage = {
      prompt_tokens: int,
      completion_tokens: int,
      total_tokens: int,
      [@yojson.default None] [@sexp.default None]
      cache_read_input_tokens: option(int),
      [@yojson.default None] [@sexp.default None]
      cache_creation_input_tokens: option(int),
      /* Ground truth for billing. [[prompt_tokens]] is a *reporting* field and
         is not what we are charged on — the two can disagree (observed on
         `google/gemini-3-flash-preview`, where prompt_tokens comes back at
         exactly 2x cache_read). [cost] is the amount OpenRouter actually
         billed for this request, in credits, so cache savings can be measured
         rather than inferred from token counts. */
      [@yojson.default None] [@sexp.default None]
      cost: option(float),
      /* BYOK requests only; the provider's own charge behind OpenRouter. */
      [@yojson.default None] [@sexp.default None]
      upstream_inference_cost: option(float),
      /* OpenRouter-normalized cache-write count from [prompt_tokens_details].
         Distinct from [[cache_creation_input_tokens]], which is the
         Anthropic-native field and is null on providers that cache
         implicitly. */
      [@yojson.default None] [@sexp.default None]
      cache_write_tokens: option(int),
      [@yojson.default None] [@sexp.default None]
      model_id: option(string),
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type tool_call = {
      id: string,
      name: string,
      args: Json.t,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      content: string,
      tool_calls: list(tool_call),
      usage: option(usage),
      [@yojson.default None] [@sexp.default None]
      reasoning: option(string),
    };
  };
};

module Message = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type role =
      | System
      | Developer
      | User
      | Assistant
      | Tool(Reply.Model.tool_call);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      role,
      content: string,
      tool_calls: list(Reply.Model.tool_call),
      /* Request a prompt-cache breakpoint on this message. Set where the
         Message-level role is known (the stable system-prompt/dev-notes
         prefix), never on the volatile context snapshot. Defaulted so older
         persisted chats still deserialize. */
      [@yojson.default false] [@sexp.default false]
      cache_anchor: bool,
    };
  };

  module Utils = {
    let string_of_role =
      fun
      | Model.System => "system"
      | Model.Developer => "system"
      | Model.User => "user"
      | Model.Assistant => "assistant"
      | Model.Tool(_) => "tool";

    let json_of_tool_call = (tool_call: Reply.Model.tool_call): Json.t =>
      `Assoc([
        ("id", `String(tool_call.id)),
        ("type", `String("function")),
        (
          "function",
          `Assoc([
            ("name", `String(tool_call.name)),
            ("arguments", `String(Yojson.Safe.to_string(tool_call.args))),
          ]),
        ),
      ]);

    /* A prompt-cache breakpoint is requested when [cache_anchor] is set —
       either the static dev-notes floor (Phase 1) or the per-request advancing
       anchor on the last history message before the volatile snapshot (Phase 2,
       set in [Chat.Utils.api_messages_for_openrouter]).

       CRITICAL for the advancing breakpoint: the marker *moves* each request, so
       a message that was the anchor last request is no longer the anchor this
       request — yet its bytes must stay identical or the cached prefix written
       last request can't be read back this request (the prefix match fails and
       cache_read collapses to the floor). cache_control must ride a content
       *block*, which means array-shaped content. If we only array-shaped the
       currently-anchored message and left every other message as a plain string,
       the just-anchored message would flip array -> string the moment the anchor
       moved past it, changing its bytes and breaking the read. So we keep all
       non-blank content array-shaped *always*, and toggle only the cache_control
       key — exactly how the always-array dev-notes floor already behaves. Blank
       content stays a plain string: Anthropic rejects empty text blocks, and a
       blank message is consistently blank so its shape never flips. Thinking
       blocks are never sent back upstream here, so there is nothing to skip. */
    let json_of_message = (message: Model.t): Json.t => {
      let nonblank = String.trim(message.content) != "";
      let cache = message.cache_anchor && nonblank;
      /* Stable array shape for non-blank content; plain string only when blank. */
      let content_json: Json.t =
        if (nonblank) {
          let base = [
            ("type", `String("text")),
            ("text", `String(message.content)),
          ];
          let fields =
            cache
              ? base
                @ [
                  (
                    "cache_control",
                    `Assoc([("type", `String("ephemeral"))]),
                  ),
                ]
              : base;
          `List([`Assoc(fields)]);
        } else {
          `String(message.content);
        };
      switch (message.role) {
      | Assistant when message.tool_calls != [] =>
        /* Assistant-with-tool-calls is never the message before the snapshot (a
           tool result always follows it), so it never carries a breakpoint; keep
           the plain OpenAI shape, which is itself byte-stable across requests. */
        `Assoc([
          ("role", `String("assistant")),
          ("content", `String(message.content)),
          (
            "tool_calls",
            `List(List.map(json_of_tool_call, message.tool_calls)),
          ),
        ])
      | Tool(tool_call) =>
        `Assoc([
          ("role", `String(string_of_role(message.role))),
          ("content", content_json),
          ("tool_call_id", `String(tool_call.id)),
        ])
      | _ =>
        `Assoc([
          ("role", `String(string_of_role(message.role))),
          ("content", content_json),
        ])
      };
    };

    let mk_assistant_msg =
        (~tool_calls: list(Reply.Model.tool_call)=[], content: string)
        : Model.t => {
      role: Assistant,
      content,
      tool_calls,
      cache_anchor: false,
    };
    let mk_user_msg = (content: string): Model.t => {
      role: User,
      content,
      tool_calls: [],
      cache_anchor: false,
    };
    let mk_developer_msg = (content: string): Model.t => {
      role: Developer,
      content,
      tool_calls: [],
      cache_anchor: false,
    };
    let mk_system_msg = (content: string): Model.t => {
      role: System,
      content,
      tool_calls: [],
      cache_anchor: false,
    };
    let mk_tool_msg =
        (content: string, tool_call: Reply.Model.tool_call): Model.t => {
      role: Tool(tool_call),
      content,
      tool_calls: [],
      cache_anchor: false,
    };
  };
};

/** Prompt-cache diagnostics. For each request we measure the byte-stable common
    prefix of message *content* (role + content + tool-call ids, deliberately
    ignoring the moving [cache_control] marker) against the previous request, and
    pair it with the provider-reported [cache_read]/[cache_creation] from the
    response. The prefix should grow append-only every request; if it grows but
    [cache_read] stays flat (or [cache_creation] is null), our bytes are fine and
    the provider is dropping the breakpoint — vs a falling/jumpy prefix, which is
    a client-side serialization instability. Off by default; set [log := true] to
    re-enable per-request console logging when diagnosing a cache regression. */
module CacheDiag = {
  let log = ref(false);
  let prev_norm: ref(list(string)) = ref([]);
  let pending: ref(string) = ref("");

  let norm = (m: Message.Model.t): string =>
    Message.Utils.string_of_role(m.role)
    ++ "|"
    ++ m.content
    ++ "|"
    ++ String.concat(
         ",",
         List.map((tc: Reply.Model.tool_call) => tc.id, m.tool_calls),
       );

  let total_chars = List.fold_left((a, s) => a + String.length(s), 0);

  let common_prefix_chars = (a: list(string), b: list(string)): int => {
    let rec go = (a, b, acc) =>
      switch (a, b) {
      | ([x, ...xs], [y, ...ys]) when x == y =>
        go(xs, ys, acc + String.length(x))
      | _ => acc
      };
    go(a, b, 0);
  };

  let note_request =
      (
        ~model_id: string,
        ~cache_enabled: bool,
        ~breakpoints: list(string),
        messages: list(Message.Model.t),
      )
      : unit =>
    if (log^) {
      let normed = List.map(norm, messages);
      let prefix = common_prefix_chars(prev_norm^, normed);
      let total = total_chars(normed);
      prev_norm := normed;
      pending :=
        Printf.sprintf(
          "model=%s enabled=%b breakpoints=[%s] common_prefix=%dch payload=%dch",
          model_id,
          cache_enabled,
          String.concat(",", breakpoints),
          prefix,
          total,
        );
    };

  let note_response =
      (~cache_read: option(int), ~cache_creation: option(int)): unit =>
    if (log^) {
      let show =
        fun
        | Some(n) => string_of_int(n)
        | None => "null";
      print_endline(
        Printf.sprintf(
          "CACHE_DIAG %s | cache_read=%s cache_creation=%s",
          pending^,
          show(cache_read),
          show(cache_creation),
        ),
      );
    };
};

module Payload = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type effort_level =
      | Low
      | Medium
      | High;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type reasoning =
      | Effort(effort_level)
      | MaxTokens(int)
      | Exclude(bool);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      model_id: string,
      reasoning: option(reasoning),
      temperature: float,
      top_p: float,
      tools: list(Json.t),
      stream: bool,
      messages: list(Message.Model.t),
      /* Per-conversation id sent as a top-level [session_id] body field. Pins
         OpenRouter sticky routing to one provider from the first request so the
         growing prompt cache (which does not transfer between Anthropic, Bedrock,
         and Vertex) keeps hitting. Defaulted so older payloads still deserialize. */
      [@yojson.default None] [@sexp.default None]
      session_id: option(string),
    };
  };
  module Utils = {
    let string_of_effort_level =
      fun
      | Model.Low => "low"
      | Model.Medium => "medium"
      | Model.High => "high";

    let mk_default =
        (
          ~model_id: string,
          ~messages: list(Message.Model.t),
          ~tools: list(Json.t),
          ~reasoning: option(Model.reasoning)=?,
          ~session_id: option(string)=None,
          (),
        )
        : Model.t => {
      model_id,
      reasoning,
      temperature: 1.0,
      top_p: 1.0,
      tools,
      stream: false,
      messages,
      session_id,
    };

    let mk_reasoning = (reasoning: Model.reasoning): Json.t =>
      switch (reasoning) {
      | Effort(effort) =>
        `Assoc([("effort", `String(string_of_effort_level(effort)))])
      | MaxTokens(max_tokens) => `Assoc([("max_tokens", `Int(max_tokens))])
      | Exclude(exclude) => `Assoc([("exclude", `Bool(exclude))])
      };

    /* Explicit per-block cache_control breakpoints are honored on OpenRouter by
       the providers that support manual caching with the same Anthropic syntax:
       Anthropic (Claude), Google (Gemini), and Alibaba (Qwen). For these, the
       multipart content-block shape is the *required* form, not an error trigger.
       Auto-caching providers (OpenAI, DeepSeek, Grok) ignore the field, and
       OpenRouter strips it rather than erroring — but we still allowlist by
       provider prefix to avoid sending the multipart shape to a provider that
       might reject it. Add a prefix here when OpenRouter adds explicit-caching
       support for another provider family. */
    let cache_control_provider_prefixes = ["anthropic/", "google/", "qwen/"];
    let supports_cache_control = (model_id: string): bool =>
      List.exists(
        prefix => {
          let len = String.length(prefix);
          String.length(model_id) >= len
          && String.sub(model_id, 0, len) == prefix;
        },
        cache_control_provider_prefixes,
      );

    let json_of_payload = (~payload: Model.t): Json.t => {
      let cache_enabled = supports_cache_control(payload.model_id);
      /* Cache breakpoints are carried per-message via `cache_anchor`: the static
         dev-notes floor and the per-request advancing anchor on the last history
         message before the snapshot (set in Chat.Utils.api_messages_for_openrouter).
         The volatile context snapshot itself is always last and never anchored —
         caching it would pay the write premium for a hit we never get. Strip all
         anchors for non-Anthropic models, which don't honor cache_control and may
         400 on the multipart content shape. */
      let messages_json =
        List.map(
          (m: Message.Model.t) => {
            let m =
              cache_enabled
                ? m
                : {
                  ...m,
                  cache_anchor: false,
                };
            Message.Utils.json_of_message(m);
          },
          payload.messages,
        );
      let base_fields = [
        ("model", `String(payload.model_id)),
        ("temperature", `Float(payload.temperature)),
        ("top_p", `Float(payload.top_p)),
        ("tools", `List(payload.tools)),
        ("stream", `Bool(payload.stream)),
        ("messages", `List(messages_json)),
      ];
      let base_fields =
        switch (payload.session_id) {
        | Some(id) => [("session_id", `String(id)), ...base_fields]
        | None => base_fields
        };
      let fields =
        switch (payload.reasoning) {
        | Some(reasoning) => [
            ("reasoning", mk_reasoning(reasoning)),
            ...base_fields,
          ]
        | None => base_fields
        };
      /* Record which message indices carry a cache_control breakpoint on the
         wire, then stash the request-side metrics; the matching cache_read /
         cache_creation are logged together when the response usage is parsed. */
      let has_cc = (mj: Json.t): bool => {
        let s = Yojson.Safe.to_string(mj);
        let needle = "cache_control";
        let nlen = String.length(needle);
        let hlen = String.length(s);
        let rec go = i =>
          i > hlen - nlen
            ? false : String.sub(s, i, nlen) == needle ? true : go(i + 1);
        nlen <= hlen && go(0);
      };
      let breakpoints =
        List.mapi((i, mj) => (i, has_cc(mj)), messages_json)
        |> List.filter(((_, hit)) => hit)
        |> List.map(((i, _)) => string_of_int(i));
      CacheDiag.note_request(
        ~model_id=payload.model_id,
        ~cache_enabled,
        ~breakpoints,
        payload.messages,
      );
      `Assoc(fields);
    };
  };
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type error = {
    message: string,
    code: int,
  };

  type result =
    | Reply(Reply.Model.t)
    | Error(error);
};

module Utils = {
  let chat =
      (~key: string, ~body: Json.t, ~handler: option(Json.t) => unit): unit => {
    request(
      ~debug=false,
      ~with_credentials=false,
      ~method=POST,
      ~url="https://openrouter.ai/api/v1/chat/completions",
      ~headers=[
        ("Content-Type", "application/json"),
        ("Authorization", "Bearer " ++ key),
      ],
      ~body,
      handler,
    );
  };

  let start_chat =
      (
        ~payload: Payload.Model.t,
        ~key: string,
        ~handler: option(Json.t) => unit,
      )
      : unit => {
    let json_of_payload = Payload.Utils.json_of_payload(~payload);
    chat(~key, ~body=json_of_payload, ~handler);
  };

  /** Numeric field tolerant of ints and decimal strings. OpenRouter returns
      [cost] as a float, but an exact-zero charge deserializes as [`Int(0)],
      and some providers stringify small decimals — a plain float match would
      silently drop those to [None] and read as "free". */
  let num_field = (json: Json.t, field: string): option(float) =>
    switch (Json.dot(field, json)) {
    | Some(`Float(f)) => Some(f)
    | Some(`Int(n)) => Some(float_of_int(n))
    | Some(`String(s)) => float_of_string_opt(s)
    | _ => None
    };

  let of_usage = (choices: Json.t): option(Reply.Model.usage) => {
    let* prompt_tokens = API.Json.Parsers.int_field(choices, "prompt_tokens");
    let* completion_tokens =
      API.Json.Parsers.int_field(choices, "completion_tokens");
    let+ total_tokens = API.Json.Parsers.int_field(choices, "total_tokens");
    let cache_read_input_tokens =
      switch (API.Json.Parsers.int_field(choices, "cache_read_input_tokens")) {
      | Some(_) as v => v
      | None =>
        switch (Json.dot("prompt_tokens_details", choices)) {
        | Some(details) =>
          API.Json.Parsers.int_field(details, "cached_tokens")
        | None => None
        }
      };
    let cache_creation_input_tokens =
      API.Json.Parsers.int_field(choices, "cache_creation_input_tokens");
    let cache_write_tokens =
      switch (Json.dot("prompt_tokens_details", choices)) {
      | Some(details) =>
        API.Json.Parsers.int_field(details, "cache_write_tokens")
      | None => None
      };
    let cost = num_field(choices, "cost");
    let upstream_inference_cost =
      switch (Json.dot("cost_details", choices)) {
      | Some(details) => num_field(details, "upstream_inference_cost")
      | None => None
      };
    CacheDiag.note_response(
      ~cache_read=cache_read_input_tokens,
      ~cache_creation=cache_creation_input_tokens,
    );
    (
      {
        prompt_tokens,
        completion_tokens,
        total_tokens,
        cache_read_input_tokens,
        cache_creation_input_tokens,
        cost,
        upstream_inference_cost,
        cache_write_tokens,
        model_id: None,
      }: Reply.Model.usage
    );
  };

  /** Decode [content] the way several providers return it: plain string, null,
      array of parts (with [text], nested [content], or bare strings in the list),
      or a single object with [text]/[content]. */
  let rec message_content_string = (content: Json.t): option(string) => {
    switch (content) {
    | `Null => Some("")
    | `String(s) => Some(s)
    | `List(parts) =>
      let texts =
        List.filter_map(
          (part: Json.t) =>
            switch (part) {
            | `String(s) => Some(s)
            | _ =>
              switch (Json.dot("text", part)) {
              | Some(t) =>
                switch (Json.str(t)) {
                | Some(s) => Some(s)
                | None => message_content_string(t)
                }
              | None =>
                switch (Json.dot("content", part)) {
                | Some(c) => message_content_string(c)
                | None => None
                }
              }
            },
          parts,
        );
      Some(String.concat("", texts));
    | `Assoc(_) as assoc =>
      switch (Json.dot("text", assoc)) {
      | Some(t) =>
        switch (Json.str(t)) {
        | Some(s) => Some(s)
        | None => message_content_string(t)
        }
      | None =>
        switch (Json.dot("content", assoc)) {
        | Some(c) => message_content_string(c)
        | None => None
        }
      }
    | _ => None
    };
  };

  /** Extracts visible content and reasoning text from the first choice.
      Returns [(content, reasoning)]. If [content] is empty/missing but a
      reasoning-style field is present, falls back to surfacing it as content
      (legacy behavior for models that only emit reasoning). */
  let first_message_content_and_reasoning =
      (choices: Json.t): (option(string), option(string)) => {
    let extracted = {
      let* choices = Json.list(choices);
      let* hd = ListUtil.hd_opt(choices);
      let* delta =
        switch (Json.dot("message", hd)) {
        | Some(message) => Some(message)
        | None => Json.dot("delta", hd)
        };
      let from_content =
        switch (Json.dot("content", delta)) {
        | None => None
        | Some(c) => message_content_string(c)
        };
      let from_reasoning =
        Option.bind(Json.dot("reasoning", delta), Json.str);
      let from_reasoning_content =
        Option.bind(Json.dot("reasoning_content", delta), Json.str);
      let from_thinking = Option.bind(Json.dot("thinking", delta), Json.str);
      let nonempty = (o: option(string)): option(string) =>
        switch (o) {
        | Some(s) when String.trim(s) != "" => Some(s)
        | _ => None
        };
      let reasoning =
        List.find_map(
          nonempty,
          [from_reasoning_content, from_reasoning, from_thinking],
        );
      Some((nonempty(from_content), reasoning));
    };
    switch (extracted) {
    | None => (None, None)
    | Some((Some(_) as c, r)) => (c, r)
    | Some((None, Some(_) as r)) =>
      // Reasoning-only response: surface as content for back-compat; don't
      // double-render by also returning it under reasoning.
      (r, None)
    | Some((None, None)) => (None, None)
    };
  };

  let parse_tool_args = (args: Json.t): Json.t => {
    switch (args) {
    | `String(str) =>
      try(Yojson.Safe.from_string(str)) {
      | _ => args
      }
    | json => json
    };
  };

  let parse_tool_call = (tool_call: Json.t): option(Reply.Model.tool_call) => {
    let* id = Json.dot("id", tool_call);
    let* id = Json.str(id);

    let* tool_call = Json.dot("function", tool_call);

    let* name = Json.dot("name", tool_call);
    let* name = Json.str(name);
    let* args = Json.dot("arguments", tool_call);

    let parsed_args = parse_tool_args(args);

    let tool_call: Reply.Model.tool_call = {
      id,
      name,
      args: parsed_args,
    };
    Some(tool_call);
  };

  let parse_tool_calls = (choices: Json.t): list(Reply.Model.tool_call) => {
    let tool_calls = {
      let* choices = Json.list(choices);
      let* hd = ListUtil.hd_opt(choices);
      let* delta =
        switch (Json.dot("message", hd)) {
        | Some(message) => Some(message)
        | None => Json.dot("delta", hd)
        };

      let* tool_calls = Json.dot("tool_calls", delta);
      Json.list(tool_calls);
    };

    switch (tool_calls) {
    | Some(tool_calls) => List.filter_map(parse_tool_call, tool_calls)
    | None => []
    };
  };

  let parse_errs = (json: Json.t): option(Model.error) => {
    let* error = Json.dot("error", json);
    let* message = Json.dot("message", error);
    let* message = Json.str(message);
    let* code = Json.dot("code", error);
    let+ code = Json.int(code);
    (
      {
        message,
        code,
      }: Model.error
    );
  };

  /** Convert a failed HTTP exchange (non-2xx status, or a network error where
      [status] is 0) into a [Model.error]. OpenRouter sends JSON error bodies
      even on streaming endpoints; the provider's message/code are preferred
      when the body parses, with the HTTP status as fallback code. */
  let error_of_http_failure = (~status: int, ~body: string): Model.error => {
    let body_error = {
      let* json =
        try(Some(Json.from_string(body))) {
        | _ => None
        };
      Json.dot("error", json);
    };
    let message =
      switch (
        {
          let* e = body_error;
          let* m = Json.dot("message", e);
          Json.str(m);
        }
      ) {
      | Some(m) => m
      | None when status == 0 => "Network error: no response received"
      | None => Printf.sprintf("HTTP error %d", status)
      };
    let code =
      switch (
        {
          let* e = body_error;
          let* c = Json.dot("code", e);
          Json.int(c);
        }
      ) {
      | Some(c) => c
      | None => status
      };
    {
      message,
      code,
    };
  };

  /** [error_of_http_failure] in the error-chunk shape the SSE stream itself
      would carry, so it can be fed to [StreamAccumulator] and [finalize] will
      surface it as [Model.Error]. */
  let error_chunk_of_http_failure = (~status: int, ~body: string): Json.t => {
    let e = error_of_http_failure(~status, ~body);
    `Assoc([
      (
        "error",
        `Assoc([("message", `String(e.message)), ("code", `Int(e.code))]),
      ),
    ]);
  };

  let handle_chat =
      (~db: string => unit=ignore, response: option(Json.t))
      : option(Model.result) => {
    Option.map(r => r |> Json.to_string |> db, response) |> ignore;
    let* json = response;

    switch (parse_errs(json)) {
    | Some(e) => Some(Model.Error(e))
    | None =>
      let (content_opt, reasoning) =
        switch (Json.dot("choices", json)) {
        | Some(choices) => first_message_content_and_reasoning(choices)
        | None => (None, None)
        };
      let content = Option.value(~default="", content_opt);
      let tool_calls =
        switch (Json.dot("choices", json)) {
        | Some(choices) => parse_tool_calls(choices)
        | None => []
        };
      let usage = {
        let* usage = Json.dot("usage", json);
        of_usage(usage);
      };
      Some(
        Model.Reply({
          content,
          tool_calls,
          usage,
          reasoning,
        }),
      );
    };
  };

  /** Accumulates SSE chunks from a streaming chat response into the same
      [Model.result] the non-streaming [handle_chat] produces. [feed] updates
      internal state per chunk and returns the visible content/reasoning delta
      that landed in this chunk (for UI dispatch); tool-call args, usage, and
      errors are buffered silently and surfaced at [finalize]. */
  module StreamAccumulator = {
    type partial_tool_call = {
      id: ref(option(string)),
      name: ref(option(string)),
      args_buf: Buffer.t,
    };

    type delta = {
      content_delta: string,
      reasoning_delta: string,
    };

    type t = {
      content: Buffer.t,
      reasoning: Buffer.t,
      /* Keyed by the streaming [index] on each tool_call delta. */
      tool_calls: Hashtbl.t(int, partial_tool_call),
      usage: ref(option(Reply.Model.usage)),
      error: ref(option(Model.error)),
    };

    let create = (): t => {
      content: Buffer.create(256),
      reasoning: Buffer.create(256),
      tool_calls: Hashtbl.create(4),
      usage: ref(None),
      error: ref(None),
    };

    let absorb_one_tool_call = (t: t, tc: Json.t): unit => {
      let idx =
        switch (Json.dot("index", tc)) {
        | Some(`Int(n)) => n
        | _ => 0
        };
      let partial =
        switch (Hashtbl.find_opt(t.tool_calls, idx)) {
        | Some(p) => p
        | None =>
          let p = {
            id: ref(None),
            name: ref(None),
            args_buf: Buffer.create(64),
          };
          Hashtbl.replace(t.tool_calls, idx, p);
          p;
        };
      switch (Json.dot("id", tc)) {
      | Some(`String(s)) => partial.id := Some(s)
      | _ => ()
      };
      switch (Json.dot("function", tc)) {
      | None => ()
      | Some(f) =>
        switch (Json.dot("name", f)) {
        | Some(`String(s)) => partial.name := Some(s)
        | _ => ()
        };
        switch (Json.dot("arguments", f)) {
        | Some(`String(s)) => Buffer.add_string(partial.args_buf, s)
        | _ => ()
        };
      };
    };

    let absorb_tool_calls = (t: t, chunk: Json.t): unit =>
      switch (Json.dot("choices", chunk)) {
      | None => ()
      | Some(choices) =>
        switch (Json.list(choices)) {
        | None
        | Some([]) => ()
        | Some([hd, ..._]) =>
          let container =
            switch (Json.dot("delta", hd)) {
            | Some(d) => Some(d)
            | None => Json.dot("message", hd)
            };
          switch (container) {
          | None => ()
          | Some(d) =>
            switch (Json.dot("tool_calls", d)) {
            | None => ()
            | Some(tcs) =>
              switch (Json.list(tcs)) {
              | None => ()
              | Some(tcs_list) =>
                List.iter(tc => absorb_one_tool_call(t, tc), tcs_list)
              }
            }
          };
        }
      };

    /* Read raw content / reasoning from the first choice without the
       back-compat swap that [first_message_content_and_reasoning] applies.
       Streaming must keep the two buffers distinct per chunk; the swap
       (surfacing reasoning as content when content is absent) is applied
       at [finalize] instead. */
    let raw_first_choice_content_reasoning =
        (choices: Json.t): (option(string), option(string)) => {
      let result = {
        let* choices_list = Json.list(choices);
        let* hd = ListUtil.hd_opt(choices_list);
        let* delta =
          switch (Json.dot("message", hd)) {
          | Some(m) => Some(m)
          | None => Json.dot("delta", hd)
          };
        let from_content =
          switch (Json.dot("content", delta)) {
          | None => None
          | Some(c) => message_content_string(c)
          };
        let from_reasoning =
          Option.bind(Json.dot("reasoning", delta), Json.str);
        let from_reasoning_content =
          Option.bind(Json.dot("reasoning_content", delta), Json.str);
        let from_thinking =
          Option.bind(Json.dot("thinking", delta), Json.str);
        let nonempty = (o: option(string)): option(string) =>
          switch (o) {
          | Some(s) when s != "" => Some(s)
          | _ => None
          };
        let reasoning =
          List.find_map(
            nonempty,
            [from_reasoning_content, from_reasoning, from_thinking],
          );
        Some((nonempty(from_content), reasoning));
      };
      switch (result) {
      | None => (None, None)
      | Some(pair) => pair
      };
    };

    let feed = (t: t, chunk: Json.t): delta => {
      switch (parse_errs(chunk)) {
      | Some(e) => t.error := Some(e)
      | None => ()
      };
      switch (Json.dot("usage", chunk)) {
      | Some(u) =>
        switch (of_usage(u)) {
        | Some(parsed) => t.usage := Some(parsed)
        | None => ()
        }
      | None => ()
      };
      let (c_opt, r_opt) =
        switch (Json.dot("choices", chunk)) {
        | Some(choices) => raw_first_choice_content_reasoning(choices)
        | None => (None, None)
        };
      let content_delta = Option.value(~default="", c_opt);
      let reasoning_delta = Option.value(~default="", r_opt);
      if (content_delta != "") {
        Buffer.add_string(t.content, content_delta);
      };
      if (reasoning_delta != "") {
        Buffer.add_string(t.reasoning, reasoning_delta);
      };
      absorb_tool_calls(t, chunk);
      {
        content_delta,
        reasoning_delta,
      };
    };

    let finalize_tool_calls = (t: t): list(Reply.Model.tool_call) => {
      let entries =
        Hashtbl.fold(
          (idx, p, acc) => [(idx, p), ...acc],
          t.tool_calls,
          [],
        );
      let sorted = List.sort(((a, _), (b, _)) => compare(a, b), entries);
      List.filter_map(
        ((_, p: partial_tool_call)) => {
          let* id = p.id^;
          let* name = p.name^;
          let args_str = Buffer.contents(p.args_buf);
          let args = parse_tool_args(`String(args_str));
          Some(
            {
              id,
              name,
              args,
            }: Reply.Model.tool_call,
          );
        },
        sorted,
      );
    };

    let finalize = (t: t): Model.result =>
      switch (t.error^) {
      | Some(e) => Model.Error(e)
      | None =>
        let content_buf = Buffer.contents(t.content);
        let reasoning_buf = Buffer.contents(t.reasoning);
        let tool_calls = finalize_tool_calls(t);
        /* Mirror [handle_chat]'s back-compat: if content is empty but
           reasoning is present and there are no tool calls, surface
           reasoning as content so downstream text-only consumers don't
           see an empty reply. Reasoning stays available separately
           only when content was independently present. */
        let (content, reasoning) =
          if (content_buf == "" && reasoning_buf != "" && tool_calls == []) {
            (reasoning_buf, None);
          } else {
            let reasoning =
              if (reasoning_buf == "") {
                None;
              } else {
                Some(reasoning_buf);
              };
            (content_buf, reasoning);
          };
        Model.Reply({
          content,
          tool_calls,
          usage: t.usage^,
          reasoning,
        });
      };
  };

  let start_streaming_chat =
      (
        ~payload: Payload.Model.t,
        ~key: string,
        ~on_chunk: Json.t => unit,
        ~on_done: unit => unit,
      )
      : API.streaming_handle => {
    let streaming_payload = {
      ...payload,
      stream: true,
    };
    let body = Payload.Utils.json_of_payload(~payload=streaming_payload);
    API.request_streaming(
      ~with_credentials=false,
      ~method=POST,
      ~url="https://openrouter.ai/api/v1/chat/completions",
      ~headers=[
        ("Content-Type", "application/json"),
        ("Accept", "text/event-stream"),
        ("Authorization", "Bearer " ++ key),
      ],
      ~body,
      ~on_chunk,
      /* Route HTTP/network failures through [on_chunk] as a synthetic error
         chunk: the caller's [StreamAccumulator] records it and [finalize]
         (run from [on_done], which fires right after) returns [Model.Error]. */
      ~on_error=
        (~status, ~body as error_body) =>
          on_chunk(error_chunk_of_http_failure(~status, ~body=error_body)),
      ~on_done,
      (),
    );
  };
};

module AvailableLLMs = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type pricing = {
      prompt: string,
      completion: string,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type llm_info = {
      id: string,
      name: string,
      pricing,
      [@yojson.default None]
      context_length: option(int),
      [@yojson.default false] [@sexp.default false]
      supports_reasoning: bool,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = list(llm_info);
  };

  // FP Lab-curated recommendations: (exact OpenRouter id, tagline).
  let recommended_entries: list((string, string)) = [
    ("anthropic/claude-opus-4.6", "Most capable"),
    ("anthropic/claude-sonnet-4.6", "High quality and speedy"),
    ("google/gemini-3-flash-preview", "Best balance of quality and cost"),
    ("xiaomi/mimo-v2-pro", "Highly capable and affordable"),
    ("google/gemma-4-31b-it", "Great cheap model"),
  ];

  let recommended_tagline = (info: Model.llm_info): option(string) =>
    List.assoc_opt(info.id, recommended_entries);

  let is_free = (info: Model.llm_info): bool =>
    StringUtil.match(StringUtil.regexp("free"), info.name);

  module Utils = {
    let get_models = (~key: string, ~handler: option(Json.t) => unit): unit => {
      request(
        ~method=GET,
        ~url="https://openrouter.ai/api/v1/models",
        ~headers=[
          ("Content-Type", "application/json"),
          ("Authorization", "Bearer " ++ key),
        ],
        ~body=`Null,
        handler,
      );
    };

    let has_required_parameters = (params_opt: option(Json.t)): bool => {
      switch (params_opt) {
      | Some(`List(params)) =>
        let params_str =
          List.map(
            param =>
              switch (param) {
              | `String(s) => s
              | _ => ""
              },
            params,
          );
        List.mem("tools", params_str); /*&& List.mem("tool_choice", params_str)*/
      | _ => false
      };
    };

    let parse_available_models_response = (json: Json.t): option(Model.t) =>
      try(
        switch (json) {
        | `Assoc(fields) =>
          switch (List.assoc_opt("data", fields)) {
          | Some(`List(models)) =>
            let parsed_models =
              List.filter_map(
                (model: Json.t) =>
                  switch (model) {
                  | `Assoc(model_fields) =>
                    let id_opt = List.assoc_opt("id", model_fields);
                    let name_opt = List.assoc_opt("name", model_fields);
                    let pricing_opt = List.assoc_opt("pricing", model_fields);
                    let params_opt =
                      List.assoc_opt("supported_parameters", model_fields);
                    let context_length =
                      switch (List.assoc_opt("context_length", model_fields)) {
                      | Some(`Int(n)) => Some(n)
                      | Some(`Float(f)) => Some(int_of_float(f))
                      | _ => None
                      };

                    if (!has_required_parameters(params_opt)) {
                      None;
                    } else {
                      let supports_reasoning =
                        switch (params_opt) {
                        | Some(`List(params)) =>
                          List.exists(
                            fun
                            | `String("reasoning") => true
                            | _ => false,
                            params,
                          )
                        | _ => false
                        };
                      switch (id_opt, name_opt, pricing_opt) {
                      | (
                          Some(`String(id)),
                          Some(`String(name)),
                          Some(`Assoc(pricing_fields)),
                        ) =>
                        let prompt = List.assoc_opt("prompt", pricing_fields);
                        let completion =
                          List.assoc_opt("completion", pricing_fields);
                        switch (prompt, completion) {
                        | (Some(`String(p)), Some(`String(c))) =>
                          Some(
                            {
                              id,
                              name,
                              pricing: {
                                prompt: p,
                                completion: c,
                              },
                              context_length,
                              supports_reasoning,
                            }: Model.llm_info,
                          )
                        | _ => None
                        };
                      | _ => None
                      };
                    };
                  | _ => None
                  },
                models,
              );
            let sorted =
              List.sort(
                (a: Model.llm_info, b: Model.llm_info) =>
                  String.compare(a.name, b.name),
                parsed_models,
              );
            let (free, paid) =
              List.partition(
                (model: Model.llm_info) =>
                  StringUtil.match(StringUtil.regexp("free"), model.name),
                sorted,
              );
            Some(free @ paid);
          | _ => None
          }
        | _ => None
        }
      ) {
      | _ => None
      };
  };
};

/** GET /api/v1/credits — `{ data: { total_credits, total_usage } }`. */
module Credits = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      total_credits: float,
      total_usage: float,
    };
  };

  module Utils = {
    let get_credits = (~key: string, ~handler: option(Json.t) => unit): unit => {
      request(
        ~method=GET,
        ~url="https://openrouter.ai/api/v1/credits",
        ~headers=[
          ("Content-Type", "application/json"),
          ("Authorization", "Bearer " ++ key),
        ],
        ~body=`Null,
        handler,
      );
    };

    let num = (json: Json.t): option(float) =>
      switch (json) {
      | `Int(n) => Some(float_of_int(n))
      | `Float(f) => Some(f)
      | _ => None
      };

    let parse_credits_response = (json: Json.t): option(Model.t) => {
      let* data = Json.dot("data", json);
      let* total_credits = Json.dot("total_credits", data);
      let* total_credits = num(total_credits);
      let* total_usage = Json.dot("total_usage", data);
      let+ total_usage = num(total_usage);
      (
        {
          total_credits,
          total_usage,
        }: Model.t
      );
    };
  };
};

/** GET /api/v1/key — label, limits, and per-period usage for the active key. */
module KeyInfo = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      label: option(string),
      limit: option(float),
      limit_remaining: option(float),
      usage: float,
      usage_daily: option(float),
      usage_weekly: option(float),
      usage_monthly: option(float),
      is_free_tier: bool,
    };
  };

  module Utils = {
    let get_key = (~key: string, ~handler: option(Json.t) => unit): unit => {
      request(
        ~method=GET,
        ~url="https://openrouter.ai/api/v1/key",
        ~headers=[
          ("Content-Type", "application/json"),
          ("Authorization", "Bearer " ++ key),
        ],
        ~body=`Null,
        handler,
      );
    };

    let num_opt = (json: option(Json.t)): option(float) =>
      switch (json) {
      | Some(`Int(n)) => Some(float_of_int(n))
      | Some(`Float(f)) => Some(f)
      | _ => None
      };

    let str_opt = (json: option(Json.t)): option(string) =>
      switch (json) {
      | Some(`String(s)) => Some(s)
      | _ => None
      };

    let bool_or = (json: option(Json.t), default: bool): bool =>
      switch (json) {
      | Some(`Bool(b)) => b
      | _ => default
      };

    let parse_key_response = (json: Json.t): option(Model.t) => {
      let* data = Json.dot("data", json);
      let usage =
        switch (num_opt(Json.dot("usage", data))) {
        | Some(f) => f
        | None => 0.0
        };
      Some(
        {
          label: str_opt(Json.dot("label", data)),
          limit: num_opt(Json.dot("limit", data)),
          limit_remaining: num_opt(Json.dot("limit_remaining", data)),
          usage,
          usage_daily: num_opt(Json.dot("usage_daily", data)),
          usage_weekly: num_opt(Json.dot("usage_weekly", data)),
          usage_monthly: num_opt(Json.dot("usage_monthly", data)),
          is_free_tier: bool_or(Json.dot("is_free_tier", data), false),
        }: Model.t,
      );
    };
  };
};
