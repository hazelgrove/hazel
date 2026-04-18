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

    let json_of_message = (message: Model.t): Json.t =>
      switch (message.role) {
      | Tool(tool_call) =>
        `Assoc([
          ("role", `String(string_of_role(message.role))),
          ("content", `String(message.content)),
          ("tool_call_id", `String(tool_call.id)),
        ])
      | Assistant when message.tool_calls != [] =>
        `Assoc([
          ("role", `String("assistant")),
          ("content", `String(message.content)),
          (
            "tool_calls",
            `List(List.map(json_of_tool_call, message.tool_calls)),
          ),
        ])
      | _ =>
        `Assoc([
          ("role", `String(string_of_role(message.role))),
          ("content", `String(message.content)),
        ])
      };

    let mk_assistant_msg =
        (~tool_calls: list(Reply.Model.tool_call)=[], content: string)
        : Model.t => {
      role: Assistant,
      content,
      tool_calls,
    };
    let mk_user_msg = (content: string): Model.t => {
      role: User,
      content,
      tool_calls: [],
    };
    let mk_developer_msg = (content: string): Model.t => {
      role: Developer,
      content,
      tool_calls: [],
    };
    let mk_system_msg = (content: string): Model.t => {
      role: System,
      content,
      tool_calls: [],
    };
    let mk_tool_msg =
        (content: string, tool_call: Reply.Model.tool_call): Model.t => {
      role: Tool(tool_call),
      content,
      tool_calls: [],
    };
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
        )
        : Model.t => {
      model_id,
      reasoning: None,
      temperature: 1.0,
      top_p: 1.0,
      tools,
      stream: false,
      messages,
    };

    let mk_reasoning = (reasoning: Model.reasoning): Json.t =>
      switch (reasoning) {
      | Effort(effort) =>
        `Assoc([("effort", `String(string_of_effort_level(effort)))])
      | MaxTokens(max_tokens) => `Assoc([("max_tokens", `Int(max_tokens))])
      | Exclude(exclude) => `Assoc([("exclude", `Bool(exclude))])
      };

    let json_of_payload = (~payload: Model.t): Json.t => {
      let base_fields = [
        ("model", `String(payload.model_id)),
        ("temperature", `Float(payload.temperature)),
        ("top_p", `Float(payload.top_p)),
        ("tools", `List(payload.tools)),
        ("stream", `Bool(payload.stream)),
        (
          "messages",
          `List(List.map(Message.Utils.json_of_message, payload.messages)),
        ),
      ];
      let fields =
        switch (payload.reasoning) {
        | Some(reasoning) => [
            ("reasoning", mk_reasoning(reasoning)),
            ...base_fields,
          ]
        | None => base_fields
        };
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
    print_endline("API: POSTing OpenRouter request");
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

  let of_usage = (choices: Json.t): option(Reply.Model.usage) => {
    let* prompt_tokens = API.Json.Parsers.int_field(choices, "prompt_tokens");
    let* completion_tokens =
      API.Json.Parsers.int_field(choices, "completion_tokens");
    let+ total_tokens = API.Json.Parsers.int_field(choices, "total_tokens");
    (
      {
        prompt_tokens,
        completion_tokens,
        total_tokens,
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

  let first_message_content = (choices: Json.t): option(string) => {
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
    let from_reasoning = Option.bind(Json.dot("reasoning", delta), Json.str);
    let from_reasoning_content =
      Option.bind(Json.dot("reasoning_content", delta), Json.str);
    let from_thinking = Option.bind(Json.dot("thinking", delta), Json.str);
    switch (
      List.find_map(
        (o: option(string)) =>
          switch (o) {
          | Some(s) when String.trim(s) != "" => Some(s)
          | _ => None
          },
        [from_content, from_reasoning_content, from_reasoning, from_thinking],
      )
    ) {
    | Some(s) => Some(s)
    | None => from_content
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

  let handle_chat =
      (~db: string => unit=ignore, response: option(Json.t))
      : option(Model.result) => {
    Option.map(r => r |> Json.to_string |> db, response) |> ignore;
    let* json = response;

    switch (parse_errs(json)) {
    | Some(e) => Some(Model.Error(e))
    | None =>
      let content =
        switch (
          {
            let* choices = Json.dot("choices", json);
            first_message_content(choices);
          }
        ) {
        | Some(content) => content
        | None => ""
        };
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
        }),
      );
    };
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

  let is_recommended = (info: Model.llm_info): bool =>
    Option.is_some(recommended_tagline(info));

  let is_free = (info: Model.llm_info): bool =>
    StringUtil.match(StringUtil.regexp("free"), info.name);

  module Utils = {
    let get_models = (~key: string, ~handler: option(Json.t) => unit): unit => {
      print_endline("API: GETting OpenRouter models");
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

    let is_top_model = (name: string): bool => {
      StringUtil.match(StringUtil.regexp("Google"), name)
      || StringUtil.match(StringUtil.regexp("Anthropic"), name)
      || StringUtil.match(StringUtil.regexp("DeepSeek"), name)
      || StringUtil.match(StringUtil.regexp("OpenAI"), name)
      || StringUtil.match(StringUtil.regexp("Meta"), name);
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
      print_endline("API: GETting OpenRouter credits");
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
      print_endline("API: GETting OpenRouter key info");
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
