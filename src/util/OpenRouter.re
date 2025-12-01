open API;
open OptUtil.Syntax;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

module Message = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type tool_contents = {
      tool_call_id: string,
      name: string,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type role =
      | System
      | Developer
      | User
      | Assistant
      | Tool(tool_contents);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      role,
      content: string,
    };
  };

  module Utils = {
    let string_of_role =
      fun
      | Model.System => "system"
      | Model.Developer => "developer"
      | Model.User => "user"
      | Model.Assistant => "assistant"
      | Model.Tool(_) => "tool";

    let json_of_message = (message: Model.t): Json.t =>
      switch (message.role) {
      | Tool(tool_contents) =>
        `Assoc([
          ("role", `String(string_of_role(message.role))),
          ("content", `String(message.content)),
          ("tool_call_id", `String(tool_contents.tool_call_id)),
          ("name", `String(tool_contents.name)),
        ])
      | _ =>
        `Assoc([
          ("role", `String(string_of_role(message.role))),
          ("content", `String(message.content)),
        ])
      };

    let mk_assistant_msg = (content: string): Model.t => {
      role: Assistant,
      content,
    };
    let mk_user_msg = (content: string): Model.t => {
      role: User,
      content,
    };
    let mk_developer_msg = (content: string): Model.t => {
      role: Developer,
      content,
    };
    let mk_system_msg = (content: string): Model.t => {
      role: System,
      content,
    };
    let mk_tool_msg =
        (content: string, tool_contents: Model.tool_contents): Model.t => {
      role: Tool(tool_contents),
      content,
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
      reasoning,
      temperature: float,
      top_p: float,
      tools: list(Json.t),
      stream: bool,
      messages: list(Message.Model.t),
      prompt: option(string),
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
          ~prompt: option(string),
          ~tools: list(Json.t),
        )
        : Model.t => {
      model_id,
      reasoning: Effort(Low),
      temperature: 0.9,
      top_p: 1.0,
      tools,
      stream: false,
      messages,
      prompt,
    };

    let mk_reasoning = (reasoning: Model.reasoning): Json.t =>
      switch (reasoning) {
      | Effort(effort) =>
        `Assoc([("effort", `String(string_of_effort_level(effort)))])
      | MaxTokens(max_tokens) => `Assoc([("max_tokens", `Int(max_tokens))])
      | Exclude(exclude) => `Assoc([("exclude", `Bool(exclude))])
      };

    let json_of_payload = (~payload: Model.t): Json.t => {
      `Assoc([
        ("model", `String(payload.model_id)),
        ("reasoning", mk_reasoning(payload.reasoning)),
        ("temperature", `Float(payload.temperature)),
        ("top_p", `Float(payload.top_p)),
        ("tools", `List(payload.tools)),
        ("stream", `Bool(payload.stream)),
        (
          "messages",
          `List(List.map(Message.Utils.json_of_message, payload.messages)),
        ),
        switch (payload.prompt) {
        | Some(prompt) => ("prompt", `String(prompt))
        | None => ("prompt", `Null)
        },
      ]);
    };
  };
};

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
      usage,
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

  let first_message_content = (choices: Json.t): option(string) => {
    let* choices = Json.list(choices);
    let* hd = ListUtil.hd_opt(choices);
    let* message = Json.dot("message", hd);

    let* content = Json.dot("content", message);
    Json.str(content);
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
      let* message = Json.dot("message", hd);

      let* tool_calls = Json.dot("tool_calls", message);
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
    db("OpenAI: Chat response:");
    Option.map(r => r |> Json.to_string |> db, response) |> ignore;
    let* json = response;

    switch (parse_errs(json)) {
    | Some(e) => Some(Model.Error(e))
    | None =>
      let* choices = Json.dot("choices", json);
      let* usage = Json.dot("usage", json);
      let* content = first_message_content(choices);
      let tool_calls = parse_tool_calls(choices);
      let+ usage = of_usage(usage);
      Model.Reply({
        content,
        tool_calls,
        usage,
      });
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
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {data: list(llm_info)};
  };

  module Utils = {
    let get_models = (~key, ~handler): unit => {
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
            Some({data: free @ paid});
          | _ => None
          }
        | _ => None
        }
      ) {
      | _ => None
      };
  };
};
