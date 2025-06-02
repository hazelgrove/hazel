module Sexp = Sexplib.Sexp;
open API;
open Util.OptUtil.Syntax;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type role =
  | System
  | Developer
  | User
  | Assistant
  | Tool;

[@deriving (show({with_path: false}), sexp, yojson)]
type message = {
  role,
  content: string,
};

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
type tool = {
  name: string,
  description: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type params = {
  model_id: string,
  reasoning,
  temperature: float,
  top_p: float,
  tools: list(tool),
  stream: bool,
  messages: list(message),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type usage = {
  prompt_tokens: int,
  completion_tokens: int,
  total_tokens: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type reply = {
  content: string,
  usage,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type error = {
  message: string,
  code: int,
};

type result =
  | Reply(reply)
  | Error(error);

let string_of_role =
  fun
  | System => "system"
  | Developer => "developer"
  | User => "user"
  | Assistant => "assistant"
  | Tool => "tool";

let string_of_effort_level =
  fun
  | Low => "low"
  | Medium => "medium"
  | High => "high";

let default_params = {
  model_id: "",
  reasoning: Effort(Low),
  temperature: 0.9,
  top_p: 1.0,
  tools: [],
  stream: false,
  messages: [],
};

let set_to_default_params = (model_id: string): params => {
  {
    ...default_params,
    model_id,
  };
};

let mk_message = ({role, content}) =>
  `Assoc([
    ("role", `String(string_of_role(role))),
    ("content", `String(content)),
  ]);

let mk_reasoning = (reasoning: reasoning) =>
  switch (reasoning) {
  | Effort(effort) =>
    `Assoc([("effort", `String(string_of_effort_level(effort)))])
  | MaxTokens(max_tokens) => `Assoc([("max_tokens", `Int(max_tokens))])
  | Exclude(exclude) => `Assoc([("exclude", `Bool(exclude))])
  };

let body = (~params: params, messages: list(message)): Json.t => {
  `Assoc([
    ("model", `String(params.model_id)),
    ("reasoning", mk_reasoning(params.reasoning)),
    ("temperature", `Float(params.temperature)),
    ("top_p", `Float(params.top_p)),
    ("messages", `List(List.map(mk_message, messages))),
    ("stream", `Bool(params.stream)),
  ]);
};

let chat = (~key, ~body, ~handler): unit => {
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
    (~params, ~key, ~outgoing_messages: list(message), handler): unit => {
  let body = body(~params, outgoing_messages);
  chat(~key, ~body, ~handler);
};

let int_field = (json: Json.t, field: string) => {
  let* num = Json.dot(field, json);
  Json.int(num);
};

let of_usage = (choices: Json.t): option(usage) => {
  let* prompt_tokens = int_field(choices, "prompt_tokens");
  let* completion_tokens = int_field(choices, "completion_tokens");
  let+ total_tokens = int_field(choices, "total_tokens");
  {
    prompt_tokens,
    completion_tokens,
    total_tokens,
  };
};

let first_message_content = (choices: Json.t): option(string) => {
  let* choices = Json.list(choices);
  let* hd = Util.ListUtil.hd_opt(choices);
  let* message = Json.dot("message", hd);
  let* content = Json.dot("content", message);
  Json.str(content);
};

let parse_errs = (json: Json.t): option(error) => {
  let* error = Json.dot("error", json);
  let* message = Json.dot("message", error);
  let* message = Json.str(message);
  let* code = Json.dot("code", error);
  let+ code = Json.int(code);
  {
    message,
    code,
  };
};

let handle_chat = (~db=ignore, response: option(Json.t)): option(result) => {
  db("OpenAI: Chat response:");
  Option.map(r => r |> Json.to_string |> db, response) |> ignore;
  let* json = response;

  switch (parse_errs(json)) {
  | Some(e) => Some(Error(e))
  | None =>
    let* choices = Json.dot("choices", json);
    let* usage = Json.dot("usage", json);
    let* content = first_message_content(choices);
    let+ usage = of_usage(usage);
    Reply({
      content,
      usage,
    });
  };
};

let mk_system_msg = (content: string): message => {
  role: System,
  content,
};

let mk_user_msg = (content: string): message => {
  role: User,
  content,
};

let mk_assistant_msg = (content: string): message => {
  role: Assistant,
  content,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pricing = {
  prompt: string,
  completion: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type model_info = {
  id: string,
  name: string,
  pricing,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type models_response = {data: list(model_info)};

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

let parse_models_response = (json: Json.t): option(models_response) =>
  try(
    switch (json) {
    | `Assoc(fields) =>
      switch (List.assoc_opt("data", fields)) {
      | Some(`List(models)) =>
        let parsed_models =
          List.filter_map(
            model =>
              switch (model) {
              | `Assoc(model_fields) =>
                let id_opt = List.assoc_opt("id", model_fields);
                let name_opt = List.assoc_opt("name", model_fields);
                let pricing_opt = List.assoc_opt("pricing", model_fields);
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
                    Some({
                      id,
                      name,
                      pricing: {
                        prompt: p,
                        completion: c,
                      },
                    })
                  // Uncomment below for recommended models (as of May 2025)
                  /* is_top_model(name)
                     ? Some({
                         id,
                         name,
                         pricing: {
                           prompt: p,
                           completion: c,
                         },
                       })
                     : None */
                  | _ => None
                  };
                | _ => None
                };
              | _ => None
              },
            models,
          );
        Some({
          data:
            List.sort(
              (a, b) => String.compare(a.name, b.name),
              parsed_models,
            ),
        });
      | _ => None
      }
    | _ => None
    }
  ) {
  | _ => None
  };
