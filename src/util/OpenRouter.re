open API;
open OptUtil.Syntax;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

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
type params = {
  model_id: string,
  reasoning,
  temperature: float,
  top_p: float,
  tools: list(Json.t),
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
// Tool Calls
type structure_action =
  | UpdatePattern
  | UpdateDefinition
  | UpdateBody
  | UpdateBinding
  | DeleteBinding
  | DeleteBody
  | AddBefore
  | AddAfter
  | InvalidStructureAction;

let string_of_structure_action =
  fun
  | UpdatePattern => "update_pattern"
  | UpdateDefinition => "update_definition"
  | UpdateBody => "update_body"
  | UpdateBinding => "update_binding"
  | DeleteBinding => "delete_binding"
  | DeleteBody => "delete_body"
  | AddBefore => "add_before"
  | AddAfter => "add_after"
  | InvalidStructureAction => "invalid_structure_action";

let structure_action_of_string = (structure_action: string) =>
  switch (structure_action) {
  | "update_pattern" => UpdatePattern
  | "update_definition" => UpdateDefinition
  | "update_body" => UpdateBody
  | "update_binding" => UpdateBinding
  | "delete_binding" => DeleteBinding
  | "delete_body" => DeleteBody
  | "add_before" => AddBefore
  | "add_after" => AddAfter
  | _ => InvalidStructureAction
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type tool_call = {
  id: string,
  name: structure_action,
  args: Json.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type reply = {
  content: string,
  tool_call: option(tool_call),
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
  | Tool(_) => "tool";

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
  switch (role) {
  | Tool(tool_contents) =>
    `Assoc([
      ("role", `String(string_of_role(role))),
      ("content", `String(content)),
      ("tool_call_id", `String(tool_contents.tool_call_id)),
      ("name", `String(tool_contents.name)),
    ])
  | _ =>
    `Assoc([
      ("role", `String(string_of_role(role))),
      ("content", `String(content)),
    ])
  };

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
    ("tools", params.tools == [] ? `Null : `List(params.tools)),
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

let first_message_tool_call = (choices: Json.t): option(tool_call) => {
  let* choices = Json.list(choices);
  let* hd = ListUtil.hd_opt(choices);
  let* message = Json.dot("message", hd);

  let* tool_calls = Json.dot("tool_calls", message);
  let* tool_calls = Json.list(tool_calls);
  let* tool_call = ListUtil.hd_opt(tool_calls);

  let* id = Json.dot("id", tool_call);
  let* id = Json.str(id);

  let* tool_call = Json.dot("function", tool_call);

  let* name = Json.dot("name", tool_call);
  let* name = Json.str(name);
  let* args = Json.dot("arguments", tool_call);

  let parsed_args = parse_tool_args(args);

  let tool_call: tool_call = {
    id,
    name: structure_action_of_string(name),
    args: parsed_args,
  };
  Some(tool_call);
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
    let tool_call = first_message_tool_call(choices);
    let+ usage = of_usage(usage);
    Reply({
      content,
      tool_call,
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

let mk_tool_msg = (content: string, tool_contents: tool_contents): message => {
  role: Tool(tool_contents),
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
                      Some({
                        id,
                        name,
                        pricing: {
                          prompt: p,
                          completion: c,
                        },
                      })
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
            (a, b) => String.compare(a.name, b.name),
            parsed_models,
          );
        let (free, paid) =
          List.partition(
            model => StringUtil.match(StringUtil.regexp("free"), model.name),
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
