open API;
open Util.OptUtil.Syntax;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type chat_models =
  | GPT4
  | GPT3_5Turbo
  | Azure_GPT4_0613
  | Azure_GPT3_5Turbo
  | Starcoder2_15B
  | Starcoder2_15B_Instruct
  | DeepSeek_Coder_V2
  | DeepSeek_Coder_V2_Lite;

[@deriving (show({with_path: false}), sexp, yojson)]
type role =
  | System
  | User
  | Assistant
  | Function;

[@deriving (show({with_path: false}), sexp, yojson)]
type params = {
  llm: chat_models,
  temperature: float,
  top_p: float,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type message = {
  role,
  content: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type prompt = list(message);

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
let string_of_chat_model =
  fun
  | GPT4 => "gpt-4"
  | GPT3_5Turbo => "gpt-3.5-turbo"
  | Azure_GPT4_0613 => "azure-gpt-4"
  | Azure_GPT3_5Turbo => "azure-gpt-3.5-turbo"
  | Starcoder2_15B => "starcoder2:15b-q5_K_M"
  | Starcoder2_15B_Instruct => "sc2"
  | DeepSeek_Coder_V2 => "deepseek-coder"
  | DeepSeek_Coder_V2_Lite => "deepseek-coder-v2:16b";

let string_of_role =
  fun
  | System => "system"
  | User => "user"
  | Assistant => "assistant"
  | Function => "function";

let default_params = {llm: Azure_GPT4_0613, temperature: 1.0, top_p: 1.0};

let mk_message = ({role, content}) =>
  `Assoc([
    ("role", `String(string_of_role(role))),
    ("content", `String(content)),
  ]);

let body = (~params: params, messages: prompt): Json.t => {
  `Assoc([
    ("model", `String(string_of_chat_model(params.llm))),
    ("temperature", `Float(params.temperature)),
    ("top_p", `Float(params.top_p)),
    ("messages", `List(List.map(mk_message, messages))),
  ]);
};

let completion_body =
    (~params: params, ~prompt: string, ~stop: list(string), ~n_predict: int)
    : Json.t => {
  `Assoc([
    ("model", `String(string_of_chat_model(params.llm))),
    ("temperature", `Float(params.temperature)),
    ("top_p", `Float(params.top_p)),
    ("prompt", `String(prompt)),
    ("stop", `List(List.map(s => `String(s), stop))),
    ("n_predict", `Int(n_predict)),
  ]);
};

let lookup_key = (llm: chat_models) =>
  switch (llm) {
  | Azure_GPT3_5Turbo => Store.Generic.load("AZURE")
  | Azure_GPT4_0613 => Store.Generic.load("AZURE4")
  | GPT3_5Turbo
  | GPT4 => Store.Generic.load("OpenAI")
  | Starcoder2_15B
  | Starcoder2_15B_Instruct => Store.Generic.load("Starcoder2")
  | DeepSeek_Coder_V2
  | DeepSeek_Coder_V2_Lite => Store.Generic.load("DeepSeek")
  };

/* SAMPLE OPENAI CHAT RESPONSE:
    {
      "id":"chatcmpl-6y5167eYM6ovo5yVThXzr5CB8oVIO",
      "object":"chat.completion",
      "created":1679776984,
      "model":"gpt-3.5-turbo-0301",
      "usage":{
         "prompt_tokens":25,
         "completion_tokens":1,
         "total_tokens":26
      },
      "choices":[
         {
            "message":{
               "role":"assistant",
               "content":"576"
            },
            "finish_reason":"stop",
            "index":0
         }
      ]
   }*/

let chat = (~key, ~body, ~handler): unit =>
  switch (key) {
  | None => print_endline("API: OpenAI KEY NOT FOUND")
  | Some(api_key) =>
    print_endline("API: POSTing OpenAI request");
    request(
      ~method=POST,
      ~url="https://api.openai.com/v1/chat/completions",
      ~headers=[
        ("Content-Type", "application/json"),
        ("Authorization", "Bearer " ++ api_key),
      ],
      ~body,
      handler,
    );
  };

let azure_request =
    (~key, ~resource, ~deployment, ~api_version, ~body, ~handler): unit =>
  switch (key) {
  | None => print_endline("API: KEY NOT FOUND")
  | Some(api_key) =>
    print_endline("API: POSTing Azure request");
    request(
      ~method=POST,
      ~url=
        Printf.sprintf(
          "https://%s.openai.azure.com/openai/deployments/%s/chat/completions?api-version=%s",
          resource,
          deployment,
          api_version,
        ),
      ~headers=[("Content-Type", "application/json"), ("api-key", api_key)],
      ~body,
      handler,
    );
  };

let chat_azure35 =
  azure_request(
    ~resource="hazel",
    ~deployment="gpt35turbo",
    ~api_version="2023-05-15",
  );

let chat_azure4 =
  azure_request(
    ~resource="hazel2",
    ~deployment="hazel-gpt-4",
    ~api_version="2023-05-15",
  );

let start_chat = (~params, ~key, prompt: prompt, handler): unit => {
  let body = body(~params, prompt);
  switch (params.llm) {
  | Azure_GPT3_5Turbo => chat_azure35(~key, ~body, ~handler)
  | Azure_GPT4_0613 => chat_azure4(~key, ~body, ~handler)
  | GPT3_5Turbo
  | GPT4 => chat(~key, ~body, ~handler)
  | Starcoder2_15B
  | Starcoder2_15B_Instruct
  | DeepSeek_Coder_V2
  | DeepSeek_Coder_V2_Lite =>
    failwith("LS: start_chat: Unsupported chat model")
  };
};

let int_field = (json: Json.t, field: string) => {
  let* num = Json.dot(field, json);
  Json.int(num);
};

let of_usage = (choices: Json.t): option(usage) => {
  let* prompt_tokens = int_field(choices, "prompt_tokens");
  let* completion_tokens = int_field(choices, "completion_tokens");
  let+ total_tokens = int_field(choices, "total_tokens");
  {prompt_tokens, completion_tokens, total_tokens};
};

let first_message_content = (choices: Json.t): option(string) => {
  let* choices = Json.list(choices);
  let* hd = Util.ListUtil.hd_opt(choices);
  let* message = Json.dot("message", hd);
  let* content = Json.dot("content", message);
  Json.str(content);
};

let handle_chat = (~db=ignore, response: option(Json.t)): option(reply) => {
  db("OpenAI: Chat response:");
  Option.map(r => r |> Json.to_string |> db, response) |> ignore;
  let* json = response;
  let* choices = Json.dot("choices", json);
  let* usage = Json.dot("usage", json);
  let* content = first_message_content(choices);
  let+ usage = of_usage(usage);
  {content, usage};
};

let handle_completion =
    (~db=ignore, response: option(Json.t)): option(reply) => {
  db("Completion response:");
  Option.map(r => r |> Json.to_string |> db, response) |> ignore;
  let* json = response;
  let* content = Json.dot("content", json);
  let* content = Json.str(content);
  let* timings = Json.dot("timings", json);
  let* prompt_n = Json.dot("prompt_n", timings);
  let* completion_n = Json.dot("predicted_n", timings);
  let* prompt_tokens = Json.int(prompt_n);
  let+ completion_tokens = Json.int(completion_n);
  let total_tokens = prompt_tokens + completion_tokens;
  let usage = {prompt_tokens, completion_tokens, total_tokens};
  {content, usage};
};

let add_to_prompt = (prompt, ~assistant, ~user): prompt =>
  prompt
  @ [{role: Assistant, content: assistant}, {role: User, content: user}];
