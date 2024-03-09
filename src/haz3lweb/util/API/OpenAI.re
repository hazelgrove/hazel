open API;
open Util.OptUtil.Syntax;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type chat_models =
  | GPT4
  | GPT3_5Turbo
  | Azure_GPT4
  | Azure_GPT3_5Turbo;

[@deriving (show({with_path: false}), sexp, yojson)]
type role =
  | System
  | User
  | Assistant
  | Function;

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
  | Azure_GPT4 => "azure-gpt-4"
  | Azure_GPT3_5Turbo => "azure-gpt-3.5-turbo";

let string_of_role =
  fun
  | System => "system"
  | User => "user"
  | Assistant => "assistant"
  | Function => "function";

let mk_message = ({role, content}) =>
  `Assoc([
    ("role", `String(string_of_role(role))),
    ("content", `String(content)),
  ]);

let body = (~llm, messages: prompt): Json.t => {
  `Assoc([
    ("model", `String(string_of_chat_model(llm))),
    ("messages", `List(List.map(mk_message, messages))),
  ]);
};

let lookup_key = (llm: chat_models) =>
  switch (llm) {
  | Azure_GPT3_5Turbo => Store.Generic.load("AZURE")
  | Azure_GPT4 => Store.Generic.load("AZURE4")
  | GPT3_5Turbo
  | GPT4 => Store.Generic.load("OpenAI")
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

let start_chat = (~llm, ~key, prompt: prompt, handler): unit => {
  let body = body(~llm, prompt);
  switch (llm) {
  | Azure_GPT3_5Turbo => chat_azure35(~key, ~body, ~handler)
  | Azure_GPT4 => chat_azure4(~key, ~body, ~handler)
  | GPT3_5Turbo
  | GPT4 => chat(~key, ~body, ~handler)
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

let handle_chat = (response: option(Json.t)): option(reply) => {
  let* json = response;
  let* choices = Json.dot("choices", json);
  let* usage = Json.dot("usage", json);
  let* content = first_message_content(choices);
  let+ usage = of_usage(usage);
  {content, usage};
};

let add_to_prompt = (prompt, ~assistant, ~user): prompt =>
  prompt
  @ [{role: Assistant, content: assistant}, {role: User, content: user}];
