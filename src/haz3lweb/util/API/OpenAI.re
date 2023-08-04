open API;
open Util.OptUtil.Syntax;

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

let body = (~llm, messages: list((role, string))): Json.t => {
  let mk_msg = ((role, content)) =>
    `Assoc([
      ("role", `String(string_of_role(role))),
      ("content", `String(content)),
    ]);
  `Assoc([
    ("model", `String(string_of_chat_model(llm))),
    ("messages", `List(List.map(mk_msg, messages))),
  ]);
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

let body_simple = (~llm, prompt) => body(~llm, [(User, prompt)]);

let chat = (~body, ~handler): unit =>
  switch (Store.Generic.load("OpenAI")) {
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

module Azure = {
  let chat = (~resource, ~deployment, ~api_version, ~body, ~handler): unit =>
    switch (Store.Generic.load("AZURE")) {
    | None => print_endline("API: AZURE KEY NOT FOUND")
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
        ~headers=[
          ("Content-Type", "application/json"),
          ("api-key", api_key),
        ],
        ~body,
        handler,
      );
    };
};

module AzureGPT3_5 = {
  let chat = (~body, ~handler): unit =>
    Azure.chat(
      ~resource="hazel",
      ~deployment="gpt-3.5-turbo",
      ~api_version="2023-05-15",
      ~body,
      ~handler,
    );
};

module AzureGPT4 = {
  let chat = (~body, ~handler): unit =>
    Azure.chat(
      ~resource="hazel2",
      ~deployment="hazel-gpt-4",
      ~api_version="2023-05-15",
      ~body,
      ~handler,
    );
};

let start_chat = (~llm, prompt, handler): unit => {
  let body = body_simple(~llm, prompt);
  switch (llm) {
  | Azure_GPT3_5Turbo => AzureGPT3_5.chat(~body, ~handler)
  | Azure_GPT4 => AzureGPT4.chat(~body, ~handler)
  | GPT3_5Turbo
  | GPT4 => chat(~body=body_simple(~llm, prompt), ~handler)
  };
};

let reply_chat = (~llm, prompt, response, reply, handler): unit => {
  let body =
    body(~llm, [(User, prompt), (Assistant, response), (User, reply)]);
  switch (llm) {
  | Azure_GPT3_5Turbo => AzureGPT3_5.chat(~body, ~handler)
  | Azure_GPT4 => AzureGPT4.chat(~body, ~handler)
  | GPT3_5Turbo
  | GPT4 => chat(~body, ~handler)
  };
};

let handle_chat = (request: request): option(string) =>
  switch (receive(request)) {
  | Some(json) =>
    let* choices = Json.dot("choices", json);
    let* choices = Json.list(choices);
    let* hd = Util.ListUtil.hd_opt(choices);
    let* message = Json.dot("message", hd);
    let* content = Json.dot("content", message);
    Json.str(content);
  | _ =>
    print_endline("API: handle_chat: no response");
    None;
  };
