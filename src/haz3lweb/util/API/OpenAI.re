open API;
open Util.OptUtil.Syntax;

type chat_models =
  | GPT4
  | GPT3_5Turbo;

type role =
  | User
  | Assistant;

let string_of_chat_model =
  fun
  | GPT4 => "gpt-4"
  | GPT3_5Turbo => "gpt-3.5-turbo";

let string_of_role =
  fun
  | User => "user"
  | Assistant => "assistant";

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

let body = (~model=GPT3_5Turbo, messages: list((role, string))): Json.t => {
  let mk_msg = ((role, content)) =>
    `Assoc([
      ("role", `String(string_of_role(role))),
      ("content", `String(content)),
    ]);
  `Assoc([
    ("model", `String(string_of_chat_model(model))),
    ("messages", `List(List.map(mk_msg, messages))),
  ]);
};

let body_simple = (~model=GPT3_5Turbo, prompt) =>
  body(~model, [(User, prompt)]);

let request_chatGPT = (handler): unit =>
  switch (LocalStorage.Generic.load(OpenAI)) {
  | None => print_endline("NO OPENAI API KEY FOUND")
  | Some(api_key) =>
    request(
      ~method=POST,
      ~url="https://api.openai.com/v1/chat/completions",
      ~headers=[
        ("Content-Type", "application/json"),
        ("Authorization", "Bearer " ++ api_key),
      ],
      ~body=
        body_simple(
          "Hello! Please respond with ONLY a random integer between 8000 and 9001.",
        ),
      handler,
    )
  };

let handle_chatGPT = (request: request): option(string) =>
  switch (receive(request)) {
  | Some(json) =>
    let* choices = Json.dot("choices", json);
    let* choices = Json.list(choices);
    let* hd = Util.ListUtil.hd_opt(choices);
    let* message = Json.dot("message", hd);
    let* content = Json.dot("content", message);
    Json.str(content);
  | _ =>
    print_endline("handleChatGPT: no response");
    None;
  };
