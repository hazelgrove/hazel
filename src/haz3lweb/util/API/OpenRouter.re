module Sexp = Sexplib.Sexp;
open API;
open Util.OptUtil.Syntax;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type chat_models =
  | DeepSeek_V3
  | O3_Mini_High
  | Gemini_Experimental_1206
  | Gemini_Flash_Lite;

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
  | DeepSeek_V3 => "deepseek-v3"
  | O3_Mini_High => "o3-mini-high"
  | Gemini_Experimental_1206 => "gemini-experimental-1206"
  | Gemini_Flash_Lite => "google/gemini-2.0-flash-lite-preview-02-05:free";

let string_of_role =
  fun
  | System => "system"
  | User => "user"
  | Assistant => "assistant"
  | Function => "function";

let default_params = {llm: Gemini_Flash_Lite, temperature: 1.0, top_p: 1.0};

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

let lookup_key = (llm: chat_models) =>
  switch (llm) {
  | DeepSeek_V3 => Store.Generic.load("API")
  | O3_Mini_High => Store.Generic.load("API")
  | Gemini_Experimental_1206 => Store.Generic.load("API")
  | Gemini_Flash_Lite => Store.Generic.load("API") // Adjust if using a different key
  };

let chat = (~key, ~body, ~handler): unit =>
  switch (key) {
  | None => print_endline("API: OpenAI KEY NOT FOUND")
  | Some(api_key) =>
    print_endline("API: POSTing OpenRouter request");
    request(
      ~method=POST,
      ~url="https://openrouter.ai/api/v1/chat/completions",
      ~headers=[
        ("Content-Type", "application/json"),
        ("Authorization", "Bearer " ++ api_key),
      ],
      ~body,
      handler,
    );
  };

let start_chat = (~params, ~key, prompt: prompt, handler): unit => {
  let body = body(~params, prompt);
  switch (params.llm) {
  | DeepSeek_V3 => chat(~key, ~body, ~handler)
  | O3_Mini_High => chat(~key, ~body, ~handler)
  | Gemini_Experimental_1206 => chat(~key, ~body, ~handler)
  | Gemini_Flash_Lite => chat(~key, ~body, ~handler) // Add Dolphin handling
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

let add_to_prompt = (prompt, ~assistant, ~user): prompt =>
  prompt
  @ [{role: Assistant, content: assistant}, {role: User, content: user}];
