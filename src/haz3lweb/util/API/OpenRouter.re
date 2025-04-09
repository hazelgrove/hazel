module Sexp = Sexplib.Sexp;
open API;
open Util.OptUtil.Syntax;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type chat_models =
  | Gemini_Experimental_2_5
  | Gemini_Flash_2_0
  | Deepseek_R1
  | DeepSeek_V3
  | Llama_3_1_Nemo
  | Claude_3_5_Sonnet
  | Claude_3_7_Sonnet;

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
type error = {
  message: string,
  code: int,
};

type result =
  | Reply(reply)
  | Error(error);

[@deriving (show({with_path: false}), sexp, yojson)]
let string_of_chat_model =
  fun
  | Gemini_Experimental_2_5 => "google/gemini-2.5-pro-exp-03-25:free"
  | Gemini_Flash_2_0 => "google/gemini-2.0-flash-001"
  | Deepseek_R1 => "deepseek/deepseek-r1:free"
  | DeepSeek_V3 => "deepseek/deepseek-chat-v3-0324:free"
  | Llama_3_1_Nemo => "nvidia/llama-3.1-nemotron-70b-instruct:free"
  | Claude_3_5_Sonnet => "anthropic/claude-3.5-sonnet"
  | Claude_3_7_Sonnet => "anthropic/claude-3.7-sonnet";

let string_of_role =
  fun
  | System => "system"
  | User => "user"
  | Assistant => "assistant"
  | Function => "function";

let default_params = {
  llm: Gemini_Experimental_2_5,
  temperature: 1.0,
  top_p: 1.0,
};

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
  | Gemini_Experimental_2_5 => Store.Generic.load("API")
  | Gemini_Flash_2_0 => Store.Generic.load("API")
  | Deepseek_R1 => Store.Generic.load("API")
  | DeepSeek_V3 => Store.Generic.load("API")
  | Llama_3_1_Nemo => Store.Generic.load("API")
  | Claude_3_5_Sonnet => Store.Generic.load("API")
  | Claude_3_7_Sonnet => Store.Generic.load("API")
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

let start_chat = (~params, ~key, prompt: prompt, handler): unit => {
  let body = body(~params, prompt);
  switch (params.llm) {
  | Gemini_Experimental_2_5 => chat(~key, ~body, ~handler)
  | Gemini_Flash_2_0 => chat(~key, ~body, ~handler)
  | Deepseek_R1 => chat(~key, ~body, ~handler)
  | DeepSeek_V3 => chat(~key, ~body, ~handler)
  | Llama_3_1_Nemo => chat(~key, ~body, ~handler)
  | Claude_3_5_Sonnet => chat(~key, ~body, ~handler)
  | Claude_3_7_Sonnet => chat(~key, ~body, ~handler)
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

let add_to_prompt = (prompt, ~assistant, ~user): prompt =>
  prompt
  @ [
    {
      role: Assistant,
      content: assistant,
    },
    {
      role: User,
      content: user,
    },
  ];
