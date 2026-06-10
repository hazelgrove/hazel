open Util;
open API;

/* HTTP entry points for [Util.OpenRouter]: POSTing chat completions and
 * GETting the model list. Parsing and payload construction stay in
 * Util.OpenRouter; this module owns the network calls (via ApiHttp). */

let chat =
    (~key: string, ~body: Json.t, ~handler: option(Json.t) => unit): unit => {
  print_endline("API: POSTing OpenRouter request");
  ApiHttp.request(
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
      ~payload: OpenRouter.Payload.Model.t,
      ~key: string,
      ~handler: option(Json.t) => unit,
    )
    : unit => {
  let json_of_payload = OpenRouter.Payload.Utils.json_of_payload(~payload);
  chat(~key, ~body=json_of_payload, ~handler);
};

let get_models = (~key: string, ~handler: option(Json.t) => unit): unit => {
  print_endline("API: GETting OpenRouter models");
  ApiHttp.request(
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
