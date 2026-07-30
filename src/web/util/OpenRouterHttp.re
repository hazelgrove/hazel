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

/** Streaming POST to chat/completions. Payload construction and the synthetic
    error chunk stay in [Util.OpenRouter]; only the transport lives here. */
let start_streaming_chat =
    (
      ~payload: OpenRouter.Payload.Model.t,
      ~key: string,
      ~on_chunk: Json.t => unit,
      ~on_done: unit => unit,
    )
    : ApiHttp.streaming_handle => {
  let body = OpenRouter.Utils.streaming_chat_body(~payload);
  ApiHttp.request_streaming(
    ~with_credentials=false,
    ~method=POST,
    ~url="https://openrouter.ai/api/v1/chat/completions",
    ~headers=[
      ("Content-Type", "application/json"),
      ("Accept", "text/event-stream"),
      ("Authorization", "Bearer " ++ key),
    ],
    ~body,
    ~on_chunk,
    /* Route HTTP/network failures through [on_chunk] as a synthetic error
       chunk: the caller's [StreamAccumulator] records it and [finalize]
       (run from [on_done], which fires right after) returns [Model.Error]. */
    ~on_error=
      (~status, ~body as error_body) =>
        on_chunk(
          OpenRouter.Utils.error_chunk_of_http_failure(
            ~status,
            ~body=error_body,
          ),
        ),
    ~on_done,
    (),
  );
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

let get_credits = (~key: string, ~handler: option(Json.t) => unit): unit => {
  ApiHttp.request(
    ~method=GET,
    ~url="https://openrouter.ai/api/v1/credits",
    ~headers=[
      ("Content-Type", "application/json"),
      ("Authorization", "Bearer " ++ key),
    ],
    ~body=`Null,
    handler,
  );
};

let get_key = (~key: string, ~handler: option(Json.t) => unit): unit => {
  ApiHttp.request(
    ~method=GET,
    ~url="https://openrouter.ai/api/v1/key",
    ~headers=[
      ("Content-Type", "application/json"),
      ("Authorization", "Bearer " ++ key),
    ],
    ~body=`Null,
    handler,
  );
};
