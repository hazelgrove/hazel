open Util;
open Js_of_ocaml;

/* HTTP transport for [Util.API]: XmlHttpRequest in the browser plus a
 * node https variant. The pure JSON helpers stay in Util.API.Json. */

module Json = API.Json;

type request = Js.t(XmlHttpRequest.xmlHttpRequest);

type method =
  | GET
  | POST
  | PUT
  | DELETE;

let string_of_method =
  fun
  | GET => "GET"
  | POST => "POST"
  | PUT => "PUT"
  | DELETE => "DELETE";

let receive = (~debug=true, request: request): option(Json.t) =>
  switch (request##.readyState) {
  | XmlHttpRequest.DONE =>
    debug ? Firebug.console##log(request##.responseText) : ();
    Js.Opt.case(
      request##.responseText,
      () => None,
      x => Some(x |> Js.to_string |> Json.from_string),
    );
  | _ => None
  };

let request =
    (
      ~debug=false,
      ~with_credentials=false,
      ~method: method,
      ~url: string,
      ~headers: list((string, string))=[],
      ~body: Json.t=`Null,
      handler: option(Json.t) => unit,
    )
    : unit => {
  debug ? Yojson.Safe.pp(Format.std_formatter, body) : ();
  let request = XmlHttpRequest.create();
  request##.onreadystatechange :=
    Js.wrap_callback(_ =>
      if (request##.readyState == XmlHttpRequest.DONE) {
        handler(receive(request));
      }
    );
  request##.withCredentials := with_credentials |> Js.bool;
  request##_open(
    method |> string_of_method |> Js.string,
    url |> Js.string,
    true |> Js.bool,
  );
  for (i in 0 to List.length(headers) - 1) {
    let (key, value) = List.nth(headers, i);
    request##setRequestHeader(Js.string(key), Js.string(value));
  };
  request##send(body |> Json.to_string |> Js.string |> Js.Opt.return);
};

/* Parse a single SSE line */
let parse_sse_line = (line: string): option(Json.t) => {
  let trimmed = String.trim(line);

  /* Skip empty lines and comments (lines starting with ':') */
  if (trimmed == "" || trimmed.[0] == ':') {
    None;
  } else if (String.starts_with(~prefix="data: ", trimmed)) {
    let data_start = 6; /* Length of "data: " */
    let data =
      String.sub(trimmed, data_start, String.length(trimmed) - data_start);
    let data = String.trim(data);

    /* Check for stream end */
    if (data == "[DONE]") {
      None;
    } else {
      try(Some(Json.from_string(data))) {
      | _ => None
      };
    };
  } else {
    None;
  };
};

let node_request =
    (
      ~debug=false,
      ~with_credentials=false,
      ~method: method,
      ~hostname: string, /* Do not include 'https://' */
      ~path: string,
      ~headers: list((string, string))=[],
      ~body: Json.t=`Null,
      handler: option(Json.t) => unit,
    )
    : unit => {
  let https = Js.Unsafe.js_expr("require('https')");
  debug ? Yojson.Safe.pp(Format.std_formatter, body) : ();
  let options =
    Printf.sprintf(
      "({hostname: \"%s\", path: \"%s\", method: \"%s\", headers: { %s } })",
      hostname,
      path,
      string_of_method(method),
      headers
      |> List.map(((k, v)) => Printf.sprintf("\"%s\": \"%s\"", k, v))
      |> String.concat(","),
    );
  debug ? Printf.printf("options: %s", options) : ();
  let callback =
    Js.wrap_callback(res => {
      let data = ref("");
      res##on(
        Js.string("data"),
        Js.wrap_callback(chunk =>
          data := data^ ++ Js.to_string(chunk##toString)
        ),
      );
      res##on(
        Js.string("end"),
        Js.wrap_callback(_ =>
          handler(
            try(Some(Json.from_string(data.contents))) {
            | _ => None
            },
          )
        ),
      );
    });
  let req = https##request(Js.Unsafe.js_expr(options), callback);
  if (with_credentials) {
    req##withCredentials := Js._true;
  };
  ignore(
    req##on(
      Js.string("error"),
      Js.wrap_callback(error => {
        Firebug.console##log("Error occurred:");
        Firebug.console##log(error);
      }),
    ),
  );
  ignore(req##write(Js.string(Json.to_string(body))));
  ignore(req##end_());
};
