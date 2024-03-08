open Js_of_ocaml;
open Util.OptUtil.Syntax;

let opt = Util.OptUtil.and_then;

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

module Json = {
  type t = Yojson.Safe.t;
  let to_string = Yojson.Safe.to_string;
  let from_string = Yojson.Safe.from_string;
  let bool = (json: t): option(bool) =>
    switch (json) {
    | `Bool(b) => Some(b)
    | _ => None
    };
  let int = (json: t): option(int) =>
    switch (json) {
    | `Int(n) => Some(n)
    | _ => None
    };
  let float = (json: t): option(float) =>
    switch (json) {
    | `Float(f) => Some(f)
    | _ => None
    };
  let str = (json: t): option(string) =>
    switch (json) {
    | `String(str) => Some(str)
    | _ => None
    };
  let list = (json: t): option(list(t)) =>
    switch (json) {
    | `List(xs) => Some(xs)
    | _ => None
    };
  let get_kvs = (json: t): option(list((string, t))) =>
    switch (json) {
    | `Assoc(pairs) => Some(pairs)
    | _ => None
    };
  let dot = (key: string, json: t): option(t) => {
    let* pairs = get_kvs(json);
    List.assoc_opt(key, pairs);
  };
};

let request =
    (
      ~debug=false,
      ~with_credentials=false,
      ~method: method,
      ~url: string,
      ~headers: list((string, string))=[],
      ~body: Json.t=`Null,
      handler: request => unit,
    )
    : unit => {
  debug ? Yojson.Safe.pp(Format.std_formatter, body) : ();
  let request = XmlHttpRequest.create();
  request##.onreadystatechange := Js.wrap_callback(_ => handler(request));
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

let node_request =
    (
      ~debug=false,
      ~with_credentials=false,
      ~method: method,
      ~hostname: string, /* Do not include 'https://' */
      ~path: string,
      ~headers: list((string, string))=[],
      ~body: Json.t=`Null,
      handler: string => unit,
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
        Js.wrap_callback(_ => {handler(data.contents)}),
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
