open Js_of_ocaml;
//open Virtual_dom.Vdom;
open Util.OptUtil.Syntax;

module Json = {
  type t = Yojson.Safe.t;
  let to_string = Yojson.Safe.to_string;
  let from_string = Yojson.Safe.from_string;
  let get_str = (json: t): option(string) =>
    switch (json) {
    | `String(str) => Some(str)
    | _ => None
    };
  let get_list = (json: t): option(list(t)) =>
    switch (json) {
    | `List(xs) => Some(xs)
    | _ => None
    };
  let get_kvs = (json: t): option(list((string, t))) =>
    switch (json) {
    | `Assoc(pairs) => Some(pairs)
    | _ => None
    };

  let get_v = (key: string, json: t): option(t) => {
    let* pairs = get_kvs(json);
    List.assoc_opt(key, pairs);
  };
};

/*
 const data = null;

 const xhr = new XMLHttpRequest();
 xhr.withCredentials = true;

 xhr.addEventListener("readystatechange", function () {
 	if (this.readyState === this.DONE) {
 		console.log(this.responseText);
 	}
 });

 xhr.open("GET", "https://weatherapi-com.p.rapidapi.com/current.json?q=%3CREQUIRED%3E");
 xhr.setRequestHeader("X-RapidAPI-Key", "SIGN-UP-FOR-KEY");
 xhr.setRequestHeader("X-RapidAPI-Host", "weatherapi-com.p.rapidapi.com");

 xhr.send(data);
 */

let requestWeather = k => {
  let request = Js_of_ocaml.XmlHttpRequest.create();
  request##.withCredentials := Js._true;
  request##.onreadystatechange := Js.wrap_callback(k(request));
  request##_open(
    Js.string("GET"),
    Js.string(
      "https://weatherapi-com.p.rapidapi.com/current.json?q=Ann%20arbor",
    ),
    Js.bool(true),
  );
  request##setRequestHeader(
    Js.string("X-RapidAPI-Key"),
    Js.string("50adacef2cmsh8f5a12552751084p108b3fjsn4fb9ac1dab1d"),
  );
  request##setRequestHeader(
    Js.string("X-RapidAPI-Host"),
    Js.string("weatherapi-com.p.rapidapi.com"),
  );
  request##send(Js.null);
};

let processWeather = (xhr): option(string) =>
  if (xhr##.readyState == XmlHttpRequest.DONE) {
    Firebug.console##log(xhr##.responseText);
    let response_str =
      Js.Opt.case(
        xhr##.responseText,
        () => "NORESPONSE",
        x => Js.to_string(x),
      );
    let json = Json.from_string(response_str);
    let blah_str =
      switch (json) {
      | `Assoc([
          ("location", _),
          (
            "current",
            `Assoc([
              ("last_updated_epoch", _),
              ("last_updated", _),
              ("temp_c", _),
              ("temp_f", _),
              ("is_day", _),
              ("condition", `Assoc([("text", `String(condition)), ..._])),
              ..._,
            ]),
          ),
        ]) => condition
      | _ => "NOPE"
      };
    //print_endline("string to paste:");
    //print_endline(blah_str);
    Some("\"" ++ blah_str ++ "\"");
  } else {
    None;
  };

let requestChatGPT =
    (k: (Js.t(XmlHttpRequest.xmlHttpRequest), unit) => unit): unit =>
  switch (LocalStorage.Generic.load(OpenAI)) {
  | None => print_endline("NO OPENAI API KEY FOUND")
  | Some(api_key) =>
    let request = Js_of_ocaml.XmlHttpRequest.create();
    //request##.withCredentials := Js._true;
    request##.onreadystatechange := Js.wrap_callback(k(request));
    request##_open(
      Js.string("POST"),
      Js.string("https://api.openai.com/v1/chat/completions"),
      Js.bool(true),
    );
    request##setRequestHeader(
      Js.string("Content-Type"),
      Js.string("application/json"),
    );
    request##setRequestHeader(
      Js.string("Authorization"),
      Js.string("Bearer " ++ api_key),
    );
    let body = {|{
    "model": "gpt-3.5-turbo",
    "messages": [{"role": "user", "content": "Hello! Please respond with ONLY a random integer between 0 and 1000."}]
  }|};
    request##send(body |> Js.string |> Js.Opt.return);
  };

/*
     {
   "id": "chatcmpl-123",
   "object": "chat.completion",
   "created": 1677652288,
   "choices": [{
     "index": 0,
     "message": {
       "role": "assistant",
       "content": "\n\nHello there, how may I assist you today?",
     },
     "finish_reason": "stop"
   }],
   "usage": {
     "prompt_tokens": 9,
     "completion_tokens": 12,
     "total_tokens": 21
   }
 }
      */

/*
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
 }
   */

let covertChatGPTOutput = (response: string): option(string) => {
  let json = Json.from_string(response);
  print_endline("got json");
  print_endline(Json.to_string(json));
  let* choices = Json.get_v("choices", json);
  print_endline("got choices");
  print_endline(Json.to_string(choices));
  let* choices = Json.get_list(choices);
  let* hd = Util.ListUtil.hd_opt(choices);
  let* message = Json.get_v("message", hd);
  print_endline("got message");
  print_endline(Json.to_string(message));
  let* content = Json.get_v("content", message);
  print_endline("got content");
  print_endline(Json.to_string(content));
  let+ str = Json.get_str(content);
  print_endline("got string");
  str;
};
/*  switch (Json.from_string(response_str)) {
    | `Assoc([
        ("id", _),
        ("object", _),
        ("created", _),
        ("model", _),
        (
          "usage",
          `Assoc([
            ("prompt_tokens", _),
            ("completion_tokens", _),
            ("total_tokens", _),
          ]),
        ),
        (
          "choices",
          `Assoc([
            (
              "message",
              `Assoc([("role", _), ("content", `String(content)), ..._]),
            ),
            ("finish_reason", _),
            ("index", _),
          ]),
        ),
      ]) =>
      Some(content)
    | _ => None
    };*/

let processChatGPT = (request): option(string) =>
  switch (request##.readyState) {
  | XmlHttpRequest.DONE =>
    Firebug.console##log(request##.responseText);
    let response_str =
      Js.Opt.case(
        request##.responseText,
        () => "NORESPONSE",
        x => Js.to_string(x),
      );
    switch (covertChatGPTOutput(response_str)) {
    | None =>
      print_endline("processChatGPT: response parse failed");
      None;
    | Some(content) => Some("\"" ++ content ++ "\"")
    };
  | _ => None
  };
