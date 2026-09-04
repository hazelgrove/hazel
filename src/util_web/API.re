open Util;
open Js_of_ocaml;
open OptUtil.Syntax;

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

  let t_of_yojson = json => json;
  let yojson_of_t = json => json;

  let t_of_sexp = sexp => {
    switch (sexp) {
    | Sexplib.Sexp.Atom(str) => Yojson.Safe.from_string(str)
    | _ => failwith("Invalid JSON sexp")
    };
  };

  let sexp_of_t = json => {
    Sexplib.Sexp.Atom(Yojson.Safe.to_string(json));
  };

  let pp = (fmt, json) => {
    Format.fprintf(fmt, "%s", Yojson.Safe.to_string(json));
  };

  let to_string = Yojson.Safe.to_string;
  let from_string = Yojson.Safe.from_string;

  let int = (json: t): option(int) =>
    switch (json) {
    | `Int(n) => Some(n)
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

  module Parsers = {
    let int_field = (json: t, field: string): option(int) => {
      let* num = dot(field, json);
      int(num);
    };

    let get_json = (item: t, entity: string) => {
      switch (dot(entity, item)) {
      | Some(value) => value
      | None =>
        raise(
          Failure(
            "The entity " ++ entity ++ " must be provided for this action",
          ),
        )
      };
    };

    let get_string = (item: t, entity: string) => {
      switch (dot(entity, item)) {
      | Some(`String(entity)) => entity
      | _ =>
        raise(
          Failure(
            "A string for " ++ entity ++ " must be provided for this action",
          ),
        )
      };
    };

    let get_json_list = (item: t, entities: string) => {
      switch (dot(entities, item)) {
      | Some(`List(entities_list)) => entities_list
      | _ =>
        raise(
          Failure(
            "A list of " ++ entities ++ " must be provided for the action",
          ),
        )
      };
    };

    let get_string_list = (item: t, entities: string) => {
      let entities_list = get_json_list(item, entities);
      List.map(
        (entity: t) =>
          switch (entity) {
          | `String(entity) => entity
          | _ =>
            raise(
              Failure("Each " ++ entities ++ " in the list must be a string"),
            )
          },
        entities_list,
      );
    };
  };
};

let receive = (~debug=false, request: request): option(Json.t) =>
  switch (request##.readyState) {
  | XmlHttpRequest.DONE =>
    /* SPIKE (wasm-eval-bench): reached through Js.Unsafe because the typed
       binding differs across backends -- js_of_ocaml 5.x has Firebug and no
       Console, 6.x has Console and no Firebug. */
    debug
      ? ignore(
          Js.Unsafe.meth_call(
            Js.Unsafe.global##.console,
            "log",
            [|Js.Unsafe.inject(request##.responseText)|],
          ),
        )
      : ();
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

/** Opaque handle returned by the streaming primitives. Call [abort] to cancel
    the in-flight request. Safe to call after the request has already finished;
    the underlying XHR / Node request ignores a late abort. */
type streaming_handle = {abort: unit => unit};

/* Split a buffer on '\n' into complete lines plus any trailing fragment that
   hasn't been terminated yet. The trailing fragment stays in the caller's
   leftover buffer until more data arrives. */
let split_sse_lines = (buf: string): (list(string), string) => {
  let lines = String.split_on_char('\n', buf);
  switch (List.rev(lines)) {
  | [] => ([], "")
  | [last, ...rev_rest] => (List.rev(rev_rest), last)
  };
};

/** Streaming counterpart to [request]. Fires [on_chunk] for every parsed SSE
    data line, and [on_done] exactly once when the response finishes (either
    naturally or via abort). A network failure or non-2xx HTTP status instead
    fires [on_error] — with the status (0 when no response arrived) and the raw
    response body — followed by [on_done]. Returns a [streaming_handle] whose
    [abort] cancels the XHR. */
let request_streaming =
    (
      ~with_credentials=false,
      ~method: method,
      ~url: string,
      ~headers: list((string, string))=[],
      ~body: Json.t=`Null,
      ~on_chunk: Json.t => unit,
      ~on_error: (~status: int, ~body: string) => unit,
      ~on_done: unit => unit,
      (),
    )
    : streaming_handle => {
  let req = XmlHttpRequest.create();
  let last_offset = ref(0);
  let leftover = ref("");
  let done_called = ref(false);
  let aborted = ref(false);
  let feed_lines = lines =>
    List.iter(
      line =>
        switch (parse_sse_line(line)) {
        | Some(json) => on_chunk(json)
        | None => ()
        },
      lines,
    );
  let drain = (text: string) => {
    let total = String.length(text);
    if (total > last_offset^) {
      let new_text = String.sub(text, last_offset^, total - last_offset^);
      last_offset := total;
      let (complete, trailing) = split_sse_lines(leftover^ ++ new_text);
      leftover := trailing;
      feed_lines(complete);
    };
  };
  let finish = () =>
    if (! done_called^) {
      done_called := true;
      /* Flush any trailing fragment as a final line. */
      if (leftover^ != "") {
        feed_lines([leftover^]);
        leftover := "";
      };
      on_done();
    };
  /* Meaningful only once headers have arrived; before that status is 0. On a
     network error the browser also resets status to 0 even if headers had
     arrived, so a mid-stream connection loss lands on the error path too. */
  let status_ok = () => {
    let status = req##.status;
    status >= 200 && status < 300;
  };
  let fail = () =>
    if (! done_called^ && ! aborted^) {
      done_called := true;
      let body = Js.Opt.case(req##.responseText, () => "", Js.to_string);
      on_error(~status=req##.status, ~body);
      on_done();
    };
  /* Gate on status so a JSON error body is never fed through the SSE parser;
     on failure the whole body goes to [on_error] at DONE instead. */
  let read_and_drain = () =>
    if (status_ok()) {
      Js.Opt.case(
        req##.responseText,
        () => (),
        text => drain(Js.to_string(text)),
      );
    };
  /* [readystatechange] fires only when [readyState] transitions — it won't
     re-fire for each chunk received while state stays at LOADING. [progress]
     fires per chunk and is the reliable hook for incremental delivery. */
  Js.Unsafe.set(
    req,
    Js.string("onprogress"),
    Js.wrap_callback(_ => read_and_drain()),
  );
  Js.Unsafe.set(req, Js.string("onerror"), Js.wrap_callback(_ => fail()));
  req##.onreadystatechange :=
    Js.wrap_callback(_ =>
      if (req##.readyState == XmlHttpRequest.DONE) {
        if (aborted^ || status_ok()) {
          read_and_drain();
          finish();
        } else {
          fail();
        };
      }
    );
  req##.withCredentials := with_credentials |> Js.bool;
  req##_open(
    method |> string_of_method |> Js.string,
    url |> Js.string,
    true |> Js.bool,
  );
  List.iter(
    ((key, value)) =>
      req##setRequestHeader(Js.string(key), Js.string(value)),
    headers,
  );
  req##send(body |> Json.to_string |> Js.string |> Js.Opt.return);
  {
    abort: () => {
      aborted := true;
      req##abort;
      finish();
    },
  };
};
