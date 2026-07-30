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

let receive = (~debug=false, request: request): option(Json.t) =>
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

/* Raw-text GET in the browser. Unlike [request], this returns the response
   body verbatim (no JSON parsing) and surfaces non-2xx / network failures as
   Error, which is what UrlFetch.get needs to fetch arbitrary CSV files. */
let request_text =
    (~url: string, handler: result(string, string) => unit): unit => {
  let request = XmlHttpRequest.create();
  request##.onreadystatechange :=
    Js.wrap_callback(_ =>
      if (request##.readyState == XmlHttpRequest.DONE) {
        let status = request##.status;
        if (status >= 200 && status < 300) {
          let text =
            Js.Opt.case(request##.responseText, () => "", Js.to_string);
          handler(Ok(text));
        } else {
          handler(
            Error(Printf.sprintf("HTTP %d fetching %s", status, url)),
          );
        };
      }
    );
  request##_open(Js.string("GET"), Js.string(url), Js.bool(true));
  request##send(Js.Opt.empty);
};

/* Raw-text GET under node (used by the CLI). Picks http/https by url scheme so
   both `http://localhost:...` dev servers and `https://` work. Mirrors the
   chunk-accumulation pattern of [node_request] but returns the body raw and
   reports non-2xx / network errors as Error. */
let node_request_text =
    (~url: string, handler: result(string, string) => unit): unit => {
  let lib = String.starts_with(~prefix="http://", url) ? "http" : "https";
  let client = Js.Unsafe.js_expr("require('" ++ lib ++ "')");
  let callback =
    Js.wrap_callback(res => {
      let status: int = Js.Unsafe.get(res, "statusCode");
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
          if (status >= 200 && status < 300) {
            handler(Ok(data^));
          } else {
            handler(
              Error(Printf.sprintf("HTTP %d fetching %s", status, url)),
            );
          }
        ),
      );
    });
  let req = client##get(Js.string(url), callback);
  ignore(
    req##on(
      Js.string("error"),
      Js.wrap_callback(error =>
        handler(
          Error(
            "network error fetching "
            ++ url
            ++ ": "
            ++ Js.to_string(Js.Unsafe.coerce(error)##toString),
          ),
        )
      ),
    ),
  );
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
        handler(None);
      }),
    ),
  );
  ignore(req##write(Js.string(Json.to_string(body))));
  ignore(req##end_());
};

/** Streaming counterpart to [node_request]. Mirrors [request_streaming] on the
    Node.js path: per-chunk SSE line parsing, one-shot [on_done]. Aborting
    [req##destroy()]s the in-flight request. */
let node_request_streaming =
    (
      ~debug=false,
      ~with_credentials=false,
      ~method: method,
      ~hostname: string, /* Do not include 'https://' */
      ~path: string,
      ~headers: list((string, string))=[],
      ~body: Json.t=`Null,
      ~on_chunk: Json.t => unit,
      ~on_done: unit => unit,
      (),
    )
    : streaming_handle => {
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
  let leftover = ref("");
  let done_called = ref(false);
  let feed_lines = lines =>
    List.iter(
      line =>
        switch (parse_sse_line(line)) {
        | Some(json) => on_chunk(json)
        | None => ()
        },
      lines,
    );
  let absorb = (chunk_str: string) => {
    let (complete, trailing) = split_sse_lines(leftover^ ++ chunk_str);
    leftover := trailing;
    feed_lines(complete);
  };
  let finish = () =>
    if (! done_called^) {
      done_called := true;
      if (leftover^ != "") {
        feed_lines([leftover^]);
        leftover := "";
      };
      on_done();
    };
  let callback =
    Js.wrap_callback(res => {
      res##on(
        Js.string("data"),
        Js.wrap_callback(chunk => absorb(Js.to_string(chunk##toString))),
      );
      res##on(Js.string("end"), Js.wrap_callback(_ => finish()));
    });
  let req = https##request(Js.Unsafe.js_expr(options), callback);
  if (with_credentials) {
    req##withCredentials := Js._true;
  };
  ignore(
    req##on(
      Js.string("error"),
      Js.wrap_callback(error => {
        Firebug.console##log("Streaming request error:");
        Firebug.console##log(error);
        finish();
      }),
    ),
  );
  ignore(req##write(Js.string(Json.to_string(body))));
  ignore(req##end_());
  {
    abort: () => {
      ignore(req##destroy());
      finish();
    },
  };
};
