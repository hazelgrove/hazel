/* Embed mode's messages, between the frame (EmbedMain) and the page around
   it, as the JS values postMessage carries. Protocol 1; the whole of it is
   in docs/hazel-as-a-js-library.md.

   Page to frame:
     { type: "set", text }                        one program, as text
     { type: "set", preamble, modules, tail }     a program of named modules,
                                                  the spec analyzeProgram takes
                                                  (`result` is taken for `tail`)
     { type: "ping" }                             answered with ready

   Frame to page:
     { type: "ready", protocol, build }
     { type: "size", height }                     when it changes
     { type: "status", ok, errors, holes, programHoles }   when it changes
     { type: "crash", message } */
open Js_of_ocaml;

let protocol = 1;

/* The pages the frame takes messages from: Messed Up Dress Up's site,
   and its dev servers on this machine (localhost and 127.0.0.1, any port,
   http or https). Also the frame's own origin, so the frame opened on its
   own takes a program posted from its console. Another page's messages
   are ignored; to embed Hazel elsewhere, add its origin here. */
let site_origins = [
  "https://matthewhammer.org",
  "https://www.matthewhammer.org",
];

let dev_hosts = ["localhost", "127.0.0.1"];

let without_prefix = (prefix: string, s: string): option(string) => {
  let n = String.length(prefix);
  String.length(s) >= n && String.sub(s, 0, n) == prefix
    ? Some(String.sub(s, n, String.length(s) - n)) : None;
};

let is_dev_origin = (origin: string): bool =>
  switch (
    switch (without_prefix("http://", origin)) {
    | Some(rest) => Some(rest)
    | None => without_prefix("https://", origin)
    }
  ) {
  | None => false
  | Some(rest) =>
    let (host, port) =
      switch (String.index_opt(rest, ':')) {
      | Some(i) => (
          String.sub(rest, 0, i),
          Some(String.sub(rest, i + 1, String.length(rest) - i - 1)),
        )
      | None => (rest, None)
      };
    List.mem(host, dev_hosts)
    && (
      switch (port) {
      | None => true
      | Some(port) =>
        port != ""
        && String.for_all(
             fun
             | '0' .. '9' => true
             | _ => false,
             port,
           )
      }
    );
  };

/* own: the frame's own origin. An opaque origin ("null") is no page's. */
let origin_allowed = (~own: string, origin: string): bool =>
  origin != "null"
  && (
    origin == own || List.mem(origin, site_origins) || is_dev_origin(origin)
  );

type incoming =
  | Set(EmbedModel.Program.t)
  | Ping;

/* j[key] when it is a string */
let string_field = (j: Js.Unsafe.any, key: string): option(string) =>
  HazelLibCore.js_is_object(j)
  && Js.to_string(Js.typeof(Js.Unsafe.get(j, key))) == "string"
    ? Some(Js.to_string(Js.Unsafe.get(j, key))) : None;

/* A message the frame acts on, or None for anything else (another
   protocol's, or a later version's) */
let decode = (data: Js.Unsafe.any): option(incoming) =>
  switch (string_field(data, "type")) {
  | Some("set") =>
    switch (string_field(data, "text")) {
    | Some(text) => Some(Set(Text(text)))
    | None =>
      let spec = HazelLibCore.program_spec_of_js(data);
      Some(
        Set(
          Parts({
            preamble: spec.preamble,
            modules: spec.modules,
            tail:
              switch (
                string_field(data, "tail"),
                string_field(data, "result"),
              ) {
              | (None, Some(result)) => result
              | _ => spec.tail
              },
          }),
        ),
      );
    }
  | Some("ping") => Some(Ping)
  | _ => None
  };

let encode = (message: EmbedModel.Outbox.message): Js.Unsafe.any => {
  let number = (n: int) =>
    Js.Unsafe.inject(Js.number_of_float(float_of_int(n)));
  let string = HazelLibCore.js_string;
  HazelLibCore.js_object(
    switch (message) {
    | Ready(build) => [
        ("type", string("ready")),
        ("protocol", number(protocol)),
        ("build", string(build)),
      ]
    | Size(height) => [
        ("type", string("size")),
        ("height", number(height)),
      ]
    | Status({errors, holes, program_holes}) => [
        ("type", string("status")),
        ...HazelLibCore.analysis_fields(~errors, ~holes, ~program_holes),
      ]
    | Crash(message) => [
        ("type", string("crash")),
        ("message", string(message)),
      ]
    },
  );
};
