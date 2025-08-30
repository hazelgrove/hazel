/* Generic web environment helpers for browser-based components */
open Js_of_ocaml;

/* Get current window origin (scheme + host) */
let window_origin = () =>
  try({
    let loc = Dom_html.window##.location;
    let protocol = Js.to_string(loc##.protocol);
    let hostname = Js.to_string(loc##.hostname);
    let port = Js.to_string(loc##.port);
    if (String.equal(port, "")
        || String.equal(port, "80")
        || String.equal(port, "443")) {
      protocol ++ "//" ++ hostname;
    } else {
      protocol ++ "//" ++ hostname ++ ":" ++ port;
    };
  }) {
  | _ => "http://localhost:8000"
  };

/* Get current pathname */
let pathname = () =>
  try(Js.to_string(Dom_html.window##.location##.pathname)) {
  | _ => "/"
  };

/* Extract /build/<branch> if present; else "" */
let base_path = () => {
  let p = pathname();
  /* Simple string matching for /build/<branch> pattern */
  if (String.length(p) >= 7 && String.equal(String.sub(p, 0, 7), "/build/")) {
    /* Find the next slash or end of string */
    let rec find_end = i =>
      if (i >= String.length(p) || Char.equal(p.[i], '/')) {
        i;
      } else {
        find_end(i + 1);
      };
    let end_pos = find_end(7);
    String.sub(p, 0, end_pos);
  } else {
    "";
  };
};

/* Get base URL (origin + base path) */
let base_url = () => window_origin() ++ base_path();

/* URL parameter lookup */
let url_param = (k: string): option(string) =>
  try({
    let params = Url.Current.arguments;
    List.assoc_opt(k, params);
  }) {
  | _ => None
  };

/* Check if running in development mode */
let is_dev = () =>
  switch (url_param("exo_env")) {
  | Some("dev") => true
  | _ =>
    let hostname =
      try(Js.to_string(Dom_html.window##.location##.hostname)) {
      | _ => "localhost"
      };
    String.equal(hostname, "localhost")
    || String.equal(hostname, "127.0.0.1");
  };

/* Choose origin based on environment with per-name overrides */
let choose_origin = (~name: string, ~dev: string, ~prod: string): string =>
  switch (url_param("exoOrigin_" ++ name)) {
  | Some(override) => override
  | None =>
    if (is_dev()) {
      dev;
    } else {
      prod;
    }
  };
