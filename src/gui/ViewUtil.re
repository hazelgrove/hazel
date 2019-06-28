module Regexp = Js_of_ocaml.Regexp;

let node_id = steps =>
  "node__" ++ Sexplib.Sexp.to_string(Path.sexp_of_steps(steps));
let text_id = steps =>
  "text__" ++ Sexplib.Sexp.to_string(Path.sexp_of_steps(steps));
let path_id = path =>
  "path__" ++ Sexplib.Sexp.to_string(Path.sexp_of_t(path));

let steps_of_node_id = s =>
  switch (Regexp.string_match(Regexp.regexp("^node__(.*)$"), s, 0)) {
  | None => None
  | Some(result) =>
    switch (Regexp.matched_group(result, 1)) {
    | None => None
    | Some(ssexp) =>
      Some(Path.steps_of_sexp(Sexplib.Sexp.of_string(ssexp)))
    }
  };
let steps_of_text_id = s =>
  switch (Regexp.string_match(Regexp.regexp("^text__(.*)$"), s, 0)) {
  | None => None
  | Some(result) =>
    switch (Regexp.matched_group(result, 1)) {
    | None => None
    | Some(ssexp) =>
      Some(Path.steps_of_sexp(Sexplib.Sexp.of_string(ssexp)))
    }
  };
let path_of_path_id = s =>
  switch (Regexp.string_match(Regexp.regexp("^path__(.*)$"), s, 0)) {
  | None => None
  | Some(result) =>
    switch (Regexp.matched_group(result, 1)) {
    | None => None
    | Some(ssexp) => Some(Path.t_of_sexp(Sexplib.Sexp.of_string(ssexp)))
    }
  };
