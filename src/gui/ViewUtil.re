module Regexp = Js_of_ocaml.Regexp;
open SemanticsCommon;

exception MalformedView;

[@deriving sexp]
type delim_path = (Path.steps, delim_index);

let node_id = steps =>
  "node__" ++ Sexplib.Sexp.to_string(Path.sexp_of_steps(steps));
let text_id = steps =>
  "text__" ++ Sexplib.Sexp.to_string(Path.sexp_of_steps(steps));
let path_id = path =>
  "path__" ++ Sexplib.Sexp.to_string(Path.sexp_of_t(path));
let delim_id = (steps, delim_index) =>
  "delim__"
  ++ Sexplib.Sexp.to_string(sexp_of_delim_path((steps, delim_index)));

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
let delim_path_of_delim_id = s =>
  switch (Regexp.string_match(Regexp.regexp("^delim__(.*)$"), s, 0)) {
  | None => None
  | Some(result) =>
    switch (Regexp.matched_group(result, 1)) {
    | None => None
    | Some(ssexp) =>
      Some(delim_path_of_sexp(Sexplib.Sexp.of_string(ssexp)))
    }
  };
