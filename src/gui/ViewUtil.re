module Regexp = Js_of_ocaml.Regexp;
open SemanticsCommon;

exception MalformedView(int);

[@deriving sexp]
type delim_path = (Path.steps, delim_index);
[@deriving sexp]
type op_path = (Path.steps, op_index);

let cell_id = "cell";

let node_id = steps =>
  "node__" ++ Sexplib.Sexp.to_string(Path.sexp_of_steps(steps));
let text_id = steps =>
  "text__" ++ Sexplib.Sexp.to_string(Path.sexp_of_steps(steps));
let path_id = path =>
  "path__" ++ Sexplib.Sexp.to_string(Path.sexp_of_t(path));
let delim_id = ((steps, delim_index) as _delim_path) =>
  "delim__"
  ++ Sexplib.Sexp.to_string(sexp_of_delim_path((steps, delim_index)));
let op_id = (steps, op_index) =>
  "op__" ++ Sexplib.Sexp.to_string(sexp_of_op_path((steps, op_index)));

let box_node_indicator_id = "box_node_indicator";
let child_indicator_id = i => "child_indicator__" ++ string_of_int(i);
let empty_hole_conclusion_mask_id = "empty_hole_conclusion_mask";
let box_tm_indicator_id = "box_tm_indicator";
let seq_tm_indicator_id = i => "seq_tm_indicator__" ++ string_of_int(i);
let op_node_indicator_id = "op_node_indicator";

let current_shifting_delim_indicator_id = "current_shifting_delim_indicator";
let current_horizontal_shift_target_id = "current_horizontal_shift_target";
let current_vertical_shift_target_id = "current_vertical_shift_target";
let horizontal_shift_target_in_subject_id = i =>
  "horizontal_shift_target_in_subject__" ++ string_of_int(i);
let horizontal_shift_target_in_frame_id = i =>
  "horizontal_shift_target_in_frame__" ++ string_of_int(i);
let vertical_shift_target_in_subject_id = i =>
  "vertical_shift_target_in_subject__" ++ string_of_int(i);
let vertical_shift_target_in_frame_id = i =>
  "vertical_shift_target_in_frame__" ++ string_of_int(i);
let horizontal_shift_rail_id = "horizontal_shift_rail";
let vertical_shift_rail_id = "vertical_shift_rail";

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

let cls_sline = "sline";
let sline_clss = line_no => [
  cls_sline,
  cls_sline ++ "-" ++ string_of_int(line_no),
];

let line_no_of_sline_cls = cls =>
  switch (Regexp.string_match(Regexp.regexp("^sline-(.*)$"), cls, 0)) {
  | None => None
  | Some(result) =>
    switch (Regexp.matched_group(result, 1)) {
    | None => None
    | Some(s) => Some(int_of_string(s))
    }
  };

let indentation_cls = "indentation";
