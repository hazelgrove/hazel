open SemanticsCommon;

exception MalformedView(int);

type cls = string;

let font_size = 20.0;
let line_height = 1.5;

let indicator_padding = font_size *. (line_height -. 1.0) /. 2.0 -. 1.5;

let cell_padding = 10.0;
let cell_border = 2.0;
let shift_target_thickness = indicator_padding;

[@deriving sexp]
type delim_path = (CursorPath.steps, delim_index);
[@deriving sexp]
type op_path = (CursorPath.steps, op_index);

let cell_id = "cell";

let node_id = steps =>
  "node__" ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_steps(steps));
let text_id = steps =>
  "text__" ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_steps(steps));
let path_id = path =>
  "path__" ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path));
let delim_id = ((steps, delim_index) as _delim_path) =>
  "delim__"
  ++ Sexplib.Sexp.to_string(sexp_of_delim_path((steps, delim_index)));
let op_id = (steps, op_index) =>
  "op__" ++ Sexplib.Sexp.to_string(sexp_of_op_path((steps, op_index)));

let multi_line_skel_hole_id = (steps, (a, b), i) =>
  "multi_line_skel_hole__"
  ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_steps(steps))
  ++ "__"
  ++ "("
  ++ string_of_int(a)
  ++ " "
  ++ string_of_int(b)
  ++ ")"
  ++ "__"
  ++ string_of_int(i);
let single_line_skel_hole_id = (steps, (a, b)) =>
  "multi_line_skel_hole__"
  ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_steps(steps))
  ++ "__"
  ++ "("
  ++ string_of_int(a)
  ++ " "
  ++ string_of_int(b)
  ++ ")";

let multi_line_ap_hole_id = (steps, (a, b), i) =>
  "multi_line_ap_hole__"
  ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_steps(steps))
  ++ "__"
  ++ "("
  ++ string_of_int(a)
  ++ " "
  ++ string_of_int(b)
  ++ ")"
  ++ "__"
  ++ string_of_int(i);
let single_line_ap_hole_id = (steps, (a, b)) =>
  "multi_line_ap_hole__"
  ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_steps(steps))
  ++ "__"
  ++ "("
  ++ string_of_int(a)
  ++ " "
  ++ string_of_int(b)
  ++ ")";

let box_node_indicator_id = "box_node_indicator";
let child_indicator_id = i => "child_indicator__" ++ string_of_int(i);
let empty_hole_conclusion_mask_id = "empty_hole_conclusion_mask";
let box_tm_indicator_id = "box_tm_indicator";
let single_line_seq_tm_indicator_id = "single_line_seq_tm_indicator";
let multi_line_seq_tm_indicator_id = i =>
  "seq_tm_indicator__" ++ string_of_int(i);
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
  if (!Re.Str.string_match(Re.Str.regexp("^node__\\(.*\\)$"), s, 0)) {
    None;
  } else {
    Some(
      CursorPath.steps_of_sexp(
        Sexplib.Sexp.of_string(Re.Str.matched_group(1, s)),
      ),
    );
  };
let steps_of_text_id = s =>
  if (!Re.Str.string_match(Re.Str.regexp("^text__\\(.*\\)$"), s, 0)) {
    None;
  } else {
    Some(
      CursorPath.steps_of_sexp(
        Sexplib.Sexp.of_string(Re.Str.matched_group(1, s)),
      ),
    );
  };
let path_of_path_id = s =>
  if (!Re.Str.string_match(Re.Str.regexp("^path__\\(.*\\)$"), s, 0)) {
    None;
  } else {
    Some(
      CursorPath.t_of_sexp(
        Sexplib.Sexp.of_string(Re.Str.matched_group(1, s)),
      ),
    );
  };
let delim_path_of_delim_id = s =>
  if (!Re.Str.string_match(Re.Str.regexp("^delim__\\(.*\\)$"), s, 0)) {
    None;
  } else {
    Some(
      delim_path_of_sexp(
        Sexplib.Sexp.of_string(Re.Str.matched_group(1, s)),
      ),
    );
  };

let cls_sline = "sline";
let sline_clss = line_no => [
  cls_sline,
  cls_sline ++ "-" ++ string_of_int(line_no),
];

let line_no_of_sline_cls = cls =>
  if (!Re.Str.string_match(Re.Str.regexp("^sline-\\(.*\\)$"), cls, 0)) {
    None;
  } else {
    Some(int_of_string(Re.Str.matched_group(1, cls)));
  };

let indentation_cls = "indentation";
