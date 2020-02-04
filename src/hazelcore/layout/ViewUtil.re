type cls = string;

[@deriving sexp]
type delim_path = (CursorPath.steps, DelimIndex.t);
[@deriving sexp]
type op_path = (CursorPath.steps, OpIndex.t);

let cell_id = "cell";

let text_id = steps =>
  "text__" ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_steps(steps));
let path_id = path =>
  "path__" ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path));

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
