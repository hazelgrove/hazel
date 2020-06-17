type cls = string;

[@deriving sexp]
type delim_path = (CursorPath_common.steps, DelimIndex.t);

[@deriving sexp]
type op_path = (CursorPath_common.steps, OpIndex.t);

let cell_id: string;

let text_id: CursorPath_common.steps => string;

let path_id: CursorPath_common.t => string;

let multi_line_skel_hole_id:
  (CursorPath_common.steps, (int, int), int) => string;

let single_line_skel_hole_id: (CursorPath_common.steps, (int, int)) => string;

let multi_line_ap_hole_id:
  (CursorPath_common.steps, (int, int), int) => string;

let single_line_ap_hole_id: (CursorPath_common.steps, (int, int)) => string;

let steps_of_text_id: string => option(CursorPath_common.steps);

let path_of_path_id: string => option(CursorPath_common.t);

let delim_path_of_delim_id: string => option(delim_path);
