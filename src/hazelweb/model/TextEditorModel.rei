type t = {
  active: bool,
  current_text: option(string),
  error: option(string),
};

[@deriving sexp]
type update =
  | OpenEditor(Program.t)
  | CloseEditor
  | SetCurrentText(string)
  | SetError(string)
  | ClearError;

let init: t;

let apply_update: (update, t) => t;

/*
 * Returns if the current text is valid (if there is no error)
 */
let is_valid: t => bool;

/*
 * Returns the count of lines in current_text
 */
let line_count: t => int;

/*
 * Returns error as a string, or empty string if no error
 */
let get_error_string: t => string;

/*
 * Returns the current text
 */
let get_current_text: t => string;
