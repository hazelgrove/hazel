type t = {
  active: bool,
  current_text: option(string),
  error: option(string),
};

[@deriving sexp]
type update =
  | OpenEditor(Program.t)
  | CloseEditor
  | ClearError
  | SetCurrentText(string)
  | SetError(string);

let init: t;

let apply_update: (update, t) => t;

/*
 * Returns if the current text is valid (if there is no error)
 */
let is_valid: t => bool;

/*
 * Returns the count of newlines in current_text
 */
let line_count: t => int;

/*
 * Returns error as a string, or empty string if no error
 */
let get_error_string: t => string;

let get_current_text: t => string;
