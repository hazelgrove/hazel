open Sexplib.Std;
[@deriving sexp]
type t = string;

let valid_regex = Re.Str.regexp({|^\.[_a-zA-Z]*[_a-zA-Z0-9']*$|});
let is_valid = s => Re.Str.string_match(valid_regex, s, 0);

let length: t => int = label => String.length(label);

// let sub: (t, int, int) => t = String.sub(label, s, e);

let insert: (int, string, t) => t =
  (caret_index, insert_l, label) => {
    StringUtil.insert(caret_index, insert_l, label);
  };

let delete: (int, t) => t =
  (caret_index, label) => {
    StringUtil.delete(caret_index, label);
  };

let backspace: (int, t) => t =
  (caret_index, label) => {
    StringUtil.backspace(caret_index, label);
  };

let empty: t => bool = label => String.length(label) <= 1;
