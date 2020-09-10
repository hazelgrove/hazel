open Sexplib.Std;
[@deriving sexp]
type t = string;

// ECD TODO: This regex is not working
let valid_regex = Re.Str.regexp({|^\.[_a-zA-Z]*[_a-zA-Z0-9']*$|});
let is_valid = s => Re.Str.string_match(valid_regex, s, 0);

let length: t => int = label => String.length(label);

let sub: (t, int, int) => t = (label, s, e) => String.sub(label, s, e);

//ECD TODO: add "is valid" function to check if an inserted value is valid char

let insert: (t, string, int) => t =
  (label, s, i) => {
    let _start = String.sub(label, 0, i);
    let _end = String.sub(label, i, length(label));
    String.concat(s, [_start, _end]);
  };

let delete: (t, int) => t =
  (label, i) => {
    let _start = String.sub(label, 0, i - 1);
    let _end = String.sub(label, i + 1, length(label));
    _start ++ _end;
  };
