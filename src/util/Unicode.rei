type unsafe_any = Js_of_ocaml.Js.Unsafe.any;
let length: string => int;
let remove_nth: (string, int) => string;
let insert_nth: (string, int, string) => string;
let split_nth: (string, int) => (string, string);
let remove_last: string => string;
let remove_first: string => string;
let append: (string, string) => string;
let to_list: string => list(string);
let of_list: list(string) => string;
module Width: {
  type unsafe_any = Js_of_ocaml.Js.Unsafe.any;
  type t =
    | One
    | Two;
  let columns_of_width: t => int;
  let graphemes: string => list(string);
  let is_emoji_cluster: string => bool;
  let classify_cluster: string => t;
  let columns_of_cluster: string => int;
  let columns_of_string: string => int;
  let bounding_box_for: string => (int, int);
  let columns_through_prefix: (string, int) => int;
  let column_to_grapheme_index: (string, int) => int;
};
