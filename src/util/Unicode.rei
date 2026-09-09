/* Grapheme-aware string operations, in the two units the editor actually
   works in.

   Everything at the top level counts and indexes in GRAPHEME CLUSTERS --
   what a user calls a character -- never bytes or codepoints, so `length`,
   `remove_nth` and friends stay correct across combining marks, ZWJ emoji
   and Hangul jamo. `Width` covers the other unit, COLUMNS: clusters like
   emoji and CJK ideographs occupy two, and monospace layout needs to know.
   Both live here so caret arithmetic, measurement and rendering cannot
   disagree about where a character is.

   Correctness reference is Intl.Segmenter; the ASCII and standalone-codepoint
   fast paths that avoid it are pinned against it in Test_Unicode. */

type unsafe_any = Js_of_ocaml.Js.Unsafe.any;
let is_simple_ascii: string => bool;
let codepoint_at: (string, int) => int;
let graphemes: string => array(string);
let length: string => int;
let remove_nth: (string, int) => string;
let insert_nth: (string, int, string) => string;
let split_nth: (string, int) => (string, string);
let remove_last: string => string;
let remove_first: string => string;
let append: (string, string) => string;
let to_array: string => array(string);
let to_list: string => list(string);
let of_list: list(string) => string;
let is_invisible_cluster: string => bool;
let normalize_nfc: string => string;
let nfc_outside_strings: string => string;
module Width: {
  type t =
    | One
    | Two;
  let columns_of_width: t => int;
  let graphemes: string => list(string);
  let classify_cluster: string => t;
  let is_wide_cluster: string => bool;
  let columns_of_cluster: string => int;
  let columns_of_string: string => int;
  let max_columns: list(string) => int;
  let bounding_box_for: string => (int, int);
  let columns_through_prefix: (string, int) => int;
  let column_to_grapheme_index: (string, int) => int;
};
