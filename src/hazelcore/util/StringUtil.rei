let is_empty: String.t => bool;

let cat: list(string) => string;

let replicat: (int, string) => string;

/**
 * A string of length n has caret positions 0 through n,
 * where 0 places the caret at the start and n places
 * the caret at the end. Split s at caret_index.
 *
 * TODO rename to split
 */
let split_string: (int, string) => (string, string);

let insert: (int, string, string) => string;

let backspace: (int, string) => string;

let delete: (int, string) => string;

let utf8_length: CamomileLibrary.UTF8.t => int;

let match_prefix: (string, string) => bool;
let match_prefix_subs: (string, string) => (bool, string);

let matched_group_opt: (int, string) => option(string);
let group_beginning_opt: int => option(int);
let search_forward_opt: (Str.regexp, string) => option(int);
let escape_regexp_special_chars: string => string;
let explode: string => list(char);
let levenshtein_dist:
  (~case_sensitive: bool=?, string, string) => (int, string, string);
