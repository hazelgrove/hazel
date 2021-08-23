let is_empty = String.equal("");

let cat = String.concat("");

let replicat = (n: int, s: string) => cat(ListUtil.replicate(n, s));

/**
 * A string of length n has caret positions 0 through n,
 * where 0 places the caret at the start and n places
 * the caret at the end. Split s at caret_index.
 *
 * TODO rename to split
 */
let split_string = (caret_index: int, s: string): (string, string) => (
  String.sub(s, 0, caret_index),
  String.sub(s, caret_index, String.length(s) - caret_index),
);

let insert = (caret_index: int, insert_s: string, s: string): string => {
  let (l, r) = s |> split_string(caret_index);
  l ++ insert_s ++ r;
};

let backspace = (caret_index: int, s: string): string => {
  let l = String.sub(s, 0, caret_index - 1);
  let r = String.sub(s, caret_index, String.length(s) - caret_index);
  l ++ r;
};

let delete = (caret_index: int, s: string): string => {
  let l = String.sub(s, 0, caret_index);
  let r = String.sub(s, caret_index + 1, String.length(s) - caret_index - 1);
  l ++ r;
};

let utf8_length = CamomileLibrary.UTF8.length;

let match_prefix = (prefix: string, target: string): bool => {
  let re = Str.regexp(prefix);
  Str.string_match(re, target, 0);
};

let match_prefix_subs = (prefix: string, target: string): (bool, string) => {
  let re = Str.regexp(prefix);
  let res = Str.string_match(re, target, 0);
  let subs = Str.matched_string(target);
  (res, subs);
};

let matched_group_opt = (n: int, s: string): option(string) =>
  try(Some(Str.matched_group(n, s))) {
  | _ => None
  };

let group_beginning_opt = (n: int): option(int) =>
  try(Some(Str.group_beginning(n))) {
  | _ => None
  };

let search_forward_opt = (re: Str.regexp, target: string) =>
  try(Some(Str.search_forward(re, target, 0))) {
  | _ => None
  };

let escape_regexp_special_chars = (s: string): string => {
  /* Escape regexp special characters */
  let re = Str.regexp("\\.");
  let replacer = _ => "\\.";
  Str.global_substitute(re, replacer, s);
};
