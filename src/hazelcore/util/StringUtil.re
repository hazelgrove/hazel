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

let levenshtein_dist = (a: string, b: string): (int, string) => {
  let placeholder = ' ';
  let placeholder_str = String.make(1, placeholder);
  let a_max = String.length(a);
  let b_max = String.length(b);
  let dist = Array.make_matrix(a_max + 1, b_max + 1, (0, ""));
  for (a_idx in 0 to a_max) {
    dist[a_idx][0] = (a_idx, "");
  };
  for (j in 0 to b_max) {
    dist[0][j] = (j, String.make(j, placeholder));
  };
  for (b_idx in 1 to b_max) {
    for (a_idx in 1 to a_max) {
      if (a.[a_idx - 1] == b.[b_idx - 1]) {
        let (n, str) = dist[a_idx - 1][b_idx - 1];
        dist[a_idx][b_idx] = (n, str ++ String.make(1, a.[a_idx - 1]));
      } else {
        let (n_del, str_del) = dist[a_idx - 1][b_idx];
        let (n_ins, str_ins) = dist[a_idx][b_idx - 1];
        let (n_rep, str_rep) = dist[a_idx - 1][b_idx - 1];
        if (n_del <= n_ins && n_del <= n_rep) {
          dist[a_idx][b_idx] = (1 + n_del, str_del);
        } else if (n_ins <= n_rep) {
          dist[a_idx][b_idx] = (1 + n_ins, str_ins ++ placeholder_str);
        } else {
          dist[a_idx][b_idx] = (1 + n_rep, str_rep ++ placeholder_str);
        };
      };
    };
  };
  dist[a_max][b_max];
};
