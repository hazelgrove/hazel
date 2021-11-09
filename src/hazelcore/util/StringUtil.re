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

let explode: string => list(char) =
  s => List.init(String.length(s), String.get(s));

/*
 * Calculate the levenshtein (edit) distance between two strings.
 * Returns a triple, where the first element is the minimum number
 * of deletes, replacements, and insertions required to transform
 * string a into string b. The second element of the pair is a
 * 'mask string', which is basically the string b with certain
 * characters replaced by spaces. The missing characters are those
 * which were removed or replaced from string a. The third element
 * is the same kind of mask except for string a.
 */
let levenshtein_dist =
    (~case_sensitive: bool=true, a: string, b: string): (int, string, string) => {
  //TODO(andrew): clean up. reversing so its greedy and doesn't skip chars
  //eg if looking for "fe" in "fee" it would otherwise indicate "f_e" instead of "fe_"
  let a =
    a
    |> explode
    |> List.rev
    |> (x => String.init(List.length(x), List.nth(x)));
  let b =
    b
    |> explode
    |> List.rev
    |> (x => String.init(List.length(x), List.nth(x)));
  let placeholder_ch = ' ';
  let placeholder_str = String.make(1, placeholder_ch);
  let compare = (c1, c2) =>
    case_sensitive
      ? c1 == c2 : Char.lowercase_ascii(c1) == Char.lowercase_ascii(c2);
  let a_max = String.length(a);
  let b_max = String.length(b);
  let dist = Array.make_matrix(a_max + 1, b_max + 1, (0, "", ""));
  for (a_idx in 0 to a_max) {
    dist[a_idx][0] = (a_idx, "", String.make(a_idx, placeholder_ch));
  };
  for (j in 0 to b_max) {
    dist[0][j] = (j, String.make(j, placeholder_ch), "");
  };
  for (b_idx in 1 to b_max) {
    for (a_idx in 1 to a_max) {
      if (compare(a.[a_idx - 1], b.[b_idx - 1])) {
        let (n, str1, str2) = dist[a_idx - 1][b_idx - 1];
        dist[a_idx][b_idx] = (
          n,
          str1 ++ String.make(1, b.[b_idx - 1]),
          str2 ++ String.make(1, a.[a_idx - 1]),
        );
      } else {
        let (n_del, str_del1, str_del2) = dist[a_idx - 1][b_idx];
        let (n_ins, str_ins1, str_ins2) = dist[a_idx][b_idx - 1];
        let (n_rep, str_rep1, str_rep2) = dist[a_idx - 1][b_idx - 1];
        if (n_del <= n_ins && n_del <= n_rep) {
          dist[a_idx][b_idx] = (
            1 + n_del,
            str_del1,
            str_del2 ++ placeholder_str,
          );
        } else if (n_ins <= n_rep) {
          dist[a_idx][b_idx] = (
            1 + n_ins,
            str_ins1 ++ placeholder_str,
            str_ins2,
          );
        } else {
          dist[a_idx][b_idx] = (
            1 + n_rep,
            str_rep1 ++ placeholder_str,
            str_rep2 ++ placeholder_str,
          );
        };
      };
    };
  };
  let (q, r1, r2) = dist[a_max][b_max];
  (
    q,
    r1
    |> explode
    |> List.rev
    |> (x => String.init(List.length(x), List.nth(x))),
    r2
    |> explode
    |> List.rev
    |> (x => String.init(List.length(x), List.nth(x))),
  );
};
