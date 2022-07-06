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

// ripped from ocaml stdlib 4.13 string.ml
let starts_with = (~prefix: string, s: string): bool => {
  let len_s = String.length(s)
  and len_pre = String.length(prefix);
  let rec aux = i =>
    if (i == len_pre) {
      true;
    } else if (String.unsafe_get(s, i) != String.unsafe_get(prefix, i)) {
      false;
    } else {
      aux(i + 1);
    };
  len_s >= len_pre && aux(0);
};
