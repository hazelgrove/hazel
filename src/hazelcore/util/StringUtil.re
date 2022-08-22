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

let contains_substring = (str: string, substr: string): bool => {
  let n = String.length(str);
  let m = String.length(substr);
  m == 0
  || m == n
  && String.equal(str, substr)
  || m < n
  && {
    let c = String.unsafe_get(substr, 0);
    let rec go = i => {
      switch (String.index_from_opt(str, i, c)) {
      | None => false
      | Some(j) =>
        j <= n
        - m
        && String.equal(String.sub(str, j, m), substr)
        || go(j + 1)
      };
    };
    go(0);
  };
};
