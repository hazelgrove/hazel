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

let rec find_and_replace = (acc: string, s: string): string => {
  let len_s = String.length(s) - 2;
  if (len_s <= (-1)) {
    acc ++ s;
  } else {
    let substr = String.sub(s, 0, 2);
    let slash_b = "\\" ++ "b";
    let slash_t = "\\" ++ "t";
    let slash_r = "\\" ++ "r";
    if (substr == slash_b) {
      find_and_replace(
        String.sub(acc, 0, String.length(acc) - 1),
        String.sub(s, 2, len_s),
      );
    } else if (substr == slash_t) {
      find_and_replace(acc ++ "\t", String.sub(s, 2, len_s));
    } else if (substr == slash_r) {
      if (String.length(acc) <= len_s) {
        find_and_replace("", String.sub(s, 2, len_s));
      } else {
        String.sub(s, 2, len_s)
        ++ String.sub(acc, len_s, String.length(acc) - len_s);
      };
    } else {
      find_and_replace(
        acc ++ String.sub(s, 0, 1),
        String.sub(s, 1, len_s + 1),
      );
    };
  };
};

let utf8_length = CamomileLibrary.UTF8.length;
