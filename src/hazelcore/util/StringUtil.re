let is_empty = String.equal("");

let cat = String.concat("");
let sep = String.concat(" ");

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

let rec escaped_enter = (s: string): string =>
  if (s == "") {
    s;
  } else if (s.[0] == '\n') {
    "\\n" ++ escaped_enter(String.sub(s, 1, String.length(s) - 1));
  } else {
    String.sub(s, 0, 1)
    ++ escaped_enter(String.sub(s, 1, String.length(s) - 1));
  };

let utf8_length = CamomileLibrary.UTF8.length;

let rec find_and_replace =
        (acc: string, s: string, err: string): (string, string) => {
  let len_s = String.length(s) - 2;
  if (len_s <= (-1)) {
    if (len_s == (-1) && s.[0] == '\\') {
      (acc ++ s, err);
    } else {
      (acc ++ s, err);
    };
  } else {
    switch (String.sub(s, 0, 2)) {
    | "\\b" =>
      if (String.length(acc) >= 1) {
        find_and_replace(
          String.sub(acc, 0, String.length(acc) - 1),
          String.sub(s, 2, len_s),
          err,
        );
      } else {
        find_and_replace("\\b", String.sub(s, 2, len_s), err);
      }
    | "\\t" => find_and_replace(acc ++ "\t", String.sub(s, 2, len_s), err)
    | "\\r" =>
      let (result, err) =
        find_and_replace("", String.sub(s, 2, len_s), err);
      let len = String.length(result);
      if (String.length(acc) <= len) {
        (result, err);
      } else {
        (result ++ String.sub(acc, len, String.length(acc) - len), err);
      };
    | "\\n" => find_and_replace(acc ++ "\n", String.sub(s, 2, len_s), err)
    | "\\\\" => find_and_replace(acc ++ "\\", String.sub(s, 2, len_s), err)
    | "\\\"" => find_and_replace(acc ++ "\"", String.sub(s, 2, len_s), err)
    | "\\\'" => find_and_replace(acc ++ "\'", String.sub(s, 2, len_s), err)
    | "\\ " => find_and_replace(acc ++ " ", String.sub(s, 2, len_s), err)
    | "\\o" when len_s >= 3 =>
      let ch1 = s.[2];
      let ch2 = s.[3];
      let ch3 = s.[4];
      if ((ch1 >= '0' && ch1 <= '7')
          && (ch2 >= '0' && ch2 <= '7')
          && ch3 >= '0'
          && ch3 <= '7') {
        if (ch1 <= '3') {
          let str =
            Char.escaped(
              Char.chr(int_of_string("0o" ++ String.sub(s, 2, 3))),
            );
          find_and_replace(acc ++ str, String.sub(s, 5, len_s - 3), err);
        } else {
          find_and_replace(
            acc ++ String.sub(s, 0, 1),
            String.sub(s, 1, len_s + 1),
            "Illegal",
          );
        };
      } else {
        find_and_replace(
          acc ++ String.sub(s, 0, 1),
          String.sub(s, 1, len_s + 1),
          err,
        );
      };
    | "\\x" when len_s >= 2 =>
      let ch1 = Char.lowercase_ascii(s.[2]);
      let ch2 = Char.lowercase_ascii(s.[3]);
      if ((ch1 >= '0' && ch1 <= '9' || ch1 >= 'a' && ch1 <= 'f')
          && (ch2 >= '0' && ch2 <= '9' || ch2 >= 'a' && ch2 <= 'f')) {
        let str =
          Char.escaped(
            Char.chr(int_of_string("0x" ++ String.sub(s, 2, 2))),
          );
        find_and_replace(acc ++ str, String.sub(s, 4, len_s - 2), err);
      } else {
        find_and_replace(
          acc ++ String.sub(s, 0, 1),
          String.sub(s, 1, len_s + 1),
          err,
        );
      };
    | _ =>
      if (String.sub(s, 0, 1) == "\\" && len_s >= 2) {
        let ch1 = s.[1];
        let ch2 = s.[2];
        let ch3 = s.[3];
        if ((ch1 >= '0' && ch1 <= '9')
            && (ch2 >= '0' && ch2 <= '9')
            && ch3 >= '0'
            && ch3 <= '9') {
          if (int_of_string(String.sub(s, 1, 3)) < 256) {
            let str =
              Char.escaped(Char.chr(int_of_string(String.sub(s, 1, 3))));
            find_and_replace(acc ++ str, String.sub(s, 4, len_s - 2), err);
          } else {
            find_and_replace(
              acc ++ String.sub(s, 0, 1),
              String.sub(s, 1, len_s + 1),
              "Illegal",
            );
          };
        } else {
          find_and_replace(
            acc ++ String.sub(s, 0, 1),
            String.sub(s, 1, len_s + 1),
            err,
          );
        };
      } else if (String.sub(s, 0, 1) == "\\") {
        find_and_replace(
          acc ++ String.sub(s, 0, 1),
          String.sub(s, 1, len_s + 1),
          err,
        );
      } else {
        find_and_replace(
          acc ++ String.sub(s, 0, 1),
          String.sub(s, 1, len_s + 1),
          err,
        );
      }
    };
  };
};
