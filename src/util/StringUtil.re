let cat = String.concat("");

let remove_nth = (n, t) => {
  assert(n < String.length(t));
  String.sub(t, 0, n) ++ String.sub(t, n + 1, String.length(t) - n - 1);
};

let remove_first = remove_nth(0);
let remove_last = t => remove_nth(String.length(t) - 1, t);

let insert_nth = (n, s, t) => {
  assert(n < String.length(t));
  String.sub(t, 0, n) ++ s ++ String.sub(t, n, String.length(t) - n);
};

let split_nth = (n, t) => {
  assert(n < String.length(t));
  (String.sub(t, 0, n), String.sub(t, n, String.length(t) - n));
};

let to_list = s => List.init(String.length(s), i => String.make(1, s.[i]));

let repeat = (n, s) => String.concat("", List.init(n, _ => s));

let abbreviate = (max_len, s) =>
  String.length(s) > max_len ? String.sub(s, 0, max_len) ++ "..." : s;

type regexp = Js_of_ocaml.Regexp.regexp;

let regexp: string => regexp = Js_of_ocaml.Regexp.regexp;

let match = (r: regexp, s: string): bool =>
  Js_of_ocaml.Regexp.string_match(r, s, 0) |> Option.is_some;

let replace = Js_of_ocaml.Regexp.global_replace;

let search = Js_of_ocaml.Regexp.search;

let plain_split: (string, string) => list(string) =
  (str, sep) =>
    Js_of_ocaml.Regexp.split(Js_of_ocaml.Regexp.regexp_string(sep), str);

let plain_match: (string, string) => bool =
  regexp => match(Js_of_ocaml.Regexp.regexp(regexp));

let plain_replace: (string, string, string) => string =
  regexp => replace(Js_of_ocaml.Regexp.regexp(regexp));

let plain_search: (string, string, int) => int =
  (regexp, str, idx) =>
    switch (search(Js_of_ocaml.Regexp.regexp(regexp), str, idx)) {
    | Some((idx, _)) => idx
    | None => (-1)
    };

let to_lines = String.split_on_char('\n');

let line_widths = (s: string): list(int) =>
  s |> to_lines |> List.map(String.length);

let max_line_width = (s: string): int =>
  s |> line_widths |> List.fold_left(max, 0);

let num_lines = (s: string): int => s |> to_lines |> List.length;

let num_linebreaks = (s: string) => {
  s |> String.to_seq |> Seq.filter((==)('\n')) |> Seq.length;
};

let escape_linebreaks: string => string = replace(regexp("\n"), _, "\\n");

let unescape_linebreaks: string => string =
  replace(regexp("\\\\n"), _, "\n");

let trim_leading = (s: string): string => {
  s
  |> replace(regexp("^[ ]*"), _, "")  // Remove leading spaces at start
  |> replace(regexp("\n[ ]*"), _, "\n"); // Remove leading spaces after newlines
};

let isEmptyOrWhitespace = str => {
  let trimmed = String.trim(str);
  String.length(trimmed) == 0;
};

let compress = (s: string): string => {
  let result =
    Js_of_ocaml.Js.encodeURIComponent(Js_of_ocaml.Js.string(s))
    |> Js_of_ocaml.Js.to_string;
  result;
};

let decompress = (s: string): string => {
  let result =
    Js_of_ocaml.Js.decodeURIComponent(Js_of_ocaml.Js.string(s))
    |> Js_of_ocaml.Js.to_string;
  result;
};

let sanitize_filename = (s: string): string => {
  replace(regexp("[^a-zA-Z0-9_-]"), s, "");
};

let trim_trailing_whitespace = (str: string): string => {
  let lines = String.split_on_char('\n', str);
  let trim_line = (line: string): string => {
    let chars = String.to_seq(line) |> List.of_seq;
    let rec drop_leading_spaces = (chars: list(char)): list(char) =>
      switch (chars) {
      | [] => []
      | [' ', ...rest] => drop_leading_spaces(rest)
      | [c, ...rest] => [c, ...rest]
      };
    // Reverse, drop leading spaces, reverse back = drop trailing spaces
    let reversed_chars = List.rev(chars);
    let trimmed_reversed = drop_leading_spaces(reversed_chars);
    let trimmed_chars = List.rev(trimmed_reversed);
    String.of_seq(List.to_seq(trimmed_chars));
  };
  let trimmed_lines = List.map(trim_line, lines);
  String.concat("\n", trimmed_lines);
};

let prefixes = (s: string): list(string) => {
  let len = String.length(s);
  let rec aux = (i: int) =>
    if (i > len) {
      [];
    } else {
      [String.sub(s, 0, i)] @ aux(i + 1);
    };
  if (len == 0) {
    [""];
  } else {
    aux(1);
  };
};

// Removes double quotes from string and escapes newlines
// Update once https://github.com/hazelgrove/hazel/issues/786 is done
let sanitize_for_string_expression = (s: string): string => {
  s |> replace(regexp("\""), _, "") |> replace(regexp("\n"), _, "\\n");
};

let sanitize_for_label = (s: string): string => {
  s |> replace(regexp("`"), _, "");
};

// AI generated function
// checks if 'sub' is a subsequence of 's'
// (i.e., all characters of 'sub' appear in 's' in the same order, but not necessarily consecutively)
// case insensitive, ignores spaces on sub
let subseq_search = (s: string, sub: string): bool => {
  let s_len = String.length(s);
  let sub_len = String.length(sub);

  let rec search = (s_idx: int, sub_idx: int): bool =>
    // If we've matched all characters in sub, we're done
    if (sub_idx >= sub_len) {
      true;
    } else if
      // If we've exhausted s but still have characters to match in sub
      (s_idx >= s_len) {
      false;
    } else if
      // Skip spaces in sub
      (sub.[sub_idx] == ' ') {
      search(s_idx, sub_idx + 1);
    } else if
      // If current characters match (case insensitive), advance both indices
      (Char.lowercase_ascii(s.[s_idx]) == Char.lowercase_ascii(sub.[sub_idx])) {
      search(s_idx + 1, sub_idx + 1);
    } else {
      // If they don't match, advance only the s index
      search(
        s_idx + 1,
        sub_idx,
      );
    };

  print_endline(
    "Subseq search: "
    ++ sub
    ++ " in "
    ++ s
    ++ "returns"
    ++ string_of_bool(search(0, 0)),
  );

  search(0, 0);
};
