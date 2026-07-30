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

/* Regular expressions via the pure-OCaml `re` library (PCRE syntax,
   compatible with the JS-style patterns used across the codebase). */
type regexp = Re.re;

let regexp: string => regexp = pat => Re.Pcre.re(pat) |> Re.compile;

/* Does the pattern match anywhere in the string? (JS exec semantics) */
let match = (r: regexp, s: string): bool => Re.execp(r, s);

/* Replace ALL matches with the literal replacement string (no $1
   backreference support — no caller uses them) */
let replace = (r: regexp, s: string, by: string): string =>
  Re.replace_string(~all=true, r, ~by, s);

let search = (r: regexp, s: string, idx: int): option(int) =>
  switch (Re.exec_opt(~pos=idx, r, s)) {
  | Some(g) => Some(fst(Re.Group.offset(g, 0)))
  | None => None
  };

let plain_split: (string, string) => list(string) =
  (str, sep) =>
    Re.split_full(Re.compile(Re.str(sep)), str)
    |> List.filter_map(
         fun
         | `Text(t) => Some(t)
         | `Delim(_) => None,
       );

let plain_match: (string, string) => bool = pat => match(regexp(pat));

let plain_replace: (string, string, string) => string =
  pat => replace(regexp(pat));

let plain_search: (string, string, int) => int =
  (pat, str, idx) =>
    switch (search(regexp(pat), str, idx)) {
    | Some(idx) => idx
    | None => (-1)
    };

let to_lines = String.split_on_char('\n');

let line_widths = (s: string): list(int) =>
  s |> to_lines |> List.map(Unicode.length);

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
  |> replace(regexp("\r\n"), _, "\n")  // Normalize Windows line breaks
  |> replace(regexp("\r"), _, "\n")  // Normalize old Mac line breaks
  |> replace(regexp("^[\\t \\r]*"), _, "")  // Leading horizontal WS at start
  |> replace(regexp("\n[\\t \\r]*"), _, "\n"); // After each newline
};

let isEmptyOrWhitespace = str => {
  let trimmed = String.trim(str);
  String.length(trimmed) == 0;
};

/* Pure equivalents of JS encodeURIComponent/decodeURIComponent
   (same unreserved set: A-Za-z0-9 - _ . ! ~ * ' ( ); UTF-8 bytes
   percent-encoded with uppercase hex) */
let compress = (s: string): string => {
  let unreserved = c =>
    switch (c) {
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '0' .. '9'
    | '-'
    | '_'
    | '.'
    | '!'
    | '~'
    | '*'
    | '\''
    | '('
    | ')' => true
    | _ => false
    };
  let buf = Buffer.create(String.length(s));
  String.iter(
    c =>
      unreserved(c)
        ? Buffer.add_char(buf, c)
        : Buffer.add_string(buf, Printf.sprintf("%%%02X", Char.code(c))),
    s,
  );
  Buffer.contents(buf);
};

let decompress = (s: string): string => {
  let len = String.length(s);
  let buf = Buffer.create(len);
  let hex = c =>
    switch (c) {
    | '0' .. '9' => Char.code(c) - Char.code('0')
    | 'a' .. 'f' => Char.code(c) - Char.code('a') + 10
    | 'A' .. 'F' => Char.code(c) - Char.code('A') + 10
    | _ => raise(Invalid_argument("decompress: bad escape"))
    };
  let rec go = i =>
    if (i < len) {
      if (s.[i] == '%' && i + 2 < len) {
        Buffer.add_char(
          buf,
          Char.chr(hex(s.[i + 1]) * 16 + hex(s.[i + 2])),
        );
        go(i + 3);
      } else {
        Buffer.add_char(buf, s.[i]);
        go(i + 1);
      };
    };
  go(0);
  Buffer.contents(buf);
};

let sanitize_filename = (s: string): string => {
  replace(regexp("[^a-zA-Z0-9_-]"), s, "");
};

let trim_trailing_whitespace = (str: string): string => {
  let lines = String.split_on_char('\n', str);
  let is_trailing_ws = (c: char): bool => c == ' ' || c == '\t' || c == '\r';
  let trim_line = (line: string): string => {
    let chars = String.to_seq(line) |> List.of_seq;
    let rec drop_trailing_ws = (chars: list(char)): list(char) =>
      switch (chars) {
      | [] => []
      | [c, ...rest] when is_trailing_ws(c) => drop_trailing_ws(rest)
      | [c, ...rest] => [c, ...rest]
      };
    // Reverse, drop leading WS from reversed = drop trailing WS on line
    let reversed_chars = List.rev(chars);
    let trimmed_reversed = drop_trailing_ws(reversed_chars);
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

let levenshtein_distance = (a: string, b: string): int => {
  let a_len = String.length(a);
  let b_len = String.length(b);
  if (a_len == 0) {
    b_len;
  } else if (b_len == 0) {
    a_len;
  } else {
    let prev = Array.init(b_len + 1, i => i);
    let curr = Array.make(b_len + 1, 0);
    for (i in 1 to a_len) {
      curr[0] = i;
      let ai = a.[i - 1];
      for (j in 1 to b_len) {
        let bj = b.[j - 1];
        let cost =
          if (ai == bj) {
            0;
          } else {
            1;
          };
        let deletion = prev[j] + 1;
        let insertion = curr[j - 1] + 1;
        let substitution = prev[j - 1] + cost;
        let m =
          if (deletion < insertion) {
            deletion;
          } else {
            insertion;
          };
        curr[j] = (
          if (m < substitution) {
            m;
          } else {
            substitution;
          }
        );
      };
      for (k in 0 to b_len) {
        prev[k] = curr[k];
      };
    };
    prev[b_len];
  };
};

/* Compute edit distance between two lists of strings using the Levenshtein algorithm */
let levenshtein_list_distance = (a: list(string), b: list(string)): int => {
  let a_len = List.length(a);
  let b_len = List.length(b);
  /* Fast-paths */
  if (a_len == 0) {
    b_len;
  } else if (b_len == 0) {
    a_len;
  } else {
    let a_arr = Array.of_list(a);
    let b_arr = Array.of_list(b);
    let prev = Array.init(b_len + 1, i => i);
    let curr = Array.make(b_len + 1, 0);

    let min3 = (x, y, z) => {
      let m =
        if (x < y) {
          x;
        } else {
          y;
        };
      if (m < z) {
        m;
      } else {
        z;
      };
    };

    for (i in 1 to a_len) {
      curr[0] = i;
      let ai = a_arr[i - 1];
      for (j in 1 to b_len) {
        let bj = b_arr[j - 1];
        let cost =
          if (ai == bj) {
            0;
          } else {
            1;
          };
        let deletion = prev[j] + 1;
        let insertion = curr[j - 1] + 1;
        let substitution = prev[j - 1] + cost;
        curr[j] = min3(deletion, insertion, substitution);
      };
      /* copy curr into prev for next iteration */
      for (k in 0 to b_len) {
        prev[k] = curr[k];
      };
    };
    prev[b_len];
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

  search(0, 0);
};
