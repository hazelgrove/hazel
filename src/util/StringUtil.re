/* NOTE: there are deliberately no byte-indexed nth/split/to_list helpers
   here. Text in the editor is indexed by grapheme cluster (see Unicode);
   a byte-indexed `to_list` in particular is indistinguishable at the call
   site from the correct `Unicode.to_list` and silently splits any
   non-ASCII character into its bytes. */

let repeat = (n, s) => String.concat(~sep="", List.init(n, ~f=_ => s));

/* Truncates on a grapheme boundary: cutting mid-cluster would emit
   invalid UTF-8. */
let abbreviate = (max_len, s) =>
  Unicode.length(s) > max_len
    ? fst(Unicode.split_nth(s, max_len)) ++ "..." : s;

type regexp = Js_of_ocaml.Regexp.regexp;

let regexp: string => regexp = Js_of_ocaml.Regexp.regexp;

let match = (r: regexp, s: string): bool =>
  Js_of_ocaml.Regexp.string_match(r, s, 0) |> Option.is_some;

/* `regexp`/`match` above are BYTE-oriented: Js_of_ocaml.Regexp maps each
   byte of the pattern and subject to one JS character, so a class holding a
   non-ASCII character (or `\s`, which matches U+00A0) constrains individual
   UTF-8 bytes. The pair below compiles with `u` over decoded text instead. */

type unicode_regexp = Js_of_ocaml.Js.t(Js_of_ocaml.Js.regExp);

/* Falls back to no `u` for the patterns it rejects (`\-`, a lone `{`), so
   compiling never turns a working pattern into an exception. */
let unicode_regexp_factory: Js_of_ocaml.Js.Unsafe.any =
  Js_of_ocaml.Js.Unsafe.eval_string(
    "(function (pattern, flags) {\n"
    ++ "  try { return new RegExp(pattern, flags + 'u'); }\n"
    ++ "  catch (e) { return new RegExp(pattern, flags); }\n"
    ++ "})",
  );

/* `~global` is not cosmetic: replace and split need `g`, but a `g`-flagged
   regex is stateful under `test` (lastIndex advances between calls), so
   predicates must compile without it. */
let unicode_regexp = (~global=false, src: string): unicode_regexp =>
  Js_of_ocaml.Js.Unsafe.fun_call(
    unicode_regexp_factory,
    [|
      Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(src)),
      Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(global ? "g" : "")),
    |],
  );

let unicode_match = (r: unicode_regexp, s: string): bool =>
  Js_of_ocaml.Js.to_bool(r##test(Js_of_ocaml.Js.string(s)));

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

let to_lines = s => String.split(s, ~on='\n');

/* Grapheme clusters per line. For LAYOUT widths use
   Unicode.Width.columns_of_string, which counts the two columns a wide
   cluster occupies. */
let line_widths = (s: string): list(int) =>
  s |> to_lines |> List.map(~f=Unicode.length);

let max_line_width = (s: string): int =>
  s |> line_widths |> List.fold_left(~f=max, ~init=0);

let num_linebreaks = (s: string) => {
  String.count(s, ~f=Char.equal('\n'));
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

/* Split at the first occurrence of a character. */
let split_first = (~on: char, s: string): option((string, string)) =>
  String.lsplit2(s, ~on);

/* Strip exactly one final newline: the artifact a writer appends (POSIX
   final newline in files; PersistentZipper.persist). All other edge
   whitespace is content and round-trips. */
let strip_final_newline = (s: string): string => {
  let n = String.length(s);
  if (n >= 2 && Char.equal(s.[n - 2], '\r') && Char.equal(s.[n - 1], '\n')) {
    String.sub(s, ~pos=0, ~len=n - 2);
  } else if (n >= 1 && Char.equal(s.[n - 1], '\n')) {
    String.sub(s, ~pos=0, ~len=n - 1);
  } else {
    s;
  };
};

let isEmptyOrWhitespace = str => {
  let trimmed = String.strip(str);
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

/* Trailing whitespace per line, which String.rstrip alone does not do
   (it would only strip the end of the whole string). */
let trim_trailing_whitespace = (str: string): string => {
  let is_trailing_ws = (c: char): bool =>
    Char.equal(c, ' ') || Char.equal(c, '\t') || Char.equal(c, '\r');
  String.split(str, ~on='\n')
  |> List.map(~f=String.rstrip(~drop=is_trailing_ws))
  |> String.concat(~sep="\n");
};

/* Every non-empty prefix, cut on grapheme boundaries. */
let prefixes = (s: string): list(string) => {
  let len = Unicode.length(s);
  if (len == 0) {
    [""];
  } else {
    List.init(len, ~f=i => fst(Unicode.split_nth(s, i + 1)));
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
    let prev = Array.init(b_len + 1, ~f=i => i);
    let curr = Array.create(~len=b_len + 1, 0);
    for (i in 1 to a_len) {
      curr[0] = i;
      let ai = a.[i - 1];
      for (j in 1 to b_len) {
        let bj = b.[j - 1];
        let cost =
          if (Char.equal(ai, bj)) {
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
    let prev = Array.init(b_len + 1, ~f=i => i);
    let curr = Array.create(~len=b_len + 1, 0);

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
          if (String.equal(ai, bj)) {
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
      (Char.equal(sub.[sub_idx], ' ')) {
      search(s_idx, sub_idx + 1);
    } else if
      // If current characters match (case insensitive), advance both indices
      (Char.equal(Char.lowercase(s.[s_idx]), Char.lowercase(sub.[sub_idx]))) {
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
