[@coverage exclude_file];
open Js_of_ocaml;

/* Grapheme-indexed string operations, backing Hazel's string builtins.

   Hazel indexes strings by grapheme cluster: that is the unit the editor
   caret moves in (`Inner(n)`) and the unit a user counts when looking at the
   text. The builtins therefore all agree on it, so that the natural idiom

     string_sub(s, string_search(re, s, 0), n)

   composes. The helpers below translate between graphemes and the two units
   the platform actually offers: UTF-16 code units (what JS strings and
   RegExp report) and UTF-8 bytes (what OCaml's String uses).

   Regex support here is code-point-correct, not grapheme-correct. Patterns
   compile with the `u` flag so `.`, character classes and `\p{...}` see whole
   code points instead of bytes, but `.` still matches one code point of a
   multi-code-point cluster (e.g. one member of a ZWJ emoji sequence). Match
   offsets are converted back to graphemes at the boundary. */

let graphemes = Unicode.to_array;

let concat_array = (a: array(string)): string =>
  a |> Array.to_list |> String.concat("");

let length = Unicode.length;

/* UTF-16 code units, as JS counts them. */
let utf16_length = (s: string): int => Js.string(s)##.length;

/* Index of the grapheme containing UTF-16 offset [u]; the grapheme count if
   [u] is at or past the end. Offsets that land inside a cluster report the
   cluster they fall in rather than the next one. */
let grapheme_of_utf16 = (s: string, u: int): int => {
  let clusters = graphemes(s);
  let n = Array.length(clusters);
  let rec go = (i: int, acc: int): int =>
    if (i >= n) {
      n;
    } else {
      let next = acc + utf16_length(clusters[i]);
      next > u ? i : go(i + 1, next);
    };
  u <= 0 ? 0 : go(0, 0);
};

/* UTF-16 offset where grapheme [g] starts; clamped into range. */
let utf16_of_grapheme = (s: string, g: int): int => {
  let clusters = graphemes(s);
  let n = Array.length(clusters);
  let g = g < 0 ? 0 : g > n ? n : g;
  let acc = ref(0);
  for (i in 0 to g - 1) {
    acc := acc^ + utf16_length(clusters[i]);
  };
  acc^;
};

/* [len] graphemes starting at grapheme [idx]. None when the range falls
   outside the string, mirroring String.sub's Invalid_argument. Never splits a
   cluster, so the result is always well-formed. */
let sub = (s: string, idx: int, len: int): option(string) => {
  let clusters = graphemes(s);
  let n = Array.length(clusters);
  if (idx < 0 || len < 0 || idx > n || len > n - idx) {
    None;
  } else {
    Some(concat_array(Array.sub(clusters, idx, len)));
  };
};

/* Full Unicode case mapping (so `é`/`Σ`/`и` are covered, and `ß` expands to
   `SS`), not OCaml's ASCII-only mapping. */
let uppercase = (s: string): string =>
  Js.to_string(Js.string(s)##toUpperCase);

let lowercase = (s: string): string =>
  Js.to_string(Js.string(s)##toLowerCase);

let map_first_grapheme = (f: string => string, s: string): string => {
  let clusters = graphemes(s);
  let n = Array.length(clusters);
  n == 0
    ? s : f(clusters[0]) ++ concat_array(Array.sub(clusters, 1, n - 1));
};

let capitalize = map_first_grapheme(uppercase);
let uncapitalize = map_first_grapheme(lowercase);

/* JS trim: ASCII whitespace plus Unicode Zs, the line/paragraph separators
   and the BOM. A superset of OCaml's String.trim. */
let trim = (s: string): string => Js.to_string(Js.string(s)##trim);

/* Like String.escaped, except bytes >= 0x80 pass through untouched instead of
   becoming `\240\159\152\128`. Scanf.unescaped still inverts this exactly:
   it copies non-backslash bytes through unchanged. */
let escaped = (s: string): string => {
  let buf = Buffer.create(String.length(s));
  String.iter(
    c =>
      switch (c) {
      | '\\' => Buffer.add_string(buf, "\\\\")
      | '"' => Buffer.add_string(buf, "\\\"")
      | '\n' => Buffer.add_string(buf, "\\n")
      | '\t' => Buffer.add_string(buf, "\\t")
      | '\r' => Buffer.add_string(buf, "\\r")
      | '\b' => Buffer.add_string(buf, "\\b")
      | c when Char.code(c) < 0x20 || Char.code(c) == 0x7F =>
        Buffer.add_string(buf, Printf.sprintf("\\%03d", Char.code(c)))
      | c => Buffer.add_char(buf, c)
      },
    s,
  );
  Buffer.contents(buf);
};

let compile = StringUtil.unicode_regexp(~global=true);

/* Escape the regex metacharacters so a literal string matches itself. Every
   character escaped here is a legal identity escape under the `u` flag, and
   none of them can occur as a UTF-8 continuation byte. */
let quote = (s: string): string => {
  let buf = Buffer.create(String.length(s));
  String.iter(
    c => {
      switch (c) {
      | '^'
      | '$'
      | '\\'
      | '.'
      | '*'
      | '+'
      | '?'
      | '('
      | ')'
      | '['
      | ']'
      | '{'
      | '}'
      | '|'
      | '/' => Buffer.add_char(buf, '\\')
      | _ => ()
      };
      Buffer.add_char(buf, c);
    },
    s,
  );
  Buffer.contents(buf);
};

/* Does [pattern] match anywhere in [s]? */
let matches = (pattern: string, s: string): bool => {
  let re = compile(pattern);
  re##.lastIndex := 0;
  Js.to_bool(re##test(Js.string(s)));
};

/* Replace every match of [pattern]. [by] is literal: `$` in it is escaped so
   JS's `$1`/`$&` substitutions don't leak into Hazel. */
let replace = (pattern: string, s: string, by: string): string => {
  let re = compile(pattern);
  let by = String.concat("$$", String.split_on_char('$', by));
  Js.to_string(Js.string(s)##replace(re, Js.string(by)));
};

/* Split on a literal separator. An empty separator splits into graphemes,
   which keeps clusters intact where JS would split code units. */
let split = (sep: string, s: string): list(string) =>
  if (sep == "") {
    Unicode.to_list(s);
  } else {
    Js.string(s)##split_regExp(compile(quote(sep)))
    |> Js.str_array
    |> Js.to_array
    |> Array.to_list
    |> List.map(Js.to_string);
  };

/* Grapheme index of the first match at or after grapheme [start]; -1 if none. */
let search = (pattern: string, s: string, start: int): int => {
  let re = compile(pattern);
  re##.lastIndex := utf16_of_grapheme(s, start);
  switch (Js.Opt.to_option(re##exec(Js.string(s)))) {
  | None => (-1)
  | Some(handle) => grapheme_of_utf16(s, Js.match_result(handle)##.index)
  };
};
