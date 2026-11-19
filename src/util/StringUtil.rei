/* String helpers. Lengths and positions are in grapheme clusters where
   noted, otherwise in bytes. Regexps are JavaScript's. */

let repeat: (int, string) => string;
/** Truncates to that many grapheme clusters and appends "...". */
let abbreviate: (int, string) => string;
type regexp = Js_of_ocaml.Regexp.regexp;
let regexp: string => regexp;
let match: (regexp, string) => bool;
type unicode_regexp = Js_of_ocaml.Js.t(Js_of_ocaml.Js.regExp);
/** A regexp with the `u` flag where the browser accepts the pattern with it,
    so `.` and classes match codepoints. Pass `~global=true` only for
    replace and split. */
let unicode_regexp: (~global: bool=?, string) => unicode_regexp;
let unicode_match: (unicode_regexp, string) => bool;
/** `replace(r, s, by)` replaces every match. */
let replace: (regexp, string, string) => string;
/** Splits on a literal separator. */
let plain_split: (string, string) => list(string);
/** Despite the name, the first argument is compiled as a regexp. */
let plain_match: (string, string) => bool;
/** Despite the name, the first argument is compiled as a regexp. */
let plain_replace: (string, string, string) => string;
/** `plain_search(needle, s, from)`: the index of the first literal
    occurrence at or after `from`, or -1. */
let plain_search: (string, string, int) => int;
let to_lines: string => list(string);
/** In grapheme clusters; for layout use `Unicode.Width.columns_of_string`. */
let max_line_width: string => int;
let num_linebreaks: string => int;
let escape_linebreaks: string => string;
let unescape_linebreaks: string => string;
/** Normalizes line endings to `\n` and strips leading spaces and tabs from
    every line. */
let trim_leading: string => string;
/** Splits around the first occurrence of the character. */
let split_first: (~on: char, string) => option((string, string));
let strip_final_newline: string => string;
let isEmptyOrWhitespace: string => bool;
/** URI-component encoding (`encodeURIComponent`); nothing is compressed. */
let compress: string => string;
/** Inverse of `compress`. */
let decompress: string => string;
/** Drops everything but ASCII letters, digits, `_` and `-`. */
let sanitize_filename: string => string;
let trim_trailing_whitespace: string => string;
/** Every non-empty prefix, shortest first, cut on grapheme boundaries;
    `[""]` for the empty string. */
let prefixes: string => list(string);
let levenshtein_distance: (string, string) => int;
/** Edit distance with whole strings as the symbols. */
let levenshtein_list_distance: (list(string), list(string)) => int;
/** Drops double quotes and escapes newlines, so the result can sit inside a
    Hazel string literal. */
let sanitize_for_string_expression: string => string;
/** Drops backticks. */
let sanitize_for_label: string => string;
/** `subseq_search(s, sub)`: whether `sub`'s characters appear in `s` in
    order, not necessarily adjacent. Case-insensitive; spaces in `sub` are
    ignored. */
let subseq_search: (string, string) => bool;
