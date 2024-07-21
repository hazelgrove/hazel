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

let split = Js_of_ocaml.Regexp.split;

let trim_leading = replace(regexp("\n[ ]*"), "\n");

let to_lines = (s: string): list(string) => split(regexp("\n"), s);

let line_widths = (s: string): list(int) =>
  s |> to_lines |> List.map(String.length);

let max_line_width = (s: string): int =>
  s |> line_widths |> List.fold_left(max, 0);

let num_lines = (s: string): int => s |> to_lines |> List.length;

let num_linebreaks = (s: string) =>
  List.init(String.length(s), String.get(s))
  |> List.fold_left((acc, c) => c == '\n' ? acc + 1 : acc, 0);

// let escape_linebreaks: string => string = replace(regexp("\n"), "\\\\n");

// let unescape_linebreaks: string => string = replace(regexp("\\\\n"), "\n");

//TODO(andrew): figure out why above dont work

let escape_linebreaks: string => string =
  Re.Str.global_replace(Re.Str.regexp("\n"), "\\n");

let unescape_linebreaks: string => string =
  Re.Str.global_replace(Re.Str.regexp("\\\\n"), "\n");
