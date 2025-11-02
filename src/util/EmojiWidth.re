open Js_of_ocaml;

/* Utilities for classifying grapheme clusters (especially emoji) by the
   number of editor columns they occupy. The measurements stay in OCaml so
   both rendering and caret logic can share them. */

type unsafe_any = Js.Unsafe.any;

/* Minimal representation of glyph widths.  We purposely limit ourselves to
   one or two columns for now, keeping layout integer-aligned. */
type width =
  | One
  | Two;

let columns_of_width = (width: width): int =>
  switch (width) {
  | One => 1
  | Two => 2
  };

/* JavaScript RegExp for the Unicode Extended_Pictographic block. */
let emoji_re: unsafe_any =
  Js.Unsafe.eval_string("/\\p{Extended_Pictographic}/u");

/* Convert an OCaml string into a list of grapheme clusters using the
   shared UnicodeGrapheme module. */
let graphemes = (s: string): list(string) =>
  UnicodeGrapheme.to_array(s) |> Array.to_list;

/* Treat anything in the pictographic block as a wide glyph. */
let is_emoji_cluster = (cluster: string): bool =>
  Js.to_bool(
    Js.Unsafe.fun_call(
      Js.Unsafe.get(emoji_re, "test"),
      [|Js.Unsafe.inject(Js.string(cluster))|],
    ),
  );

let classify_cluster = (cluster: string): width =>
  is_emoji_cluster(cluster) ? Two : One;

let columns_of_cluster = (cluster: string): int =>
  columns_of_width(classify_cluster(cluster));

/* Total columns used by a single-line string. */
let columns_of_string = (s: string): int =>
  graphemes(s)
  |> List.fold_left((acc, cluster) => acc + columns_of_cluster(cluster), 0);

let max_columns = (lines: list(string)): int =>
  lines
  |> List.fold_left(
       (acc, line) => {
         let width = columns_of_string(line);
         width > acc ? width : acc;
       },
       0,
     );

let split_lines = (s: string): list(string) =>
  String.split_on_char('\n', s);

/* Tuple `(rows, cols)` that matches Hazel's measurement semantics. */
let bounding_box_for = (s: string): (int, int) => {
  let lines = split_lines(s);
  let length = List.length(lines);
  let row = length <= 0 ? 0 : length - 1;
  let col = max_columns(lines);
  (row, col);
};

/* Cache prefix arrays per string to avoid repeated segmentation when
   caret logic revisits the same token many times. */
let prefix_cache: Hashtbl.t(string, array(int)) = Hashtbl.create(128);

let prefix_columns = (s: string): array(int) =>
  switch (Hashtbl.find_opt(prefix_cache, s)) {
  | Some(arr) => arr
  | None =>
    let clusters = graphemes(s);
    let len = List.length(clusters);
    let arr = Array.make(len + 1, 0);
    let rec fill = (idx: int, cs: list(string)) =>
      switch (cs) {
      | [] => ()
      | [hd, ...tl] =>
        arr[idx + 1] = arr[idx] + columns_of_cluster(hd);
        fill(idx + 1, tl);
      };
    fill(0, clusters);
    Hashtbl.add(prefix_cache, s, arr);
    arr;
  };

/* Columns consumed by the first `count` grapheme clusters of the string. */
let columns_through_prefix = (s: string, count: int): int => {
  let arr = prefix_columns(s);
  let max_idx = Array.length(arr) - 1;
  let idx =
    if (count < 0) {
      0;
    } else if (count > max_idx) {
      max_idx;
    } else {
      count;
    };
  arr[idx];
};
