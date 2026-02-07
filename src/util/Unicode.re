[@coverage exclude_file];
open Js_of_ocaml;

/* Lightweight Unicode grapheme helpers shared by text editing
   and measurement. We rely on Intl.Segmenter when available
   and fall back to simple JS iteration otherwise. */

type unsafe_any = Js.Unsafe.any;

let segmenter_src =
  "(function () {\n"
  ++ "  if (typeof Intl === 'undefined' || typeof Intl.Segmenter === 'undefined') {\n"
  ++ "    return undefined;\n"
  ++ "  }\n"
  ++ "  var segmenter = new Intl.Segmenter(undefined, { granularity: 'grapheme' });\n"
  ++ "  return function (input) {\n"
  ++ "    return Array.from(segmenter.segment(input), function (result) { return result.segment; });\n"
  ++ "  };\n"
  ++ "})()";

let segmenter_fn: Js.Optdef.t(unsafe_any) =
  Js.Unsafe.eval_string(segmenter_src);

let fallback_segmenter_src = "(function (input) { return Array.from(input); })";

let fallback_segmenter: unsafe_any =
  Js.Unsafe.eval_string(fallback_segmenter_src);

let to_js_array = (value: unsafe_any) => Js.Unsafe.coerce(value);

let graphemes = (s: string): array(string) => {
  let js_str = Js.string(s);
  let arr =
    switch (Js.Optdef.to_option(segmenter_fn)) {
    | Some(fn) =>
      to_js_array(Js.Unsafe.fun_call(fn, [|Js.Unsafe.inject(js_str)|]))
    | None =>
      to_js_array(
        Js.Unsafe.fun_call(
          fallback_segmenter,
          [|Js.Unsafe.inject(js_str)|],
        ),
      )
    };
  arr |> Js.to_array |> Array.map(Js.to_string);
};

let of_graphemes = (clusters: array(string)): string =>
  clusters |> Array.to_list |> String.concat("");

let length = (s: string): int => s |> graphemes |> Array.length;

let remove_nth = (s: string, idx: int): string => {
  let clusters = graphemes(s);
  let len = Array.length(clusters);
  if (idx < 0 || idx >= len) {
    invalid_arg("Grapheme.remove_nth");
  };
  if (len == 1) {
    "";
  } else {
    let result = Array.make(len - 1, "");
    Array.blit(clusters, 0, result, 0, idx);
    Array.blit(clusters, idx + 1, result, idx, len - idx - 1);
    of_graphemes(result);
  };
};

let insert_nth = (s: string, idx: int, fragment: string): string => {
  let clusters = graphemes(s);
  let insert = graphemes(fragment);
  let len = Array.length(clusters);
  let ins_len = Array.length(insert);
  if (idx < 0 || idx > len) {
    invalid_arg("Grapheme.insert_nth");
  };
  if (ins_len == 0) {
    s;
  } else {
    let target = Array.make(len + ins_len, "");
    let pos = idx;
    Array.blit(clusters, 0, target, 0, pos);
    Array.blit(insert, 0, target, pos, ins_len);
    Array.blit(clusters, pos, target, pos + ins_len, len - pos);
    of_graphemes(target);
  };
};

let split_nth = (s: string, idx: int): (string, string) => {
  let clusters = graphemes(s);
  let len = Array.length(clusters);
  if (idx < 0 || idx > len) {
    invalid_arg("Grapheme.split_nth");
  };
  let pos = idx;
  let left = Array.make(pos, "");
  let right = Array.make(len - pos, "");
  Array.blit(clusters, 0, left, 0, pos);
  Array.blit(clusters, pos, right, 0, len - pos);
  (of_graphemes(left), of_graphemes(right));
};

let remove_last = (s: string): string => remove_nth(s, length(s) - 1);

let remove_first = (s: string): string => remove_nth(s, 0);

let append = (a: string, b: string): string => {
  let clusters_a = graphemes(a);
  let clusters_b = graphemes(b);
  let len_a = Array.length(clusters_a);
  let len_b = Array.length(clusters_b);
  let target = Array.make(len_a + len_b, "");
  Array.blit(clusters_a, 0, target, 0, len_a);
  Array.blit(clusters_b, 0, target, len_a, len_b);
  of_graphemes(target);
};

let to_array = graphemes;

let to_list = s => to_array(s) |> Array.to_list;

let of_list = (lst: list(string)): string => String.concat("", lst);

module Width = {
  /* Utilities for classifying grapheme clusters (especially emoji) by the
     number of editor columns they occupy. The measurements stay in OCaml so
     both rendering and caret logic can share them. */

  type unsafe_any = Js.Unsafe.any;

  /* Minimal representation of glyph widths.  We purposely limit ourselves to
     one or two columns for now, keeping layout integer-aligned. */
  type t =
    | One
    | Two;

  let columns_of_width = (width: t): int =>
    switch (width) {
    | One => 1
    | Two => 2
    };

  /* JavaScript RegExp for the Unicode Extended_Pictographic block. */
  let emoji_re: unsafe_any =
    Js.Unsafe.eval_string("/\\p{Extended_Pictographic}/u");

  /* Convert an OCaml string into a list of grapheme clusters using the
     shared Grapheme module. */
  let graphemes = to_list;

  /* Treat anything in the pictographic block as a wide glyph. */
  let is_emoji_cluster = (cluster: string): bool =>
    Js.to_bool(
      Js.Unsafe.fun_call(
        Js.Unsafe.get(emoji_re, "test"),
        [|Js.Unsafe.inject(Js.string(cluster))|],
      ),
    );

  let classify_cluster = (cluster: string): t =>
    is_emoji_cluster(cluster) ? Two : One;

  let columns_of_cluster = (cluster: string): int =>
    columns_of_width(classify_cluster(cluster));

  /* Total columns used by a single-line string. */
  let columns_of_string = (s: string): int =>
    graphemes(s)
    |> List.fold_left(
         (acc, cluster) => acc + columns_of_cluster(cluster),
         0,
       );

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

  let column_to_grapheme_index = (s: string, col: int): int => {
    let clusters = to_array(s);
    let len = Array.length(clusters);
    let target_col = max(col, 0);
    let rec loop = (idx: int, acc: int): int =>
      if (idx >= len || acc >= target_col) {
        idx;
      } else {
        let cluster = clusters[idx];
        loop(idx + 1, acc + columns_of_cluster(cluster));
      };
    loop(0, 0);
  };
};
