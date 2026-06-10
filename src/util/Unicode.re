[@coverage exclude_file];

/* Lightweight Unicode grapheme helpers shared by text editing
   and measurement. Grapheme segmentation is UAX #29 via the pure-OCaml
   uuseg library (previously the browser's Intl.Segmenter). */

let graphemes = (s: string): array(string) => {
  let (clusters, _) =
    Uuseg_string.fold_utf_8(
      `Grapheme_cluster,
      ((acc, buf), cluster) => ([cluster, ...acc], buf),
      ([], ()),
      s,
    );
  clusters |> List.rev |> Array.of_list;
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

  /* Convert an OCaml string into a list of grapheme clusters using the
     shared Grapheme module. */
  let graphemes = to_list;

  /* Treat anything in the pictographic block as a wide glyph (the
     Unicode Extended_Pictographic property, via uucp — previously a
     JS /\p{Extended_Pictographic}/u regexp). A cluster is "emoji" if
     any of its scalars is extended-pictographic, matching the JS
     regexp's unanchored search. */
  let is_emoji_cluster = (cluster: string): bool => {
    let len = String.length(cluster);
    let rec scan = i =>
      if (i >= len) {
        false;
      } else {
        let d = String.get_utf_8_uchar(cluster, i);
        let u = Uchar.utf_decode_uchar(d);
        Uucp.Emoji.is_extended_pictographic(u)
          ? true : scan(i + Uchar.utf_decode_length(d));
      };
    scan(0);
  };

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
