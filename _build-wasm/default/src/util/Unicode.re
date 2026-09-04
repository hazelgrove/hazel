[@coverage exclude_file];
open Js_of_ocaml;

/* Lightweight Unicode grapheme helpers shared by text editing and
   measurement.

   Everything the editor measures or edits is in GRAPHEME CLUSTERS (what a
   user calls a character) and, for layout, in COLUMNS (clusters like emoji
   and CJK occupy two). Both are computed here so caret arithmetic, the
   measurement pass and rendering cannot disagree.

   Three layers, cheapest first:

   1. Pure-ASCII strings: one byte is one cluster is one column, so we never
      leave OCaml and never allocate. This is the overwhelmingly common case
      in a code editor, and it is why width-awareness can be applied to every
      token rather than only to string literals.
   2. Strings whose codepoints all stand alone as clusters (see
      `standalone_ranges`): we walk the UTF-8 in OCaml.
   3. Anything else -- combining marks, ZWJ sequences, flags, Hangul jamo --
      goes to Intl.Segmenter, which remains the correctness reference. The
      fast paths are pinned against it in Test_Unicode. */

type unsafe_any = Js.Unsafe.any;

/* --- Range tables ---------------------------------------------------- */

/* Ranges are written as comma-separated `lo-hi` hex pairs: an OCaml array
   literal of the same data is several hundred lines after formatting. */
let parse_ranges = (src: string): array(int) =>
  src
  |> String.split_on_char(',')
  |> List.concat_map(item =>
       switch (String.split_on_char('-', item)) {
       | [lo, hi] => [int_of_string("0x" ++ lo), int_of_string("0x" ++ hi)]
       | [lo] =>
         let cp = int_of_string("0x" ++ lo);
         [cp, cp];
       | _ => []
       }
     )
  |> Array.of_list;

/* [ranges] is a flat sorted array of inclusive [lo0, hi0, lo1, hi1, ...]. */
let in_ranges = (ranges: array(int), cp: int): bool => {
  let lo = ref(0);
  let hi = ref(Array.length(ranges) / 2 - 1);
  let found = ref(false);
  while (lo^ <= hi^ && ! found^) {
    let mid = (lo^ + hi^) / 2;
    if (cp < ranges[2 * mid]) {
      hi := mid - 1;
    } else if (cp > ranges[2 * mid + 1]) {
      lo := mid + 1;
    } else {
      found := true;
    };
  };
  found^;
};

/* --- UTF-8 ----------------------------------------------------------- */

/* True when every byte is ASCII and none is CR. CR is excluded because CRLF
   is a single cluster (UAX #29 GB3); every other ASCII byte is its own
   cluster of width one, so for these strings bytes = clusters = columns. */
let is_simple_ascii = (s: string): bool => {
  let n = String.length(s);
  let i = ref(0);
  let ok = ref(true);
  while (ok^ && i^ < n) {
    let c = Char.code(String.unsafe_get(s, i^));
    if (c >= 0x80 || c == 0x0D) {
      ok := false;
    } else {
      incr(i);
    };
  };
  ok^;
};

/* Byte index just past the UTF-8 sequence starting at [i]. Malformed lead
   bytes advance by one so scans always terminate. */
let next_offset = (s: string, i: int): int => {
  let b0 = Char.code(String.unsafe_get(s, i));
  let len =
    if (b0 < 0xC0) {
      1;
    } else if (b0 < 0xE0) {
      2;
    } else if (b0 < 0xF0) {
      3;
    } else {
      4;
    };
  min(i + len, String.length(s));
};

/* Codepoint of the UTF-8 sequence starting at [i]; malformed sequences
   degrade to their lead byte. */
let codepoint_at = (s: string, i: int): int => {
  let n = String.length(s);
  let cont = j => Char.code(String.unsafe_get(s, j)) land 0x3F;
  let b0 = Char.code(String.unsafe_get(s, i));
  if (b0 < 0xC0) {
    b0;
  } else if (b0 < 0xE0) {
    i + 1 < n ? (b0 land 0x1F) lsl 6 lor cont(i + 1) : b0;
  } else if (b0 < 0xF0) {
    i + 2 < n
      ? (b0 land 0x0F) lsl 12 lor cont(i + 1) lsl 6 lor cont(i + 2) : b0;
  } else {
    i + 3 < n
      ? (b0 land 0x07)
        lsl 18
        lor cont(i + 1)
        lsl 12
        lor cont(i + 2)
        lsl 6
        lor cont(i + 3)
      : b0;
  };
};

/* Codepoints that are guaranteed to be a grapheme cluster on their own: none
   of them is Extend, SpacingMark, Prepend, ZWJ, Regional_Indicator or a
   Hangul jamo, so none can join a neighbour under UAX #29. If every codepoint
   of a string is in this set, its clusters are exactly its codepoints.

   Deliberately a WHITELIST covering the ranges a code editor actually sees
   (Latin, Greek/Cyrillic, punctuation and symbols, CJK, kana, emoji);
   anything outside falls back to Intl.Segmenter. Derived by testing every
   candidate codepoint against Intl.Segmenter itself (c joins neither an
   adjacent "a" nor a copy of itself), so it cannot claim more than the
   reference implementation allows. */
let standalone_ranges =
  parse_ranges(
    "0009-000A,0020-007E,00A0-02FF,0370-0482,048A-058F,2000-200B,200E-20CF,20F1-2BFF,2E80-3029,3030-303E,3041-3098,309B-A4CF,A97D-A97F,AC00-D7AF,D7C7-D7CA,D7FC-D7FF,F900-FAFF,FE10-FE1F,FE30-FE6F,FF01-FF60,FFE0-FFE6,1F000-1F1E5,1F200-1F3FA,1F400-1FAFF,20000-2FFFD,30000-3FFFD",
  );

/* Does this string's cluster structure coincide with its codepoints? */
let clusters_are_codepoints = (s: string): bool => {
  let n = String.length(s);
  let i = ref(0);
  let ok = ref(true);
  while (ok^ && i^ < n) {
    if (!in_ranges(standalone_ranges, codepoint_at(s, i^))) {
      ok := false;
    } else {
      i := next_offset(s, i^);
    };
  };
  ok^;
};

/* --- Invisible characters -------------------------------------------- */

/* Codepoints that render as nothing, or as blank indistinguishable from a
   space: zero-width and bidirectional controls, soft hyphen, fill
   characters, variation selectors, tags, and the non-ASCII space
   separators. A cluster made only of these is drawn as a labeled
   placeholder cell (GraphemeView) so it cannot hide in code. The test is
   whole-cluster so attached forms stay untouched: a VS16 or ZWJ inside an
   emoji cluster has a visible base. */
let invisible_ranges =
  parse_ranges(
    "00A0,00AD,034F,061C,115F-1160,17B4-17B5,180B-180F,2000-200F,2028-202E,205F-2064,2066-206F,3000,3164,FE00-FE0F,FEFF,FFA0,FFF9-FFFB,1D173-1D17A,E0000-E007F,E0100-E01EF",
  );

let is_invisible_cluster = (cluster: string): bool => {
  let n = String.length(cluster);
  n > 0
  && !is_simple_ascii(cluster)
  && {
    let i = ref(0);
    let ok = ref(true);
    while (ok^ && i^ < n) {
      if (in_ranges(invisible_ranges, codepoint_at(cluster, i^))) {
        i := next_offset(cluster, i^);
      } else {
        ok := false;
      };
    };
    ok^;
  };
};

/* --- Segmentation ---------------------------------------------------- */

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

/* The reference implementation: one round trip through JS per call. */
let segmented = (s: string): array(string) => {
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

let ascii_chars = (s: string): array(string) =>
  Array.init(String.length(s), i =>
    String.make(1, String.unsafe_get(s, i))
  );

let codepoint_chars = (s: string): array(string) => {
  let n = String.length(s);
  let acc = ref([]);
  let i = ref(0);
  while (i^ < n) {
    let next = next_offset(s, i^);
    acc := [String.sub(s, i^, next - i^), ...acc^];
    i := next;
  };
  acc^ |> List.rev |> Array.of_list;
};

let count_codepoints = (s: string): int => {
  let n = String.length(s);
  let count = ref(0);
  let i = ref(0);
  while (i^ < n) {
    incr(count);
    i := next_offset(s, i^);
  };
  count^;
};

let graphemes = (s: string): array(string) =>
  if (is_simple_ascii(s)) {
    ascii_chars(s);
  } else if (clusters_are_codepoints(s)) {
    codepoint_chars(s);
  } else {
    segmented(s);
  };

let of_graphemes = (clusters: array(string)): string =>
  clusters |> Array.to_list |> String.concat("");

let length = (s: string): int =>
  if (is_simple_ascii(s)) {
    String.length(s);
  } else if (clusters_are_codepoints(s)) {
    count_codepoints(s);
  } else {
    Array.length(segmented(s));
  };

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

/* Concatenating two strings never merges their clusters into a different
   sequence of BYTES, so this is plain concatenation. */
let append = (a: string, b: string): string => a ++ b;

let to_array = graphemes;

let to_list = s => to_array(s) |> Array.to_list;

let of_list = (lst: list(string)): string => String.concat("", lst);

/* --- Normalization --------------------------------------------------- */

let normalize_nfc = (s: string): string =>
  is_simple_ascii(s)
    ? s
    : Js.to_string(
        Js.Unsafe.meth_call(
          Js.string(s),
          "normalize",
          [|Js.Unsafe.inject(Js.string("NFC"))|],
        ),
      );

/* NFC-normalize code text, leaving string literal contents byte-for-byte
   intact (UTS #55: never silently normalize literals). Some paste sources
   (notably macOS) emit decomposed accents, which would otherwise mint
   identifiers that never match their precomposed typed spelling. Literals
   are single-line; a backslash-escaped quote is honored as an escape (the
   text lexer's rule, a superset of the editor's) and an unterminated
   literal runs to the end of its line. */
let nfc_outside_strings = (s: string): string =>
  if (is_simple_ascii(s)) {
    s;
  } else {
    let n = String.length(s);
    let buf = Buffer.create(n);
    let flush_code = (start, stop) =>
      if (stop > start) {
        Buffer.add_string(
          buf,
          normalize_nfc(String.sub(s, start, stop - start)),
        );
      };
    let i = ref(0);
    let code_start = ref(0);
    while (i^ < n) {
      if (String.unsafe_get(s, i^) == '"') {
        flush_code(code_start^, i^);
        let j = ref(i^ + 1);
        while (j^ < n
               && String.unsafe_get(s, j^) != '"'
               && String.unsafe_get(s, j^) != '\n') {
          j :=
            String.unsafe_get(s, j^) == '\\' && j^ + 1 < n ? j^ + 2 : j^ + 1;
        };
        let stop = j^ < n && String.unsafe_get(s, j^) == '"' ? j^ + 1 : j^;
        Buffer.add_string(buf, String.sub(s, i^, stop - i^));
        i := stop;
        code_start := stop;
      } else {
        incr(i);
      };
    };
    flush_code(code_start^, n);
    Buffer.contents(buf);
  };

module Width = {
  /* Column widths for grapheme clusters. Layout stays integer-aligned: a
     cluster is either one or two columns. */

  type t =
    | One
    | Two;

  let columns_of_width = (width: t): int =>
    switch (width) {
    | One => 1
    | Two => 2
    };

  /* East Asian Wide and Fullwidth codepoints (UAX #11, Unicode 16.0), the
     standard definition of "renders two columns" -- it covers CJK, kana,
     Hangul syllables, fullwidth forms AND emoji with default emoji
     presentation. Derived from unicodedata.east_asian_width. */
  let wide_ranges =
    parse_ranges(
      "1100-115F,231A-231B,2329-232A,23E9-23EC,23F0,23F3,25FD-25FE,2614-2615,2630-2637,2648-2653,267F,268A-268F,2693,26A1,26AA-26AB,26BD-26BE,26C4-26C5,26CE,26D4,26EA,26F2-26F3,26F5,26FA,26FD,2705,270A-270B,2728,274C,274E,2753-2755,2757,2795-2797,27B0,27BF,2B1B-2B1C,2B50,2B55,2E80-2E99,2E9B-2EF3,2F00-2FD5,2FF0-303E,3041-3096,3099-30FF,3105-312F,3131-318E,3190-31E5,31EF-321E,3220-3247,3250-A48C,A490-A4C6,A960-A97C,AC00-D7A3,F900-FAFF,FE10-FE19,FE30-FE52,FE54-FE66,FE68-FE6B,FF01-FF60,FFE0-FFE6,16FE0-16FE4,16FF0-16FF1,17000-187F7,18800-18CD5,18CFF-18D08,1AFF0-1AFF3,1AFF5-1AFFB,1AFFD-1AFFE,1B000-1B122,1B132,1B150-1B152,1B155,1B164-1B167,1B170-1B2FB,1D300-1D356,1D360-1D376,1F004,1F0CF,1F18E,1F191-1F19A,1F200-1F202,1F210-1F23B,1F240-1F248,1F250-1F251,1F260-1F265,1F300-1F320,1F32D-1F335,1F337-1F37C,1F37E-1F393,1F3A0-1F3CA,1F3CF-1F3D3,1F3E0-1F3F0,1F3F4,1F3F8-1F43E,1F440,1F442-1F4FC,1F4FF-1F53D,1F54B-1F54E,1F550-1F567,1F57A,1F595-1F596,1F5A4,1F5FB-1F64F,1F680-1F6C5,1F6CC,1F6D0-1F6D2,1F6D5-1F6D7,1F6DC-1F6DF,1F6EB-1F6EC,1F6F4-1F6FC,1F7E0-1F7EB,1F7F0,1F90C-1F93A,1F93C-1F945,1F947-1F9FF,1FA70-1FA7C,1FA80-1FA89,1FA8F-1FAC6,1FACE-1FADC,1FADF-1FAE9,1FAF0-1FAF8,20000-2FFFD,30000-3FFFD",
    );

  /* A flag is a pair of regional indicators; each pair renders two columns. */
  let regional_indicator = cp => cp >= 0x1F1E6 && cp <= 0x1F1FF;

  /* VS16 asks for emoji presentation, which is two columns even when the
     base character is narrow (e.g. U+2764 U+FE0F). */
  let emoji_presentation_selector = 0xFE0F;

  let is_wide_cp = (cp: int): bool =>
    in_ranges(wide_ranges, cp) || regional_indicator(cp);

  let rec has_vs16 = (cluster: string, i: int): bool =>
    i >= String.length(cluster)
      ? false
      : codepoint_at(cluster, i) == emoji_presentation_selector
          ? true : has_vs16(cluster, next_offset(cluster, i));

  let classify_cluster = (cluster: string): t =>
    if (cluster == "" || is_simple_ascii(cluster)) {
      One;
    } else if (is_wide_cp(codepoint_at(cluster, 0))) {
      Two;
    } else if (has_vs16(cluster, next_offset(cluster, 0))) {
      Two;
    } else {
      One;
    };

  let is_wide_cluster = (cluster: string): bool =>
    classify_cluster(cluster) == Two;

  let columns_of_cluster = (cluster: string): int =>
    columns_of_width(classify_cluster(cluster));

  let graphemes = to_list;

  let columns_of_cp = (cp: int): int => is_wide_cp(cp) ? 2 : 1;

  /* Total columns used by a single-line string. */
  let columns_of_string = (s: string): int =>
    if (is_simple_ascii(s)) {
      String.length(s);
    } else if (clusters_are_codepoints(s)) {
      /* VS16 is not standalone, so per-codepoint widths are exact here. */
      let n = String.length(s);
      let acc = ref(0);
      let i = ref(0);
      while (i^ < n) {
        acc := acc^ + columns_of_cp(codepoint_at(s, i^));
        i := next_offset(s, i^);
      };
      acc^;
    } else {
      segmented(s)
      |> Array.fold_left((acc, c) => acc + columns_of_cluster(c), 0);
    };

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

  let count_char = (c: char, s: string): int => {
    let n = String.length(s);
    let acc = ref(0);
    for (i in 0 to n - 1) {
      if (String.unsafe_get(s, i) == c) {
        incr(acc);
      };
    };
    acc^;
  };

  /* Tuple `(rows, cols)` that matches Hazel's measurement semantics. */
  let bounding_box_for = (s: string): (int, int) =>
    if (is_simple_ascii(s)) {
      /* Widest line, in bytes, plus one row per linebreak. */
      let n = String.length(s);
      let best = ref(0);
      let start = ref(0);
      for (i in 0 to n - 1) {
        if (String.unsafe_get(s, i) == '\n') {
          if (i - start^ > best^) {
            best := i - start^;
          };
          start := i + 1;
        };
      };
      if (n - start^ > best^) {
        best := n - start^;
      };
      (count_char('\n', s), best^);
    } else {
      let lines = split_lines(s);
      let length = List.length(lines);
      let row = length <= 0 ? 0 : length - 1;
      (row, max_columns(lines));
    };

  /* Columns consumed by the first [count] grapheme clusters of [s]. */
  let columns_through_prefix = (s: string, count: int): int =>
    if (count <= 0) {
      0;
    } else if (is_simple_ascii(s)) {
      min(count, String.length(s));
    } else if (clusters_are_codepoints(s)) {
      let n = String.length(s);
      let acc = ref(0);
      let seen = ref(0);
      let i = ref(0);
      while (i^ < n && seen^ < count) {
        acc := acc^ + columns_of_cp(codepoint_at(s, i^));
        incr(seen);
        i := next_offset(s, i^);
      };
      acc^;
    } else {
      let clusters = segmented(s);
      let stop = min(count, Array.length(clusters));
      let acc = ref(0);
      for (i in 0 to stop - 1) {
        acc := acc^ + columns_of_cluster(clusters[i]);
      };
      acc^;
    };

  /* Inverse of `columns_through_prefix`: the cluster index reached by
     advancing [col] columns from the start of [s]. */
  let column_to_grapheme_index = (s: string, col: int): int =>
    if (col <= 0) {
      0;
    } else if (is_simple_ascii(s)) {
      min(col, String.length(s));
    } else {
      let clusters = to_array(s);
      let len = Array.length(clusters);
      let rec loop = (idx: int, acc: int): int =>
        if (idx >= len || acc >= col) {
          idx;
        } else {
          loop(idx + 1, acc + columns_of_cluster(clusters[idx]));
        };
      loop(0, 0);
    };
};
