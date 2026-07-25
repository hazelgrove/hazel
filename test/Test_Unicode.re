/* Unicode.length/graphemes/Width take three routes depending on the input
 * (ASCII byte scan, OCaml UTF-8 walk, Intl.Segmenter). These tests pin the
 * two fast routes against the Segmenter, which is the correctness reference,
 * and pin the column widths that the caret, Measured and the renderer all
 * share. Also covers the token classes whose regexes must range over
 * codepoints rather than UTF-8 bytes. */
open Alcotest;
open Util;
open Haz3lcore;
open Js_of_ocaml;

/* The reference: Intl.Segmenter with no fast paths in front of it. */
let reference_segmenter: Js.Unsafe.any =
  Js.Unsafe.eval_string(
    "(function (input) {\n"
    ++ "  var s = new Intl.Segmenter(undefined, { granularity: 'grapheme' });\n"
    ++ "  return Array.from(s.segment(input), function (r) { return r.segment; });\n"
    ++ "})",
  );

let reference_graphemes = (s: string): list(string) =>
  Js.Unsafe.fun_call(
    reference_segmenter,
    [|Js.Unsafe.inject(Js.string(s))|],
  )
  |> Js.Unsafe.coerce
  |> Js.to_array
  |> Array.map(Js.to_string)
  |> Array.to_list;

let corpus = [
  ("empty", ""),
  ("ascii", "let x = 1 in"),
  ("ascii with tab and newline", "a\tb\nc"),
  ("crlf", "a\r\nb"),
  ("latin-1", "\xc2\xa2\xc2\xa3\xc2\xa9"), /* ¢£© */
  ("latin accents precomposed", "caf\xc3\xa9"), /* café */
  ("latin accents combining", "cafe\xcc\x81"), /* cafe + U+0301 */
  ("greek", "\xce\xbb\xce\xa3"), /* λΣ */
  ("cjk", "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e"), /* 日本語 */
  ("fullwidth", "\xef\xbd\x81\xef\xbd\x82"), /* ａｂ */
  ("arrows", "\xe2\x86\x92\xe2\x87\x92"), /* →⇒ */
  ("emoji", "\xf0\x9f\x98\x80"), /* 😀 */
  ("emoji in text", "a\xf0\x9f\x98\x80b"),
  ("heart with VS16", "\xe2\x9d\xa4\xef\xb8\x8f"), /* ❤️ */
  ("skin tone", "\xf0\x9f\x91\x8d\xf0\x9f\x8f\xbd"), /* 👍🏽 */
  (
    "zwj family",
    "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x91\xa7",
  ),
  ("regional indicator flag", "\xf0\x9f\x87\xba\xf0\x9f\x87\xb8"), /* 🇺🇸 */
  ("hangul syllable", "\xed\x95\x9c\xea\xb5\xad"), /* 한국 */
  ("hangul jamo", "\xe1\x84\x92\xe1\x85\xa1\xe1\x86\xab"), /* ᄒ + ᅡ + ᆫ */
  ("mixed", "x = \"\xf0\x9f\x98\x80\xe6\x97\xa5\" # \xc3\xa9 #"),
];

/* Columns computed the slow way: segment with the reference, then add up the
 * per-cluster widths. */
let reference_columns = (s: string): int =>
  reference_graphemes(s)
  |> List.fold_left(
       (acc, c) => acc + Unicode.Width.columns_of_cluster(c),
       0,
     );

let agrees_with_segmenter = ((name, s)) =>
  test_case(
    "segmenter agreement: " ++ name,
    `Quick,
    () => {
      let reference = reference_graphemes(s);
      check(list(string), "clusters", reference, Unicode.to_list(s));
      check(int, "length", List.length(reference), Unicode.length(s));
      check(string, "roundtrip", s, Unicode.of_list(Unicode.to_list(s)));
      /* columns, on each line separately: bounding_box measures per line */
      List.iter(
        line =>
          check(
            int,
            "columns",
            reference_columns(line),
            Unicode.Width.columns_of_string(line),
          ),
        String.split_on_char('\n', s),
      );
      /* every prefix length, including out-of-range ones */
      let n = List.length(reference);
      for (k in 0 to n + 2) {
        let expected =
          reference
          |> List.filteri((i, _) => i < k)
          |> List.fold_left(
               (acc, c) => acc + Unicode.Width.columns_of_cluster(c),
               0,
             );
        check(
          int,
          Printf.sprintf("columns_through_prefix %d", k),
          expected,
          Unicode.Width.columns_through_prefix(s, k),
        );
      };
      /* column_to_grapheme_index inverts columns_through_prefix */
      for (i in 0 to n) {
        let col = Unicode.Width.columns_through_prefix(s, i);
        check(
          int,
          Printf.sprintf("column_to_grapheme_index at col %d", col),
          i,
          Unicode.Width.column_to_grapheme_index(s, col),
        );
      };
    },
  );

let bounding_box_agrees = ((name, s)) =>
  test_case(
    "bounding box: " ++ name,
    `Quick,
    () => {
      let lines = String.split_on_char('\n', s);
      let expected = (
        List.length(lines) - 1,
        List.fold_left(
          (acc, l) => max(acc, reference_columns(l)),
          0,
          lines,
        ),
      );
      check(
        pair(int, int),
        "bounding box",
        expected,
        Unicode.Width.bounding_box_for(s),
      );
    },
  );

let width = (s: string): int => Unicode.Width.columns_of_string(s);

let width_tests = [
  test_case(
    "narrow clusters are one column",
    `Quick,
    () => {
      check(int, "ascii", 1, width("a"));
      check(int, "cent", 1, width("\xc2\xa2"));
      check(int, "copyright", 1, width("\xc2\xa9"));
      check(int, "arrow", 1, width("\xe2\x86\x92"));
      check(int, "lambda", 1, width("\xce\xbb"));
      check(int, "precomposed e-acute", 1, width("\xc3\xa9"));
      check(int, "combining e-acute", 1, width("e\xcc\x81"));
    },
  ),
  test_case(
    "wide clusters are two columns",
    `Quick,
    () => {
      check(int, "emoji", 2, width("\xf0\x9f\x98\x80"));
      check(int, "cjk", 2, width("\xe6\x97\xa5"));
      check(int, "kana", 2, width("\xe3\x81\x82"));
      check(int, "hangul syllable", 2, width("\xed\x95\x9c"));
      check(int, "fullwidth latin", 2, width("\xef\xbd\x81"));
      check(
        int,
        "VS16 emoji presentation",
        2,
        width("\xe2\x9d\xa4\xef\xb8\x8f"),
      );
      check(int, "flag", 2, width("\xf0\x9f\x87\xba\xf0\x9f\x87\xb8"));
      check(
        int,
        "zwj family",
        2,
        width(
          "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x91\xa7",
        ),
      );
    },
  ),
  test_case(
    "ASCII fast path recognises exactly the safe strings",
    `Quick,
    () => {
      check(bool, "empty", true, Unicode.is_simple_ascii(""));
      check(bool, "ascii", true, Unicode.is_simple_ascii("let x = 1"));
      check(bool, "newline", true, Unicode.is_simple_ascii("a\nb"));
      /* CR clusters with a following LF, so it is not on the fast path */
      check(bool, "cr", false, Unicode.is_simple_ascii("a\r\nb"));
      check(bool, "latin-1", false, Unicode.is_simple_ascii("\xc2\xa2"));
      check(
        bool,
        "emoji",
        false,
        Unicode.is_simple_ascii("\xf0\x9f\x98\x80"),
      );
    },
  ),
];

/* Token classification: these regexes exclude `¿` and whitespace, which only
 * works if the character class ranges over codepoints. With byte-oriented
 * matching, `¿` (0xC2 0xBF) rejected every character encoded with either
 * byte, and `\s` rejected every character containing byte 0xA0. */
let operator_tests = [
  test_case(
    "non-ASCII symbols are potential operators",
    `Quick,
    () => {
      let potential = s =>
        check(bool, s, true, Token.is_potential_operator(s));
      potential("\xc2\xa2"); /* ¢ -- lead byte 0xC2, same as ¿ */
      potential("\xc2\xa3"); /* £ */
      potential("\xc2\xa9"); /* © */
      potential("\xc2\xab"); /* « */
      potential("\xc2\xb0"); /* ° */
      potential("\xc2\xb1"); /* ± */
      potential("\xc2\xac"); /* ¬ */
      potential("\xc5\xbf"); /* ſ -- trailing byte 0xBF, same as ¿ */
      potential("\xc3\xa0"); /* à -- trailing byte 0xA0, which \s matches */
      potential("\xe2\x86\x92"); /* → */
      potential("\xce\xbb"); /* λ */
      potential("\xf0\x9f\x98\x80"); /* 😀 */
      potential("\xe6\x97\xa5"); /* 日 */
      potential("+");
      potential("|>");
    },
  ),
  test_case(
    "excluded characters are not potential operators",
    `Quick,
    () => {
      let not_potential = s =>
        check(bool, s, false, Token.is_potential_operator(s));
      not_potential(Token.implicit_hole_marker); /* ¿ */
      not_potential("+\xc2\xbf");
      not_potential("a");
      not_potential("+ ");
      not_potential("+\n");
      not_potential("(");
      not_potential("\"");
      not_potential("#");
      not_potential("\xc2\xa0"); /* NBSP is Unicode whitespace */
      not_potential("");
    },
  ),
  test_case(
    "the implicit-hole marker does not glue onto neighbours",
    `Quick,
    () => {
      check(
        bool,
        "begins_with on marker",
        false,
        Token.begins_with_potential_operator(
          Token.implicit_hole_marker ++ ",",
        ),
      );
      check(
        bool,
        "begins_with on operator",
        true,
        Token.begins_with_potential_operator("+,"),
      );
      check(
        bool,
        "begins_with on non-ASCII operator",
        true,
        Token.begins_with_potential_operator("\xc2\xa2+"),
      );
    },
  ),
  test_case(
    "potential tokens agree for a representative set",
    `Quick,
    () => {
      let potential = s => check(bool, s, true, Token.is_potential_token(s));
      potential("\xc2\xa2"); /* ¢ */
      potential("\xc5\xbf"); /* ſ */
      potential("\xe2\x86\x92"); /* → */
      potential("\xf0\x9f\x98\x80"); /* 😀 */
      potential("\xe6\x97\xa5"); /* 日 */
      potential(Token.implicit_hole_marker);
      /* non-ASCII inside quoted forms */
      potential("\"caf\xc3\xa9\"");
      potential("#\xf0\x9f\x98\x80#");
      potential("`\xe6\x97\xa5\xe6\x9c\xac`");
    },
  ),
  test_case(
    "quoted-label and livelit parsing stay in grapheme units",
    `Quick,
    () => {
      check(
        string,
        "strip quoted label",
        "caf\xc3\xa9",
        Token.strip_quotes(~quote=Token.label_delim, "`caf\xc3\xa9`"),
      );
      check(
        string,
        "strip string quotes",
        "\xf0\x9f\x98\x80",
        Token.strip_quotes("\"\xf0\x9f\x98\x80\""),
      );
      check(
        string,
        "parse livelit",
        "slider",
        Token.parse_livelit("^slider"),
      );
      check(
        option(string),
        "projector invoke",
        Some("fold"),
        Token.of_projector_invoke("^^fold"),
      );
    },
  ),
];

/* The keydown handler decides "is this a character key?" by measuring
 * KeyboardEvent.key. Measuring it in BYTES excluded every non-ASCII key. */
let key_action = (k: string): option(Action.t) =>
  Web.Keyboard.handle_key_event({
    key: Key.D(k),
    code: "",
    sys: Key.Mac,
    shift: Key.Up,
    meta: Key.Up,
    ctrl: Key.Up,
    alt: Key.Up,
  });

let inserted = (k: string): option(string) =>
  switch (key_action(k)) {
  | Some(Insert(s)) => Some(s)
  | _ => None
  };

let keyboard_tests = [
  test_case(
    "character keys insert themselves",
    `Quick,
    () => {
      let inserts = k => check(option(string), k, Some(k), inserted(k));
      inserts("a");
      inserts("+");
      inserts("\xc3\xa9"); /* é, two bytes */
      inserts("\xc2\xa2"); /* ¢ */
      inserts("\xe6\x97\xa5"); /* 日, three bytes */
      inserts("\xf0\x9f\x98\x80"); /* 😀, four bytes */
      check(option(string), "Enter", Some("\n"), inserted("Enter"));
    },
  ),
  test_case(
    "named keys are not insertions",
    `Quick,
    () => {
      let named = k => check(option(string), k, None, inserted(k));
      named("Shift");
      named("Control");
      named("Alt");
      named("Meta");
      named("CapsLock");
      named("Escape");
      named("Dead");
      named("Process");
      named("F1");
      named("F12");
      named("ArrowLeft");
      named("ArrowRight");
      named("Home");
      named("End");
      named("PageUp");
      named("Unidentified");
    },
  ),
];

let tests = [
  (
    "Unicode.Segmenter",
    List.map(agrees_with_segmenter, corpus)
    @ List.map(bounding_box_agrees, corpus),
  ),
  ("Unicode.Width", width_tests),
  ("Token.Potential", operator_tests),
  ("Keyboard.Insert", keyboard_tests),
];
