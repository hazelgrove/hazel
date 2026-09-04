/* Unicode.length/graphemes/Width take three routes depending on the input
 * (ASCII byte scan, OCaml UTF-8 walk, Intl.Segmenter). These tests pin the
 * two fast routes against the Segmenter, which is the correctness reference,
 * and pin the column widths that the caret, Measured and the renderer all
 * share. Also covers the token classes whose regexes must range over
 * codepoints rather than UTF-8 bytes. */
open Alcotest;
open Util_web;
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

/* Token classification. Operators are an explicit, closed list: the 18 ASCII
 * characters that were operator characters before names took Unicode, plus a
 * whitelist of the non-ASCII characters Hazel emits as tokens. Everything
 * else non-ASCII is a name character. The regexes still have to range over
 * codepoints rather than UTF-8 bytes: with byte-oriented matching, `¿`
 * (0xC2 0xBF) rejected every character encoded with either byte, and `\s`
 * rejected every character containing byte 0xA0. */

/* The complete ASCII operator alphabet. This list is a pin: it is exactly
 * what the old negated definition worked out to, and it must not grow or
 * shrink, so ASCII editing behaviour is unchanged. */
let ascii_operators = [
  "!",
  "%",
  "&",
  "*",
  "+",
  ",",
  "-",
  ".",
  "/",
  ":",
  ";",
  "<",
  "=",
  ">",
  "@",
  "\\",
  "|",
  "~",
];

/* The complete non-ASCII operator whitelist: the Drv judgment symbols that
 * Haz3lcore.ExpToSegment emits as INFIX tiles, alongside the ASCII `< > =`.
 * Occupying an operator position is the test, not merely being printed. */
let unicode_operators = [
  "\xe2\x88\x88", /* ∈ */
  "\xe2\x89\xa0", /* ≠ */
  "\xe2\x89\xae", /* ≮ */
  "\xe2\x89\xaf", /* ≯ */
  "\xe2\x8a\x86" /* ⊆ */
];

/* Characters that used to be operators only because they were left over
 * after the Unicode name categories were subtracted. They are names now. */
let former_operators = [
  "\xc2\xa2", /* ¢ -- lead byte 0xC2, same as ¿ */
  "\xc2\xa3", /* £ */
  "\xc2\xab", /* « */
  "\xc2\xb0", /* ° */
  "\xc2\xb1", /* ± */
  "\xc2\xac", /* ¬ */
  "\xc3\x97", /* × */
  "\xc2\xa9", /* © */
  "\xe2\x84\xa2", /* ™ */
  "\xe2\x9c\x93", /* ✓ */
  "\xe2\x9c\x94", /* ✔ -- and its near-twin agrees */
  "\xe2\x98\x85", /* ★ */
  "\xe2\x98\x80", /* ☀ */
  "\xe2\x86\x92", /* → */
  "\xe2\x80\xa6", /* … -- see the dedicated test below */
  "\xe2\x88\xa7", /* ∧, a Drv rule-name symbol -- label, never a token */
  "\xe2\x88\xa8", /* ∨ */
  "\xe2\x8a\x83", /* ⊃ */
  "\xe2\x8a\xa5", /* ⊥ */
  "\xe2\x8a\xa4" /* ⊤ */
];

let operator_tests = [
  test_case(
    "the ASCII operator alphabet is exactly these 18 characters",
    `Quick,
    () => {
      check(int, "count", 18, List.length(ascii_operators));
      List.iter(
        s => check(bool, s, true, Token.is_potential_operator(s)),
        ascii_operators,
      );
      /* Nothing else printable-ASCII is an operator, and the ASCII NAME
       * alphabet is likewise exactly what it was. Together these pin ASCII
       * editing behaviour as unchanged by the move to an explicit list. */
      let is_ascii_name = c =>
        c >= 'a'
        && c <= 'z'
        || c >= 'A'
        && c <= 'Z'
        || c >= '0'
        && c <= '9'
        || List.mem(c, ['_', '\'', '?', '^', '$']);
      for (c in 0x21 to 0x7e) {
        let s = String.make(1, Char.chr(c));
        check(
          bool,
          "operator " ++ s,
          List.mem(s, ascii_operators),
          Token.is_potential_operator(s),
        );
        check(
          bool,
          "name " ++ s,
          is_ascii_name(Char.chr(c)),
          Token.is_potential_operand(s),
        );
      };
      check(
        bool,
        "multi-character",
        true,
        Token.is_potential_operator("|>"),
      );
      check(bool, "++", true, Token.is_potential_operator("++"));
    },
  ),
  test_case(
    "the non-ASCII whitelist is operator, and it is the Drv vocabulary",
    `Quick,
    () => {
      /* These are the only Unicode characters the language emits as tokens
       * (Drv judgments, printed by ExpToSegment). Pinned in both directions:
       * dropping one would silently make it a name. */
      List.iter(
        s => check(bool, s, true, Token.is_potential_operator(s)),
        unicode_operators,
      );
      List.iter(
        s => check(bool, s, false, Token.is_potential_operand(s)),
        unicode_operators,
      );
    },
  ),
  test_case(
    "every other non-ASCII character is a name character",
    `Quick,
    () => {
      /* THE CHANGE: membership is no longer a Unicode-category question, so
       * symbols, pictographs, letters and marks all behave the same. The
       * near-twins ✔ and ✓ used to land on opposite sides. */
      let name = s => {
        check(bool, s, true, Token.is_potential_operand(s));
        check(bool, s, false, Token.is_potential_operator(s));
      };
      List.iter(name, former_operators);
      name("\xc3\xa9"); /* é */
      name("\xe6\x97\xa5"); /* 日 */
      name("\xf0\x9f\x98\x80"); /* 😀 */
      name("\xce\xbb"); /* λ */
      name("\xc5\xbf"); /* ſ -- trailing byte 0xBF, same as ¿ */
      name("\xc3\xa0"); /* à -- trailing byte 0xA0, which \s matches */
      name("\xcc\x81"); /* combining acute */
      name("\xe2\x80\x8d"); /* ZWJ, the emoji-sequence glue */
    },
  ),
  test_case(
    "the ellipsis is a name character, not an operator",
    `Quick,
    () => {
      /* Hazel prints … all over the place (Language.Abbreviate truncation
       * markers, probe-projector abbreviations), but being printed is not
       * the test: it never occupies an operator position, and it is a
       * placeholder OPERAND on another branch. Pinned so nobody adds it to
       * the whitelist on the grounds that ExpToSegment emits it. */
      let ellipsis = "\xe2\x80\xa6";
      check(
        bool,
        "not operator",
        false,
        Token.is_potential_operator(ellipsis),
      );
      check(bool, "is operand", true, Token.is_potential_operand(ellipsis));
      check(bool, "is var", true, Token.is_var(ellipsis));
      check(
        bool,
        "not an operator start",
        false,
        Token.begins_with_potential_operator(ellipsis),
      );
    },
  ),
  test_case(
    "the name and operator classes are disjoint",
    `Quick,
    () => {
      /* Mold resolution is ambiguous if a character is in both. Sweep all of
       * ASCII plus every non-ASCII character named anywhere in this file. */
      let both = s =>
        Token.is_potential_operand(s) && Token.is_potential_operator(s);
      for (c in 0 to 0x7f) {
        let s = String.make(1, Char.chr(c));
        check(bool, Printf.sprintf("ascii %#x", c), false, both(s));
      };
      List.iter(
        s => check(bool, s, false, both(s)),
        ascii_operators
        @ unicode_operators
        @ former_operators
        @ [
          Token.implicit_hole_marker,
          "\xc2\xa0",
          "\xc3\xa9",
          "\xe6\x97\xa5",
          "\xf0\x9f\x98\x80",
          "\xce\xbb",
          "\xcc\x81",
          "\xe2\x80\x8d",
        ],
      );
    },
  ),
  test_case(
    "excluded characters are in neither class",
    `Quick,
    () => {
      let neither = s => {
        check(bool, s, false, Token.is_potential_operator(s));
        check(bool, s, false, Token.is_potential_operand(s));
      };
      neither(Token.implicit_hole_marker); /* ¿ */
      neither("(");
      neither("\"");
      neither("#");
      neither("`");
      neither(" ");
      neither("\n");
      neither("\xc2\xa0"); /* NBSP is Unicode whitespace */
      neither("");
      let not_potential = s =>
        check(bool, s, false, Token.is_potential_operator(s));
      not_potential("+\xc2\xbf");
      not_potential("a");
      not_potential("+ ");
      not_potential("+\n");
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
        "begins_with on whitelisted operator",
        true,
        Token.begins_with_potential_operator("\xe2\x88\x88x"),
      );
      /* `^` prefixes livelits and projector invocations */
      check(
        bool,
        "begins_with on caret",
        true,
        Token.begins_with_potential_operator("^^fold"),
      );
      /* No longer an operator, so no longer an operator start */
      check(
        bool,
        "begins_with on a name character",
        false,
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

/* The name alphabet: every non-ASCII character except the operator whitelist
 * is a name character, and caseless characters count as non-uppercase, so a
 * name led by an emoji, a symbol or a CJK character is a variable rather than
 * a constructor. */
let cafe = "caf\xc3\xa9"; /* café */
let cafe_decomposed = "cafe\xcc\x81"; /* café, e + U+0301 */
let nihongo = "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e"; /* 日本語 */
let grin = "\xf0\x9f\x98\x80"; /* 😀 */
let family = "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x91\xa7"; /* 👨‍👩‍👧 */
let flag = "\xf0\x9f\x87\xba\xf0\x9f\x87\xb8"; /* 🇺🇸 */
let thumb_toned = "\xf0\x9f\x91\x8d\xf0\x9f\x8f\xbd"; /* 👍🏽 */

let name_tests = [
  test_case(
    "symbols inside a name do not split it",
    `Quick,
    () => {
      /* These characters used to be operators, so `a©b` lexed as three
       * tokens. They are ordinary name characters now. */
      let one_name = s => {
        check(bool, s, true, Token.is_potential_operand(s));
        check(bool, s, true, Token.is_var(s));
      };
      one_name("a\xc2\xa9b"); /* a©b */
      one_name("a\xe2\x9c\x93b"); /* a✓b */
      one_name("a\xe2\x9c\x94b"); /* a✔b */
      one_name("a\xe2\x98\x85b"); /* a★b */
      one_name("a\xe2\x80\xa6b"); /* a…b, the abbreviation marker */
      /* but a whitelisted operator still splits one */
      check(
        bool,
        "a∈b",
        false,
        Token.is_potential_operand("a\xe2\x88\x88b"),
      );
      /* leading symbols are non-uppercase, so still variables */
      List.iter(
        s => check(bool, s, true, Token.is_var(s ++ "x")),
        former_operators,
      );
      List.iter(
        s => check(bool, s, false, Token.is_ctr(s ++ "x")),
        former_operators,
      );
    },
  ),
  test_case(
    "Unicode names are single potential operands",
    `Quick,
    () => {
      let operand = s => check(bool, s, true, Token.is_potential_operand(s));
      operand(cafe);
      operand(cafe_decomposed);
      operand(nihongo);
      operand("x" ++ grin);
      operand(grin ++ "x");
      /* Multi-codepoint emoji: ZWJ sequences, flags and skin tones are only
       * one token if the joiners count as name characters too. */
      operand(family);
      operand(flag);
      operand(thumb_toned);
      operand("\xce\xa3igma"); /* Σigma */
    },
  ),
  test_case(
    "an operator next to a Unicode name is not part of it",
    `Quick,
    () => {
      let not_operand = s =>
        check(bool, s, false, Token.is_potential_operand(s));
      not_operand(cafe ++ "+1");
      not_operand(nihongo ++ "++x");
      not_operand(grin ++ "+");
      check(bool, "+", true, Token.is_potential_operator("+"));
      check(bool, "++", true, Token.is_potential_operator("++"));
    },
  ),
  test_case(
    "caseless characters count as non-uppercase",
    `Quick,
    () => {
      let var_ = s => check(bool, s, true, Token.is_var(s));
      let ctr = s => check(bool, s, true, Token.is_ctr(s));
      let not_var = s => check(bool, s, false, Token.is_var(s));
      let not_ctr = s => check(bool, s, false, Token.is_ctr(s));
      var_(cafe);
      var_(cafe_decomposed);
      var_(nihongo);
      var_(grin ++ "foo");
      var_("x" ++ grin);
      var_("\xcf\x83igma"); /* σigma */
      not_ctr(cafe);
      not_ctr(nihongo);
      not_ctr(grin ++ "foo");
      ctr("Caf\xc3\xa9"); /* Café */
      ctr("Foo" ++ grin);
      ctr("\xce\xa3igma"); /* Σigma */
      ctr("\xc7\x85ungla"); /* ǅungla, titlecase */
      not_var("Caf\xc3\xa9");
      not_var("Foo" ++ grin);
      /* Both are type variables either way */
      check(bool, "typ var lower", true, Token.is_typ_var(nihongo));
      check(bool, "typ var upper", true, Token.is_typ_var("Caf\xc3\xa9"));
    },
  ),
  test_case(
    "a decomposed accent stays inside the name",
    `Quick,
    () => {
      /* One name, and one grapheme fewer than its codepoints. */
      check(bool, "operand", true, Token.is_potential_operand("e\xcc\x81"));
      check(bool, "var", true, Token.is_var("e\xcc\x81"));
      check(int, "graphemes", 4, Token.length(cafe_decomposed));
      check(int, "bytes", 6, String.length(cafe_decomposed));
    },
  ),
  test_case(
    "livelit and label names take Unicode",
    `Quick,
    () => {
      check(bool, "livelit", true, Token.is_livelit("^" ++ cafe));
      check(bool, "livelit cjk", true, Token.is_livelit("^" ++ nihongo));
      check(bool, "livelit emoji", true, Token.is_livelit("^" ++ grin));
      /* Livelit names are variable-like, so a capitalized one is rejected */
      check(bool, "livelit ctr", false, Token.is_livelit("^Caf\xc3\xa9"));
      /* Labels that are already names print unquoted */
      check(
        string,
        "label unquoted",
        nihongo,
        Token.quote_label_when_necessary(nihongo),
      );
      check(
        string,
        "label quoted",
        Token.label_quote("a b"),
        Token.quote_label_when_necessary("a b"),
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

let invisible_tests = [
  test_case(
    "format and zero-width clusters are invisible",
    `Quick,
    () => {
      let invisible = (name, s) =>
        check(bool, name, true, Unicode.is_invisible_cluster(s));
      invisible("ZWSP", "\xe2\x80\x8b");
      invisible("ZWNJ", "\xe2\x80\x8c");
      invisible("ZWJ", "\xe2\x80\x8d");
      invisible("LRM", "\xe2\x80\x8e");
      invisible("RLO", "\xe2\x80\xae");
      invisible("LRI", "\xe2\x81\xa6");
      invisible("PDI", "\xe2\x81\xa9");
      invisible("word joiner", "\xe2\x81\xa0");
      invisible("soft hyphen", "\xc2\xad");
      invisible("NBSP", "\xc2\xa0");
      invisible("ALM", "\xd8\x9c");
      invisible("BOM", "\xef\xbb\xbf");
      invisible("ideographic space", "\xe3\x80\x80");
      invisible("lone VS16", "\xef\xb8\x8f");
      invisible("tag char", "\xf3\xa0\x81\xa1");
      /* GB9 joins ZWSP + ZWJ into one cluster; still all-invisible. */
      invisible("ZWSP+ZWJ cluster", "\xe2\x80\x8b\xe2\x80\x8d");
    },
  ),
  test_case(
    "visible and attached forms are not invisible",
    `Quick,
    () => {
      let visible = (name, s) =>
        check(bool, name, false, Unicode.is_invisible_cluster(s));
      visible("empty", "");
      visible("ascii", "a");
      visible("space", " ");
      visible("e-acute", "\xc3\xa9");
      visible("lone combining acute", "\xcc\x81");
      visible("cjk", "\xe6\x97\xa5");
      /* VS16 attached to a visible base leaves the cluster visible. */
      visible("heart + VS16", "\xe2\x9d\xa4\xef\xb8\x8f");
    },
  ),
  test_case(
    "invisible clusters get their own labeled segment",
    `Quick,
    () => {
      let seg_str = (seg: Web.GraphemeView.segment) =>
        switch (seg) {
        | Web.GraphemeView.Text(s) => "T:" ++ s
        | Web.GraphemeView.Grapheme(s, _) => "G:" ++ s
        | Web.GraphemeView.Invisible(s, _) => "I:" ++ s
        };
      let segs = s =>
        Web.GraphemeView.segments_for_token(s) |> List.map(seg_str);
      check(
        list(string),
        "ZWSP inside a name",
        ["T:a", "I:\xe2\x80\x8b", "T:b"],
        segs("a\xe2\x80\x8bb"),
      );
      check(
        list(string),
        "emoji cluster keeps its VS16",
        ["T:x", "G:\xe2\x9d\xa4\xef\xb8\x8f"],
        segs("x\xe2\x9d\xa4\xef\xb8\x8f"),
      );
    },
  ),
];

let nfc_tests = [
  test_case(
    "normalize_nfc composes decomposed accents",
    `Quick,
    () => {
      check(string, "ascii identity", "abc", Unicode.normalize_nfc("abc"));
      check(
        string,
        "combining acute composes",
        "caf\xc3\xa9",
        Unicode.normalize_nfc("cafe\xcc\x81"),
      );
      check(
        string,
        "already NFC is unchanged",
        "caf\xc3\xa9",
        Unicode.normalize_nfc("caf\xc3\xa9"),
      );
    },
  ),
  test_case(
    "nfc_outside_strings leaves literal contents intact",
    `Quick,
    () => {
      let go = Unicode.nfc_outside_strings;
      check(
        string,
        "identifier normalizes",
        "let caf\xc3\xa9 = 1",
        go("let cafe\xcc\x81 = 1"),
      );
      check(
        string,
        "string contents preserved",
        "x = \"e\xcc\x81\"",
        go("x = \"e\xcc\x81\""),
      );
      check(
        string,
        "code normalizes around a literal",
        "\xc3\xa9 ++ \"e\xcc\x81\" ++ \xc3\xa9",
        go("e\xcc\x81 ++ \"e\xcc\x81\" ++ e\xcc\x81"),
      );
      check(
        string,
        "unterminated literal runs to end of line",
        "\"e\xcc\x81\n\xc3\xa9",
        go("\"e\xcc\x81\ne\xcc\x81"),
      );
      check(
        string,
        "escaped quote stays inside the literal",
        {|"a\"e|} ++ "\xcc\x81\" x",
        go({|"a\"e|} ++ "\xcc\x81\" x"),
      );
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
  ("Unicode.Invisible", invisible_tests),
  ("Unicode.NFC", nfc_tests),
  ("Token.Potential", operator_tests),
  ("Token.Names", name_tests),
  ("Keyboard.Insert", keyboard_tests),
];
