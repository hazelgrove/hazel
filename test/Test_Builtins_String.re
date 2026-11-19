open Alcotest;
open Language;
open IdTagged.FreshGrammar;

/* The string builtins index by grapheme cluster. These tests pin that unit
   down, and in particular that string_search's result is in the same unit
   string_sub consumes. */

let imp_of = (name: string): (DHExp.t => option(DHExp.t)) => {
  let named = (f: BuiltinsUtil.fn) => f.name == name;
  switch (List.find_opt(named, BuiltinsBase.string_fns)) {
  | Some(f) => f.imp
  | None => failwith("unknown string builtin: " ++ name)
  };
};

let call1 = (name: string, arg: string) => imp_of(name, Exp.string(arg));
let call = (name: string, args) => imp_of(name, Exp.tuple(args));

let as_string = (d: option(DHExp.t)): string =>
  switch (d) {
  | Some(d) =>
    switch (DHExp.term_of(d)) {
    | Atom(String(s)) => s
    | _ => failwith("expected a String result, got " ++ DHExp.show(d))
    }
  | None => failwith("builtin did not reduce")
  };

let as_int = (d: option(DHExp.t)): int =>
  switch (d) {
  | Some(d) =>
    switch (DHExp.term_of(d)) {
    | Atom(Int(i)) => Bigint.to_int(i) |> Option.get
    | _ => failwith("expected an Int result, got " ++ DHExp.show(d))
    }
  | None => failwith("builtin did not reduce")
  };

let as_bool = (d: option(DHExp.t)): bool =>
  switch (d) {
  | Some(d) =>
    switch (DHExp.term_of(d)) {
    | Atom(Bool(b)) => b
    | _ => failwith("expected a Bool result, got " ++ DHExp.show(d))
    }
  | None => failwith("builtin did not reduce")
  };

let as_strings = (d: option(DHExp.t)): list(string) =>
  switch (d) {
  | Some(d) =>
    switch (DHExp.term_of(d)) {
    | ListLit(ds) => List.map(d => as_string(Some(d)), ds)
    | _ => failwith("expected a list result, got " ++ DHExp.show(d))
    }
  | None => failwith("builtin did not reduce")
  };

let is_index_error = (d: option(DHExp.t)): bool =>
  switch (d) {
  | Some(d) =>
    switch (DHExp.term_of(d)) {
    | DynamicErrorHole(_, IndexOutOfBounds) => true
    | _ => false
    }
  | None => false
  };

let grin = "\xF0\x9F\x98\x80"; /* U+1F600 */
let family = "\xF0\x9F\x91\xA8\xE2\x80\x8D\xF0\x9F\x91\xA9\xE2\x80\x8D\xF0\x9F\x91\xA7\xE2\x80\x8D\xF0\x9F\x91\xA6"; /* ZWJ family */
let e_combining = "e\xCC\x81"; /* e + U+0301 */
let e_acute = "\xC3\xA9"; /* U+00E9 */
let nbsp = "\xC2\xA0"; /* U+00A0 */
let middot = "\xC2\xB7"; /* U+00B7 */

let length_tests = [
  test_case(
    "string_length counts graphemes, not bytes",
    `Quick,
    () => {
      check(int, "ascii", 5, as_int(call1("string_length", "hello")));
      check(int, "emoji", 1, as_int(call1("string_length", grin)));
      check(
        int,
        "combining mark",
        1,
        as_int(call1("string_length", e_combining)),
      );
      check(int, "zwj family", 1, as_int(call1("string_length", family)));
      check(
        int,
        "mixed",
        5,
        as_int(call1("string_length", "h" ++ e_acute ++ "llo")),
      );
    },
  ),
];

let sub_tests = {
  let sub = (s, i, n) =>
    call("string_sub", [Exp.string(s), Exp.int(i), Exp.int(n)]);
  [
    test_case(
      "string_sub slices whole clusters",
      `Quick,
      () => {
        let s = grin ++ "abc";
        check(string, "leading emoji", grin, as_string(sub(s, 0, 1)));
        check(
          int,
          "emoji slice is intact utf-8",
          4,
          String.length(as_string(sub(s, 0, 1))),
        );
        check(string, "after the emoji", "ab", as_string(sub(s, 1, 2)));
        check(string, "whole string", s, as_string(sub(s, 0, 4)));
        check(
          string,
          "zwj family is one cluster",
          family,
          as_string(sub("a" ++ family ++ "b", 1, 1)),
        );
      },
    ),
    test_case(
      "string_sub out of range gives an error hole",
      `Quick,
      () => {
        check(
          bool,
          "len past end",
          true,
          is_index_error(sub(grin ++ "abc", 0, 5)),
        );
        check(
          bool,
          "negative index",
          true,
          is_index_error(sub("abc", -1, 1)),
        );
        check(
          bool,
          "negative length",
          true,
          is_index_error(sub("abc", 1, -1)),
        );
        check(
          bool,
          "index past end",
          true,
          is_index_error(sub("abc", 4, 0)),
        );
      },
    ),
  ];
};

let search_tests = {
  let search = (re, s, i) =>
    call("string_search", [Exp.string(re), Exp.string(s), Exp.int(i)]);
  let sub = (s, i, n) =>
    call("string_sub", [Exp.string(s), Exp.int(i), Exp.int(n)]);
  [
    test_case(
      "string_search returns a grapheme index",
      `Quick,
      () => {
        check(int, "ascii", 0, as_int(search("h.+z", "hazel", 0)));
        check(int, "not found", -1, as_int(search("foo", "hazel", 0)));
        check(
          int,
          "after an emoji",
          2,
          as_int(search("hazel", grin ++ " hazel", 0)),
        );
        check(
          int,
          "start index is in graphemes too",
          3,
          as_int(search("a", grin ++ "a" ++ grin ++ "a", 2)),
        );
      },
    ),
    test_case(
      "string_sub(s, string_search(...), n) composes",
      `Quick,
      () => {
        let s = grin ++ " hazel";
        let i = as_int(search("hazel", s, 0));
        check(string, "extracted match", "hazel", as_string(sub(s, i, 5)));
      },
    ),
  ];
};

let split_tests = {
  let split = (sep, s) =>
    call("string_split", [Exp.string(sep), Exp.string(s)]);
  [
    test_case(
      "string_split on a multi-byte separator",
      `Quick,
      () => {
        check(
          list(string),
          "U+00B7",
          ["a", "b", "c"],
          as_strings(split(middot, "a" ++ middot ++ "b" ++ middot ++ "c")),
        );
        check(
          list(string),
          "emoji separator",
          ["a", "b"],
          as_strings(split(grin, "a" ++ grin ++ "b")),
        );
      },
    ),
    test_case("string_split on an empty separator gives graphemes", `Quick, () =>
      check(
        list(string),
        "clusters",
        ["a", grin],
        as_strings(split("", "a" ++ grin)),
      )
    ),
    test_case(
      "string_split still handles ascii and metacharacters",
      `Quick,
      () => {
        check(
          list(string),
          "ascii",
          ["a", "c"],
          as_strings(split("b", "abc")),
        );
        check(
          list(string),
          "separator is literal",
          ["a", "c"],
          as_strings(split(".*", "a.*c")),
        );
      },
    ),
  ];
};

let case_tests = [
  test_case(
    "case mapping covers non-ascii",
    `Quick,
    () => {
      check(
        string,
        "ascii up",
        "HELLO",
        as_string(call1("string_uppercase", "hello")),
      );
      check(
        string,
        "ascii down",
        "hello",
        as_string(call1("string_lowercase", "HELLO")),
      );
      check(
        string,
        "latin-1 up",
        "H\xC3\x89LLO",
        as_string(call1("string_uppercase", "h" ++ e_acute ++ "llo")),
      );
      check(
        string,
        "greek down",
        "\xCE\xB1\xCE\xB2\xCE\xB3",
        as_string(call1("string_lowercase", "\xCE\x91\xCE\x92\xCE\x93")),
      );
      check(
        string,
        "cyrillic up",
        "\xD0\x9F\xD0\xA0\xD0\x98\xD0\x92\xD0\x95\xD0\xA2",
        as_string(
          call1(
            "string_uppercase",
            "\xD0\xBF\xD1\x80\xD0\xB8\xD0\xB2\xD0\xB5\xD1\x82",
          ),
        ),
      );
    },
  ),
  test_case(
    "capitalize works on the first grapheme",
    `Quick,
    () => {
      check(
        string,
        "ascii",
        "Hello",
        as_string(call1("string_capitalize", "hello")),
      );
      check(
        string,
        "ascii down",
        "hello",
        as_string(call1("string_uncapitalize", "Hello")),
      );
      check(
        string,
        "precomposed",
        "\xC3\x89tude",
        as_string(call1("string_capitalize", e_acute ++ "tude")),
      );
      check(
        string,
        "decomposed cluster stays one cluster",
        "E\xCC\x81tude",
        as_string(call1("string_capitalize", e_combining ++ "tude")),
      );
      check(
        string,
        "uncapitalize",
        e_acute ++ "tude",
        as_string(call1("string_uncapitalize", "\xC3\x89tude")),
      );
      check(string, "empty", "", as_string(call1("string_capitalize", "")));
      check(
        string,
        "leading emoji is untouched",
        grin ++ "ok",
        as_string(call1("string_capitalize", grin ++ "ok")),
      );
    },
  ),
];

let regex_tests = {
  let matches = (re, s) =>
    call("string_match", [Exp.string(re), Exp.string(s)]);
  let replace = (re, s, by) =>
    call(
      "string_replace",
      [Exp.string(re), Exp.string(s), Exp.string(by)],
    );
  [
    test_case(
      "string_match sees code points, not bytes",
      `Quick,
      () => {
        check(bool, "ascii true", true, as_bool(matches("hazel", "hazel")));
        check(
          bool,
          "ascii false",
          false,
          as_bool(matches("hazel", "world")),
        );
        check(
          bool,
          "dot spans a 2-byte char",
          true,
          as_bool(matches("^.$", e_acute)),
        );
        check(
          bool,
          "dot spans an astral char",
          true,
          as_bool(matches("^.$", grin)),
        );
      },
    ),
    test_case(
      "string_replace keeps utf-8 well-formed",
      `Quick,
      () => {
        check(
          string,
          "ascii",
          "worldworld",
          as_string(replace("ha*zel", "hazelhzel", "world")),
        );
        check(
          string,
          "replacing a 2-byte char",
          "Xtude",
          as_string(replace("^.", e_acute ++ "tude", "X")),
        );
        check(
          string,
          "replacement is literal",
          "$&b",
          as_string(replace("a", "ab", "$&")),
        );
      },
    ),
  ];
};

let trim_tests = [
  test_case(
    "string_trim removes unicode whitespace",
    `Quick,
    () => {
      check(
        string,
        "ascii",
        "hi",
        as_string(call1("string_trim", "  hi\n")),
      );
      check(
        string,
        "nbsp",
        "hi",
        as_string(call1("string_trim", nbsp ++ " hi " ++ nbsp)),
      );
      check(
        string,
        "interior is kept",
        "a b",
        as_string(call1("string_trim", " a b ")),
      );
    },
  ),
];

let escape_tests = {
  let escaped = s => as_string(call1("string_escaped", s));
  let unescaped = s => as_string(call1("string_unescaped", s));
  [
    test_case(
      "string_escaped leaves non-ascii readable",
      `Quick,
      () => {
        check(string, "backslash", "\\\\hello", escaped("\\hello"));
        check(string, "newline", "a\\nb", escaped("a\nb"));
        check(string, "quote", "a\\\"b", escaped("a\"b"));
        check(
          string,
          "accented",
          "h" ++ e_acute ++ "llo",
          escaped("h" ++ e_acute ++ "llo"),
        );
        check(string, "emoji", grin, escaped(grin));
      },
    ),
    test_case(
      "escaped/unescaped round trip",
      `Quick,
      () => {
        let roundtrip = s =>
          check(string, "roundtrip", s, unescaped(escaped(s)));
        roundtrip("hello");
        roundtrip("a\"b\\c\nd\te");
        roundtrip("\x00\x01\x1F\x7F");
        roundtrip("h" ++ e_acute ++ "llo " ++ grin);
        roundtrip(family);
        roundtrip(e_combining);
      },
    ),
  ];
};

let tests = (
  "Builtins.String",
  length_tests
  @ sub_tests
  @ search_tests
  @ split_tests
  @ case_tests
  @ regex_tests
  @ trim_tests
  @ escape_tests,
);
