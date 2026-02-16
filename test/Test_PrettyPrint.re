open Alcotest;
open Haz3lcore;

let exp_to_segment_settings: ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: false,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
};

let segmentize =
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings);

let format = (~width=80, ~settings=PrettySegment.default_settings, input: string): string => {
  switch (Parser.to_term(input)) {
  | Some(exp) =>
    let segment = segmentize(exp);
    let pretty = PrettySegment.prettify(~width, ~settings, segment);
    Printer.of_segment(~holes="?", ~indent="  ", pretty)
    |> Util.StringUtil.trim_trailing_whitespace;
  | None => failwith("Failed to parse: " ++ input)
  };
};

/* Segment-path formatter: preserves comments (like the CLI does).
   Uses Parser.to_segment instead of Parser.to_term. */
let format_seg = (~width=80, ~settings=PrettySegment.default_settings, input: string): string => {
  switch (Parser.to_segment(input)) {
  | Some(segment) =>
    let pretty = PrettySegment.prettify(~width, ~settings, segment);
    Printer.of_segment(~holes="?", ~indent=" ", pretty)
    |> Util.StringUtil.trim_trailing_whitespace;
  | None => failwith("Failed to parse: " ++ input)
  };
};

let test_format_seg = (~name, ~width=80, ~settings=PrettySegment.default_settings, ~input, ~expected, ()): test_case(_) =>
  test_case(name, `Quick, () =>
    check(string, name, expected, format_seg(~width, ~settings, input))
  );

let test_format = (~name, ~width=80, ~settings=PrettySegment.default_settings, ~input, ~expected, ()): test_case(_) =>
  test_case(name, `Quick, () =>
    check(string, name, expected, format(~width, ~settings, input))
  );

/* === Flat (fits on one line) === */

let flat_tests = [
  test_format(~name="Integer literal", ~input="42", ~expected="42", ()),
  test_format(
    ~name="Simple let",
    ~input="let x = 5 in x + 1",
    ~expected="let x = 5 in x + 1",
    (),
  ),
  test_format(
    ~name="If-then-else flat",
    ~input="if true then 1 else 2",
    ~expected="if true then 1 else 2",
    (),
  ),
  test_format(
    ~name="Function flat",
    ~input="fun x -> x + 1",
    ~expected="fun x -> x + 1",
    (),
  ),
  test_format(
    ~name="Tuple flat",
    ~input="(1, 2, 3)",
    ~expected="(1, 2, 3)",
    (),
  ),
  test_format(
    ~name="List flat",
    ~input="[1, 2, 3]",
    ~expected="[1, 2, 3]",
    (),
  ),
  test_format(~name="Tight application", ~input="f(5)", ~expected="f(5)", ()),
  test_format(
    ~name="Arithmetic flat",
    ~input="1 + 2 * 3",
    ~expected="1 + 2 * 3",
    (),
  ),
];

/* === Breaking (needs line breaks) === */

let breaking_tests = [
  test_format(
    ~name="Let breaks body",
    ~width=15,
    ~input="let x = 5 in x + 1",
    ~expected={|let x = 5 in
x + 1|},
    (),
  ),
  test_format(
    ~name="If breaks before keywords",
    ~width=15,
    ~input="if true then 1 else 2",
    ~expected={|if true
then 1
else 2|},
    (),
  ),
  test_format(
    ~name="Fun breaks body",
    ~width=15,
    ~input="fun x -> fun y -> x + y",
    ~expected={|fun x ->
    fun y -> x + y|},
    (),
  ),
  test_format(
    ~name="Nested lets break sequentially",
    ~width=25,
    ~input="let x = 1 in let y = 2 in let z = x + y in z * 2",
    ~expected={|let x = 1 in
let y = 2 in
let z = x + y in z * 2|},
    (),
  ),
  test_format(
    ~name="Let breaks binding when narrow",
    ~width=15,
    ~input="let x = 1 in let y = 2 in let z = x + y in z * 2",
    ~expected={|let x = 1 in
let y = 2 in
let z =
    x + y in
z * 2|},
    (),
  ),
  test_format(
    ~name="Infix all-or-nothing",
    ~width=10,
    ~input="1 + 2 + 3 + 4 + 5",
    ~expected={|1
+ 2
+ 3
+ 4
+ 5|},
    (),
  ),
];

/* === Delimiters (parens, brackets) === */

let delimiter_tests = [
  test_format(
    ~name="Tuple breaks vertically",
    ~width=10,
    ~input="(1, 2, 3, 4, 5)",
    ~expected={|(
    1,
    2,
    3,
    4,
    5
)|},
    (),
  ),
  test_format(
    ~name="List breaks vertically",
    ~width=10,
    ~input="[1, 2, 3, 4, 5]",
    ~expected={|[
    1,
    2,
    3,
    4,
    5
]|},
    (),
  ),
  test_format(
    ~name="Parens no inner spaces flat",
    ~input="(1)",
    ~expected="(1)",
    (),
  ),
  test_format(~name="Empty parens", ~input="()", ~expected="()", ()),
];

/* === Case expressions === */

let case_tests = [
  test_format(
    ~name="Case always breaks rules",
    ~input={|case x | 0 => "zero" | 1 => "one" end|},
    ~expected={|case x
| 0 => "zero"
| 1 => "one"
end|},
    (),
  ),
  test_format(
    ~name="Case breaks rules",
    ~width=25,
    ~input={|case x | 0 => "zero" | 1 => "one" | _ => "other" end|},
    ~expected={|case x
| 0 => "zero"
| 1 => "one"
| _ => "other"
end|},
    (),
  ),
];

/* === Combined / complex === */

let complex_tests = [
  test_format(
    ~name="Typed function",
    ~width=25,
    ~input="let f : Int -> Int = fun x -> x + 1 in f(5)",
    ~expected={|let f : (Int -> Int) =
    fun x -> x + 1 in
f(5)|},
    (),
  ),
  test_format(
    ~name="If with compound condition",
    ~width=20,
    ~input="if x > 0 then x + 1 else x - 1",
    ~expected={|if x > 0
then x + 1
else x - 1|},
    (),
  ),
  test_format(
    ~name="Nested function application",
    ~input="let f = fun x -> x + 1 in f(5)",
    ~expected="let f = fun x -> x + 1 in f(5)",
    (),
  ),
  test_format(
    ~name="Let chain with fun body",
    ~input="let x = 1 in let y = 2 in fun z -> x + y + z",
    ~expected={|let x = 1 in
let y = 2 in
fun z -> x + y + z|},
    (),
  ),
  test_format(
    ~name="Tight application at narrow width",
    ~width=10,
    ~input="let x = f(5) in x",
    ~expected={|let x =
    f(5) in
x|},
    (),
  ),
  test_format(
    ~name="Chained application stays tight",
    ~input="f(g(h(1)))",
    ~expected="f(g(h(1)))",
    (),
  ),
];

/* === Compound structures with commas === */

let comma_compound_tests = [
  test_format(
    ~name="Tuple of functions flat",
    ~input="(fun x -> x + 1, fun y -> y + 2)",
    ~expected="(fun x -> x + 1, fun y -> y + 2)",
    (),
  ),
  test_format(
    ~name="Tuple of functions breaks symmetrically",
    ~width=20,
    ~input="(fun x -> x + 1, fun y -> y + 2)",
    ~expected={|(
    fun x -> x + 1,
    fun y -> y + 2
)|},
    (),
  ),
  test_format(
    ~name="Three functions in tuple",
    ~width=25,
    ~input="(fun x -> x + 1, fun y -> y + 2, fun z -> z + 3)",
    ~expected=
      {|(
    fun x -> x + 1,
    fun y -> y + 2,
    fun z -> z + 3
)|},
    (),
  ),
  test_format(
    ~name="Let body with tuple hanging delimiters",
    ~width=15,
    ~input="let p = (1, 2, 3) in p",
    ~expected={|let p = (
    1,
    2,
    3
) in
p|},
    (),
  ),
  test_format(
    ~name="Fun in list breaks symmetrically",
    ~width=20,
    ~input="[fun x -> x, fun y -> y]",
    ~expected={|[
    fun x -> x,
    fun y -> y
]|},
    (),
  ),
];

/* === Real-world patterns (from emojipaint) === */

let realworld_tests = [
  /* Type alias chain: each should get its own line */
  test_format(
    ~name="Type alias chain",
    ~width=60,
    ~input="type A = Int in type B = String in type C = Bool in 1",
    ~expected={|type A = Int in
type B = String in
type C = Bool in 1|},
    (),
  ),
  /* Simple type alias stays flat */
  test_format(
    ~name="Type alias flat",
    ~input="type Emoji = String in 1",
    ~expected="type Emoji = String in 1",
    (),
  ),
  /* Fun with multiple params: params stay on one line
     (ExpToSegment parenthesizes multi-param funs) */
  test_format(
    ~name="Fun multi-param stays flat",
    ~width=40,
    ~input="let f = fun a, b, c -> a + b + c in f(1, 2, 3)",
    ~expected={|let f = fun (a, b, c) -> a + b + c in
f(1, 2, 3)|},
    (),
  ),
  /* Fun with long body: header stays flat, body breaks */
  test_format(
    ~name="Fun header flat with long body",
    ~width=30,
    ~input="let f = fun a, b, c -> a + b + c + 1 + 2 + 3 in 1",
    ~expected={|let f =
    fun (a, b, c) ->
        a + b + c + 1 + 2 + 3 in
1|},
    (),
  ),
  /* Fun with paren params and nested funs */
  test_format(
    ~name="Fun paren params flat",
    ~width=50,
    ~input="let f = fun (canvas, emoji) -> map(canvas, fun row -> map(row, fun x -> emoji)) in 1",
    ~expected={|let f =
    fun (canvas, emoji) ->
        map(canvas, fun row -> map(row, fun x -> emoji)) in
1|},
    (),
  ),
  /* Let with type annotation and fun body */
  test_format(
    ~name="Typed let with fun body",
    ~width=45,
    ~input="let f : Int -> Int = fun x -> x + 1 in f(5)",
    ~expected={|let f : (Int -> Int) = fun x -> x + 1 in
f(5)|},
    (),
  ),
  /* Case inside fun body */
  test_format(
    ~name="Fun with case body",
    ~width=40,
    ~input={|let f = fun m, action -> case action | 0 => m + 1 | 1 => m - 1 end in 1|},
    ~expected={|let f =
    fun (m, action) ->
        case action
        | 0 => m + 1
        | 1 => m - 1
        end in
1|},
    (),
  ),
  /* Nested fun in arguments: inner funs stay flat */
  test_format(
    ~name="Nested fun args stay flat",
    ~width=50,
    ~input="map(xs, fun x -> x + 1)",
    ~expected="map(xs, fun x -> x + 1)",
    (),
  ),
];

/* === Settings: hanging_delimiters === */

let no_hanging: PrettySegment.settings = {
  ...PrettySegment.default_settings,
  hanging_delimiters: false,
};

let hanging_delimiter_tests = [
  /* Default (true): opener stays on = line */
  test_format(
    ~name="Hanging on: tuple in let",
    ~width=15,
    ~input="let p = (1, 2, 3) in p",
    ~expected={|let p = (
    1,
    2,
    3
) in
p|},
    (),
  ),
  test_format(
    ~name="Hanging on: list in let",
    ~width=15,
    ~input="let p = [1, 2, 3] in p",
    ~expected={|let p = [
    1,
    2,
    3
] in
p|},
    (),
  ),
  /* Off: opener goes on its own indented line */
  test_format(
    ~name="Hanging off: tuple in let",
    ~settings=no_hanging,
    ~width=15,
    ~input="let p = (1, 2, 3) in p",
    ~expected={|let p =
    (1, 2, 3) in
p|},
    (),
  ),
  test_format(
    ~name="Hanging off: list in let",
    ~settings=no_hanging,
    ~width=15,
    ~input="let p = [1, 2, 3] in p",
    ~expected={|let p =
    [1, 2, 3] in
p|},
    (),
  ),
  /* When it fits on one line, both settings produce the same result */
  test_format(
    ~name="Hanging on: tuple fits flat",
    ~input="let p = (1, 2) in p",
    ~expected="let p = (1, 2) in p",
    (),
  ),
  test_format(
    ~name="Hanging off: tuple fits flat",
    ~settings=no_hanging,
    ~input="let p = (1, 2) in p",
    ~expected="let p = (1, 2) in p",
    (),
  ),
];

/* === Settings: break_fun_params === */

let break_params: PrettySegment.settings = {
  ...PrettySegment.default_settings,
  break_fun_params: true,
};

let break_fun_params_tests = [
  /* Default (false): fun header stays flat, body breaks independently */
  test_format(
    ~name="Params together: fun with long body",
    ~width=25,
    ~input="let f = fun a, b, c -> a + b + c + 1 + 2 in 1",
    ~expected={|let f =
    fun (a, b, c) ->
        a + b + c + 1 + 2 in
1|},
    (),
  ),
  /* With break_fun_params=true: params break vertically */
  test_format(
    ~name="Params break: fun with long body",
    ~settings=break_params,
    ~width=25,
    ~input="let f = fun a, b, c -> a + b + c + 1 + 2 in 1",
    ~expected={|let f =
    fun (
        a,
        b,
        c
    ) -> a + b + c + 1 + 2 in
1|},
    (),
  ),
  /* Both settings: flat when it fits */
  test_format(
    ~name="Params together: fits flat",
    ~input="fun x -> x + 1",
    ~expected="fun x -> x + 1",
    (),
  ),
  test_format(
    ~name="Params break: fits flat",
    ~settings=break_params,
    ~input="fun x -> x + 1",
    ~expected="fun x -> x + 1",
    (),
  ),
];

/* === Comments (segment-path tests, preserves comments) === */

let comment_tests = [
  /* Trailing comments stay on same line after comma */
  test_format_seg(
    ~name="Record trailing comments break",
    ~width=45,
    ~input="let x = (canvas = 1, # The 2D grid # brush = 2, # Selected emoji # palette = 3 # Available emojis #) in x",
    ~expected={|let x = (
  canvas = 1, # The 2D grid #
  brush = 2, # Selected emoji #
  palette = 3 # Available emojis #
) in
x|},
    (),
  ),
  /* When flat, comments stay inline */
  test_format_seg(
    ~name="Record trailing comments flat",
    ~width=80,
    ~input="let x = (a = 1, # field a # b = 2, # field b # c = 3) in x",
    ~expected="let x = (a = 1, # field a # b = 2, # field b # c = 3) in x",
    (),
  ),
  /* Simple trailing comment after comma */
  test_format_seg(
    ~name="Trailing comment after comma",
    ~width=30,
    ~input="let x = (a = 1, # hi # b = 2) in x",
    ~expected={|let x = (
  a = 1, # hi #
  b = 2
) in
x|},
    (),
  ),
  /* Standalone comment on its own line */
  test_format_seg(
    ~name="Standalone comment before let",
    ~width=25,
    ~input={|# standalone comment #
let x = 1 in x|},
    ~expected={|# standalone comment #
let x = 1 in x|},
    (),
  ),
];

let tests = [
  ("PrettyPrint.Flat", flat_tests),
  ("PrettyPrint.Breaking", breaking_tests),
  ("PrettyPrint.Delimiters", delimiter_tests),
  ("PrettyPrint.Case", case_tests),
  ("PrettyPrint.Complex", complex_tests),
  ("PrettyPrint.CommaCompound", comma_compound_tests),
  ("PrettyPrint.Realworld", realworld_tests),
  ("PrettyPrint.HangingDelimiters", hanging_delimiter_tests),
  ("PrettyPrint.BreakFunParams", break_fun_params_tests),
  ("PrettyPrint.Comments", comment_tests),
];
