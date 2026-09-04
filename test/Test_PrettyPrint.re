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
  hole_tiles: false,
  project_tables: false,
};

let segmentize =
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings);

let format =
    (~width=80, ~settings=PrettySegment.default_settings, input: string)
    : string => {
  switch (Parser.to_term(input, ~root=Exp)) {
  | Some(exp) =>
    let segment = segmentize(exp);
    let pretty = PrettySegment.prettify(~width, ~settings, segment);
    Printer.of_segment(~holes="?", ~indent="  ", pretty)
    |> Util_web.StringUtil.trim_trailing_whitespace;
  | None => failwith("Failed to parse: " ++ input)
  };
};

/* Segment-path formatter: preserves comments (like the CLI does).
   Uses Parser.to_segment instead of Parser.to_term. */
let format_seg =
    (~width=80, ~settings=PrettySegment.default_settings, input: string)
    : string => {
  switch (Parser.to_segment(input, ~root=Exp)) {
  | Some(segment) =>
    let pretty = PrettySegment.prettify(~width, ~settings, segment);
    Printer.of_segment(~holes="?", ~indent=" ", pretty)
    |> Util_web.StringUtil.trim_trailing_whitespace;
  | None => failwith("Failed to parse: " ++ input)
  };
};

let test_format_seg =
    (
      ~name,
      ~width=80,
      ~settings=PrettySegment.default_settings,
      ~input,
      ~expected,
      (),
    )
    : test_case(_) =>
  test_case(name, `Quick, () =>
    check(string, name, expected, format_seg(~width, ~settings, input))
  );

let test_format =
    (
      ~name,
      ~width=80,
      ~settings=PrettySegment.default_settings,
      ~input,
      ~expected,
      (),
    )
    : test_case(_) =>
  test_case(name, `Quick, () =>
    check(string, name, expected, format(~width, ~settings, input))
  );

/* === Flat (fits on one line) === */

let flat_tests = [
  test_format(~name="Integer literal", ~input="42", ~expected="42", ()),
  test_format(
    ~name="Simple let",
    ~input="let x = 5 in x + 1",
    ~expected={|let x = 5 in
x + 1|},
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
let z = x + y in
z * 2|},
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
    ~expected={|let f = fun x -> x + 1 in
f(5)|},
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
type C = Bool in
1|},
    (),
  ),
  /* Type alias with final body: body lands on its own line */
  test_format(
    ~name="Type alias body on new line",
    ~input="type Emoji = String in 1",
    ~expected={|type Emoji = String in
1|},
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
    ~expected=
      {|let f =
    fun (a, b, c) ->
        a + b + c + 1 + 2 + 3 in
1|},
    (),
  ),
  /* Fun with paren params and nested funs */
  test_format(
    ~name="Fun paren params flat",
    ~width=50,
    ~input=
      "let f = fun (canvas, emoji) -> map(canvas, fun row -> map(row, fun x -> emoji)) in 1",
    ~expected=
      {|let f =
    fun (canvas, emoji) ->
        map(
            canvas,
            fun row -> map(row, fun x -> emoji)
        ) in
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
    ~input=
      {|let f = fun m, action -> case action | 0 => m + 1 | 1 => m - 1 end in 1|},
    ~expected=
      {|let f =
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
  /* Binding and header stay flat when they fit, but the body still
     lands on its own line (trailing `in` rule) */
  test_format(
    ~name="Hanging on: tuple fits flat",
    ~input="let p = (1, 2) in p",
    ~expected={|let p = (1, 2) in
p|},
    (),
  ),
  test_format(
    ~name="Hanging off: tuple fits flat",
    ~settings=no_hanging,
    ~input="let p = (1, 2) in p",
    ~expected={|let p = (1, 2) in
p|},
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
    ~expected=
      {|let f =
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
    ~input=
      "let x = (canvas = 1, # The 2D grid # brush = 2, # Selected emoji # palette = 3 # Available emojis #) in x",
    ~expected=
      {|let x = (
  canvas = 1, # The 2D grid #
  brush = 2, # Selected emoji #
  palette = 3 # Available emojis #
) in
x|},
    (),
  ),
  /* When flat, record stays flat but body lands on new line */
  test_format_seg(
    ~name="Record trailing comments flat",
    ~width=80,
    ~input="let x = (a = 1, # field a # b = 2, # field b # c = 3) in x",
    ~expected={|let x = (a = 1, # field a # b = 2, # field b # c = 3) in
x|},
    (),
  ),
  /* Simple trailing comment after comma */
  test_format_seg(
    ~name="Trailing comment after comma",
    ~width=35,
    ~input="let x = (a = 1, # hi # b = 2) in x",
    ~expected={|let x = (a = 1, # hi # b = 2) in
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
let x = 1 in
x|},
    (),
  ),
  /* Sum type constructors with trailing comments:
     infix + stays attached to following constructor */
  test_format_seg(
    ~name="Sum type + with trailing comments",
    ~width=40,
    ~input=
      "type T = + A # a long comment here # + B # another long comment # + C # third # in 1",
    ~expected=
      {|type T =
  + A # a long comment here #
  + B # another long comment #
  + C # third # in
1|},
    (),
  ),
];

/* === Form handling: arrow forms, test/end, hint/test/end === */

let form_tests = [
  /* typfun/-> keeps header on one line (previously fell through to generic) */
  test_format_seg(
    ~name="Typfun arrow stays on one line",
    ~width=20,
    ~input="typfun a -> fun x -> x + 1",
    ~expected={|typfun a ->
  fun x -> x + 1|},
    (),
  ),
  /* poly/-> in type context keeps header on one line */
  test_format_seg(
    ~name="Poly arrow in type annotation",
    ~width=30,
    ~input="let f : poly a -> a -> a = typfun a -> fun x : a -> x in 1",
    ~expected={|let f : poly a -> a -> a =
  typfun a -> fun x : a -> x in
1|},
    (),
  ),
  /* test/end with semicolons: test on own line, end trails body */
  test_format_seg(
    ~name="Test/end with semicolons consistent",
    ~width=40,
    ~input="test 1 + 1 == 2 end; test 3 + 3 == 6 end; 5",
    ~expected={|test
  1 + 1 == 2 end;
test
  3 + 3 == 6 end;
5|},
    (),
  ),
  /* Nested typfun waterfall (matches basic.hz polymorphic section) */
  test_format_seg(
    ~name="Nested typfun waterfall",
    ~width=30,
    ~input="let f = typfun a -> typfun b -> fun x -> x in 1",
    ~expected={|let f =
  typfun a ->
    typfun b -> fun x -> x in
1|},
    (),
  ),
  /* poly/-> in breaking type annotation keeps arrow on same line */
  test_format_seg(
    ~name="Poly arrow breaks in type annotation",
    ~width=30,
    ~input=
      "let f : poly a -> poly b -> (a, b) -> (a, b) = typfun a -> typfun b -> fun x -> x in f",
    ~expected=
      {|let f
: poly a ->
  poly b -> (a, b) -> (a, b) =
  typfun a ->
    typfun b -> fun x -> x in
f|},
    (),
  ),
  /* Hanging delimiter in fun body (paren stays on -> line) */
  test_format_seg(
    ~name="Fun hanging delimiter record body",
    ~width=40,
    ~input=
      "let closeDay = fun ledger -> (harvests = 1, totalValue = 2, streakBonus = 0, lastQuality = 3) in 1",
    ~expected=
      {|let closeDay =
  fun ledger -> (
    harvests = 1,
    totalValue = 2,
    streakBonus = 0,
    lastQuality = 3
  ) in
1|},
    (),
  ),
  /* hint/test/end: hint on first line, test on own line, end trails */
  test_format_seg(
    ~name="Hint/test/end formatting",
    ~width=40,
    ~input={|hint "msg" test 1 + 1 == 2 end; 5|},
    ~expected={|hint "msg"
test
  1 + 1 == 2 end;
5|},
    (),
  ),
  /* use/in: body lands on its own line */
  test_format_seg(
    ~name="Use/in body on new line",
    ~input="use Int in 5",
    ~expected={|use Int in
5|},
    (),
  ),
  /* use/in: chains with let like binding forms */
  test_format_seg(
    ~name="Use/in chains with let",
    ~width=30,
    ~input="use Int in let x = 1 in x",
    ~expected={|use Int in
let x = 1 in
x|},
    (),
  ),
  /* use/in: multiple use forms chain */
  test_format_seg(
    ~name="Use/in chaining",
    ~width=30,
    ~input="use Int in use String in let x = 1 in x",
    ~expected={|use Int in
use String in
let x = 1 in
x|},
    (),
  ),
  /* hide/in: chains with let */
  test_format_seg(
    ~name="Hide/in chains with let",
    ~width=40,
    ~input="hide 1 + 2 in let x = 1 in x",
    ~expected={|hide 1 + 2 in
let x = 1 in
x|},
    (),
  ),
  /* Type application @<>: tight delimiters, no internal spacing.
     Body still breaks onto its own line after the trailing `in`. */
  test_format_seg(
    ~name="Type application tight",
    ~input="let f = typfun a -> fun x : a -> x in f @< Int >(5)",
    ~expected={|let f = typfun a -> fun x : a -> x in
f@<Int>(5)|},
    (),
  ),
  /* Type application breaking */
  test_format_seg(
    ~name="Type application breaking",
    ~width=40,
    ~input=
      "let f = typfun a -> fun x : a -> x in f @< Int >(5) + f @< String >(\"hello\")",
    ~expected=
      {|let f = typfun a -> fun x : a -> x in
f@<Int>(5) + f@<String>("hello")|},
    (),
  ),
];

/* === Composition and regression tests === */

let composition_tests = [
  /* Nested case/end: case rule body contains another case expression */
  test_format_seg(
    ~name="Nested case/end",
    ~width=30,
    ~input=
      {|case x | 0 => case y | 0 => "both zero" | _ => "x zero" end | _ => "other" end|},
    ~expected=
      {|case x
| 0 =>
  case y
  | 0 => "both zero"
  | _ => "x zero"
  end
| _ => "other"
end|},
    (),
  ),
  /* Case rule with body that wraps to next line */
  test_format_seg(
    ~name="Case rule long body wraps",
    ~width=35,
    ~input=
      {|case x | 0 => longfunction(a, b) + anotherfunc(c) | 1 => short end|},
    ~expected=
      {|case x
| 0 =>
  longfunction(a, b) + anotherfunc(c)
| 1 => short
end|},
    (),
  ),
  /* Block expression {} flat; body lands on its own line */
  test_format_seg(
    ~name="Block flat",
    ~input="let x = { 1 + 2 } in x",
    ~expected={|let x = {1 + 2} in
x|},
    (),
  ),
  /* Block expression {} breaking */
  test_format_seg(
    ~name="Block breaking",
    ~width=20,
    ~input="let x = { 1 + 2 + 3 + 4 + 5 } in x",
    ~expected={|let x =
  {
    1
    + 2
    + 3
    + 4
    + 5
  } in
x|},
    (),
  ),
  /* Blank line preservation between let chains.
     Both blank lines from the input are preserved. */
  test_format_seg(
    ~name="Blank lines preserved",
    ~width=40,
    ~input={|let a = 1 in

let b = 2 in

a + b|},
    ~expected={|let a = 1 in

let b = 2 in

a + b|},
    (),
  ),
  /* Long let chain: regression test for O(2^N) blowup */
  test_format_seg(
    ~name="Long let chain (20 bindings)",
    ~width=40,
    ~input=
      "let a = 1 in let b = 2 in let c = 3 in let d = 4 in let e = 5 in let f = 6 in let g = 7 in let h = 8 in let i = 9 in let j = 10 in let k = 11 in let l = 12 in let m = 13 in let n = 14 in let o = 15 in let p = 16 in let q = 17 in let r = 18 in let s = 19 in let t = 20 in t",
    ~expected=
      {|let a = 1 in
let b = 2 in
let c = 3 in
let d = 4 in
let e = 5 in
let f = 6 in
let g = 7 in
let h = 8 in
let i = 9 in
let j = 10 in
let k = 11 in
let l = 12 in
let m = 13 in
let n = 14 in
let o = 15 in
let p = 16 in
let q = 17 in
let r = 18 in
let s = 19 in
let t = 20 in
t|},
    (),
  ),
];

/* === Labeled tuples === */

let labeled_tuple_tests = [
  test_format(
    ~name="Labeled tuple flat",
    ~input="(a= 1, b = 2)",
    ~expected="(a = 1, b = 2)",
    (),
  ),
  test_format(
    ~name="Labeled tuple breaks vertically",
    ~width=20,
    ~input="(firsts = [1, 2], seconds = [3, 4])",
    ~expected={|(
    firsts = [1, 2],
    seconds = [3, 4]
)|},
    (),
  ),
  test_format(
    ~name="Labeled tuple single entry",
    ~input="(a= 1)",
    ~expected="(a = 1)",
    (),
  ),
];

/* === Trailing `in` body ===
   The expression following a trailing `in` (let/in, type/in, filter/in)
   always lands on its own line, even when it would fit flat. Trailing
   holes (implicit empty grouts, explicit `?`) get the same treatment in
   non-binding positions too (e.g., `fun x -> ?`). */

let trailing_hole_tests = [
  /* Exact example from the user request */
  test_format(
    ~name="Let chain with arithmetic body",
    ~input="let x = 1 in let y = 2 in x + y",
    ~expected={|let x = 1 in
let y = 2 in
x + y|},
    (),
  ),
  /* Let chain with implicit trailing hole ends on its own line */
  test_format(
    ~name="Let chain implicit trailing hole",
    ~input="let x = 1 in let x = 2 in ",
    ~expected={|let x = 1 in
let x = 2 in
?|},
    (),
  ),
  /* Let chain with explicit trailing hole ends on its own line */
  test_format_seg(
    ~name="Let chain explicit trailing hole",
    ~input="let x = 1 in let x = 2 in ?",
    ~expected={|let x = 1 in
let x = 2 in
?|},
    (),
  ),
  /* Single let with explicit trailing hole breaks to new line */
  test_format_seg(
    ~name="Single let explicit trailing hole",
    ~input="let x = 1 in ?",
    ~expected={|let x = 1 in
?|},
    (),
  ),
  /* Single let with implicit trailing hole breaks to new line */
  test_format(
    ~name="Single let implicit trailing hole",
    ~input="let x = 1 in ",
    ~expected={|let x = 1 in
?|},
    (),
  ),
  /* Non-trailing holes (inside an expression) stay inline within the
     body — only the body itself lands on a new line after `in`. */
  test_format_seg(
    ~name="Non-trailing hole stays inline",
    ~input="let x = 1 in ? + 1",
    ~expected={|let x = 1 in
? + 1|},
    (),
  ),
  /* Hole inside binding (not trailing) stays inline in the binding */
  test_format_seg(
    ~name="Hole in binding position stays inline",
    ~input="let x = ? in 1",
    ~expected={|let x = ? in
1|},
    (),
  ),
  /* Fun with trailing explicit hole body breaks to new line
     (body is indented under the arrow) */
  test_format_seg(
    ~name="Fun explicit trailing hole",
    ~input="fun x -> ?",
    ~expected={|fun x ->
  ?|},
    (),
  ),
  /* Typfun with trailing explicit hole body breaks to new line
     (body is indented under the arrow) */
  test_format_seg(
    ~name="Typfun explicit trailing hole",
    ~input="typfun a -> ?",
    ~expected={|typfun a ->
  ?|},
    (),
  ),
  /* Filter form (use/in) with trailing explicit hole */
  test_format_seg(
    ~name="Use/in explicit trailing hole",
    ~input="use Int in ?",
    ~expected={|use Int in
?|},
    (),
  ),
  /* Mixed chain: let then fun, both trailing.
     The hole is indented under `fun`'s arrow. */
  test_format_seg(
    ~name="Let then fun explicit trailing hole",
    ~input="let x = 1 in fun y -> ?",
    ~expected={|let x = 1 in
fun y ->
  ?|},
    (),
  ),
];

/* === Block forms on their own line ===
   Block-like expressions (let/case/fun/if/...) always land on a new line
   when they appear as the body of another block form. This is the natural
   extension of the "trailing `in` body" rule to all contexts where a block
   form would otherwise share a line with a keyword like `then`. */

let block_form_body_tests = [
  /* Exact example from the user's request */
  test_format_seg(
    ~name="Case rule with let body (user example)",
    ~input="case xs | [] => [] | hd::tl => let x = 1 in x + y end",
    ~expected={|case xs
| [] => []
| hd::tl =>
  let x = 1 in
  x + y
end|},
    (),
  ),
  /* if/then with a let conseq: block-form body on its own line after `then` */
  test_format_seg(
    ~name="If then let conseq",
    ~input="if true then let x = 1 in x else 0",
    ~expected={|if true
then
  let x = 1 in
  x
else 0|},
    (),
  ),
  /* if/then with a multi-rule case conseq */
  test_format_seg(
    ~name="If then case conseq",
    ~input="if true then case x | 0 => 1 | _ => 2 end else 0",
    ~expected={|if true
then
  case x
  | 0 => 1
  | _ => 2
  end
else 0|},
    (),
  ),
  /* if/then with a short single-rule case conseq still breaks */
  test_format_seg(
    ~name="If then short case conseq",
    ~input="if true then case x | 0 => 1 end else 0",
    ~expected={|if true
then
  case x
  | 0 => 1
  end
else 0|},
    (),
  ),
  /* if/else with a let else branch */
  test_format_seg(
    ~name="If else let branch",
    ~input="if true then 0 else let x = 1 in x",
    ~expected={|if true
then 0
else
  let x = 1 in
  x|},
    (),
  ),
  /* if/else with a case else branch */
  test_format_seg(
    ~name="If else case branch",
    ~input="if true then 0 else case x | 0 => 1 | _ => 2 end",
    ~expected={|if true
then 0
else
  case x
  | 0 => 1
  | _ => 2
  end|},
    (),
  ),
  /* if with block forms in both branches */
  test_format_seg(
    ~name="If with block forms both branches",
    ~input=
      "if true then case x | 0 => 1 end else case y | 0 => 1 | _ => 2 end",
    ~expected=
      {|if true
then
  case x
  | 0 => 1
  end
else
  case y
  | 0 => 1
  | _ => 2
  end|},
    (),
  ),
  /* Non-block conseq stays on same line as `then` */
  test_format(
    ~name="If with simple conseq stays flat",
    ~input="if true then 1 else 2",
    ~expected="if true then 1 else 2",
    (),
  ),
  /* When width forces a break and conseq isn't block-like,
     `then <simple>` stays together */
  test_format(
    ~name="If narrow with simple branches",
    ~width=15,
    ~input="if true then 1 else 2",
    ~expected={|if true
then 1
else 2|},
    (),
  ),
  /* Case rule with a case body */
  test_format_seg(
    ~name="Case rule with case body",
    ~input="case x | 0 => case y | 0 => 1 | _ => 2 end | _ => 0 end",
    ~expected=
      {|case x
| 0 =>
  case y
  | 0 => 1
  | _ => 2
  end
| _ => 0
end|},
    (),
  ),
  /* Fun with let body */
  test_format_seg(
    ~name="Fun with let body",
    ~input="fun x -> let y = 1 in x + y",
    ~expected={|fun x ->
  let y = 1 in
  x + y|},
    (),
  ),
  /* Fun with case body */
  test_format_seg(
    ~name="Fun with case body (short)",
    ~input="fun x -> case x | 0 => 1 end",
    ~expected={|fun x ->
  case x
  | 0 => 1
  end|},
    (),
  ),
  /* if nested in if conseq */
  test_format_seg(
    ~name="If with nested if conseq",
    ~input="if a then if b then 1 else 2 else 3",
    ~expected={|if a
then
  if b then 1 else 2
else 3|},
    (),
  ),
];

let tests = [
  ("PrettyPrint.TrailingHoles", trailing_hole_tests),
  ("PrettyPrint.BlockFormBody", block_form_body_tests),
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
  ("PrettyPrint.Forms", form_tests),
  ("PrettyPrint.Composition", composition_tests),
  ("PrettyPrint.LabeledTuple", labeled_tuple_tests),
];
