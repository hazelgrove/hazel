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
  project_tables: false,
  raise_if_padding: false,
};

let segmentize =
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings);

let format = (~width=80, input: string): string => {
  switch (Parser.to_term(input)) {
  | Some(exp) =>
    let segment = segmentize(exp);
    let pretty = PrettySegment.prettify(~width, segment);
    Printer.of_segment(~holes="?", ~indent="  ", pretty)
    |> Util.StringUtil.trim_trailing_whitespace;
  | None => failwith("Failed to parse: " ++ input)
  };
};

let test_format = (~name, ~width=80, ~input, ~expected, ()): test_case(_) =>
  test_case(name, `Quick, () =>
    check(string, name, expected, format(~width, input))
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
else
    2|},
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
| 1 => "one" end|},
    (),
  ),
  test_format(
    ~name="Case breaks rules",
    ~width=25,
    ~input={|case x | 0 => "zero" | 1 => "one" | _ => "other" end|},
    ~expected={|case x
| 0 => "zero"
| 1 => "one"
| _ => "other" end|},
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
else
    x - 1|},
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
    ~name="Let body with tuple preserves scope",
    ~width=15,
    ~input="let p = (1, 2, 3) in p",
    ~expected={|let p =
    (1, 2, 3) in
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

/* === Labeled tuples === */

let labeled_tuple_tests = [
  test_format(
    ~name="Labeled tuple flat",
    ~input="(a= 1, b = 2)",
    ~expected="(a= 1, b= 2)",
    (),
  ),
  test_format(
    ~name="Labeled tuple breaks vertically",
    ~width=15,
    ~input="(firsts = [1, 2], seconds = [3, 4])",
    ~expected={|(
    firsts= [1, 2],
    seconds= [3, 4]
)|},
    (),
  ),
  test_format(
    ~name="Labeled tuple single entry",
    ~input="(a= 1)",
    ~expected="(a= 1)",
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
  ("PrettyPrint.LabeledTuple", labeled_tuple_tests),
];
