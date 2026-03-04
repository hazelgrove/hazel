/**
 * Tests for the Indicated module, which determines which piece
 * is "indicated" (under/near the cursor) at each caret position.
 *
 * Test format: each test specifies an input string with a ¦ caret
 * marker, and the expected indicated piece as a string with a tag
 * showing the direction and relation:
 *   [L,S] = Left side, Sibling
 *   [R,S] = Right side, Sibling
 *   [L,P] = Left side, Parent
 *   [R,P] = Right side, Parent
 *
 * Key insight: binary operators (+, -, *, etc.), prefix operators
 * (-, !), type annotations (:), and commas are all SIBLING tiles,
 * not parent tiles. Parent indication only occurs for delimited
 * forms like (...), [...], let...in, if...then...else, etc. when
 * the caret is at a child boundary.
 */
open Alcotest;
open Haz3lcore;
open Action;

let caret_char = "¦";
let convex_char = "?";
let concave_char = "~";

/* --- Test harness (adapted from Test_Editing) --- */

let string_to_ltr_actions = (s: string): list(Action.t) =>
  s |> Token.to_list |> List.map(c => Action.Insert(c));

let mv_l = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Move(Local(Left, ByChar)));

let perform = (zip: Zipper.t, actions: list(Action.t)): Zipper.t => {
  let perform = (a: Action.t, z: Zipper.t) =>
    Perform.go(
      ~statics=CachedStatics.empty,
      ~syntax=CachedSyntax.init(z),
      a,
      {zipper: z, col_target: None},
    );
  List.fold_left(
    (z: Zipper.t, a: Action.t) =>
      switch (perform(a, z)) {
      | Ok(z) => z
      | Error(err) =>
        Alcotest.fail("Failed on action: " ++ Action.Failure.show(err))
      },
    zip,
    actions,
  );
};

let mk = (init: string): list(Action.t) => {
  let rec split =
          (before: list(string), rest: list(string))
          : (list(string), list(string)) =>
    switch (rest) {
    | [] => Alcotest.fail("No caret in: " ++ init)
    | [hd, ...tl] =>
      hd == caret_char
        ? (List.rev(before), tl) : split([hd, ...before], tl)
    };
  let (before, after) = split([], Token.to_list(init));
  let s = Token.of_list(before @ after);
  string_to_ltr_actions(s) @ mv_l(List.length(after));
};

/* Convert a piece to a human-readable string */
let piece_to_string = (p: Piece.t): string =>
  Base.piece_to_string(
    ~holes=convex_char,
    ~concave_holes=concave_char,
    ~refractors=[],
    ~refractor_seg_to_seg=(r, s) => (r, s),
    ~projector_to_segment=Triggers.projector_to_invoke,
    p,
  );

/* Format the indication result as "piece_string [D,R]" */
let indicated_str = (z: Zipper.t): string =>
  switch (Indicated.for_decoration(z)) {
  | Some((p, dir, rel)) =>
    let dir_s =
      switch (dir) {
      | Left => "L"
      | Right => "R"
      };
    let rel_s =
      switch (rel) {
      | Sibling => "S"
      | Parent => "P"
      };
    piece_to_string(p) ++ " [" ++ dir_s ++ "," ++ rel_s ++ "]"
  | None => "<none>"
  };

let ind = (~name, ~input, ~indicated) =>
  test_case(name, `Quick, () => {
    let z = mk(input) |> perform(Zipper.init());
    check(
      testable(Fmt.string, String.equal),
      indicated,
      indicated,
      indicated_str(z),
    );
  });

/* Debug test: type raw characters and check indication without caret marker */

/* ==================================================================
 * TEST SUITES — Inward bias behavior
 *
 * Inward bias: when between two pieces at an Outer caret position,
 * we favor the piece whose caret-facing nib is Convex (term-shaped).
 * Single-token infix operators (like +, *, ,, ;) get a special case:
 * they are indicated at their left position since they have no inner
 * caret position and would otherwise have no indicated position.
 * ================================================================== */

let literal_tests = [
  /* --- Integer literals --- */
  ind(~name="Before int", ~input={|¦1|}, ~indicated="1 [R,S]"),
  ind(~name="After int", ~input={|1¦|}, ~indicated="1 [L,S]"),
  ind(~name="Before multi-digit int", ~input={|¦123|}, ~indicated="123 [R,S]"),
  ind(~name="After multi-digit int", ~input={|123¦|}, ~indicated="123 [L,S]"),
  /* --- Booleans --- */
  ind(~name="Before true", ~input={|¦true|}, ~indicated="true [R,S]"),
  ind(~name="After true", ~input={|true¦|}, ~indicated="true [L,S]"),
  /* --- Variables --- */
  ind(~name="Before var", ~input={|¦foo|}, ~indicated="foo [R,S]"),
  ind(~name="After var", ~input={|foo¦|}, ~indicated="foo [L,S]"),
  /* --- Explicit holes --- */
  ind(~name="Before explicit hole", ~input={|¦?|}, ~indicated="? [R,S]"),
  ind(~name="After explicit hole", ~input={|?¦|}, ~indicated="? [L,S]"),
];

let parens_tests = [
  /* --- Parenthesized expression (1) --- */
  ind(~name="Before parens", ~input={|¦(1)|}, ~indicated="(1) [R,S]"),
  ind(~name="Inside parens before content (inward bias: child over parent)",
    ~input={|(¦1)|}, ~indicated="1 [R,S]"),
  ind(~name="Inside parens after content",
    ~input={|(1¦)|}, ~indicated="1 [L,S]"),
  ind(~name="After parens", ~input={|(1)¦|}, ~indicated="(1) [L,S]"),
  /* --- Nested parens ((1)) --- */
  ind(~name="Nested: before inner parens (inward: child over parent)",
    ~input={|(¦(1))|}, ~indicated="(1) [R,S]"),
  ind(~name="Nested: inside inner before content (inward: child over parent)",
    ~input={|((¦1))|}, ~indicated="1 [R,S]"),
  ind(~name="Nested: inside inner after content",
    ~input={|((1¦))|}, ~indicated="1 [L,S]"),
  ind(~name="Nested: after inner parens",
    ~input={|((1)¦)|}, ~indicated="(1) [L,S]"),
  /* --- Parens with explicit hole --- */
  ind(~name="Parens with hole: before hole (inward: child over parent)",
    ~input={|(¦?)|}, ~indicated="? [R,S]"),
  ind(~name="Parens with hole: after hole",
    ~input={|(?¦)|}, ~indicated="? [L,S]"),
];

let binary_op_with_spaces_tests = [
  /* --- Addition with spaces: 1 + 2
   * Operators are SIBLINGS, so + appears as its own tile "+" */
  ind(~name="Plus: before left operand",
    ~input={|¦1 + 2|}, ~indicated="1 [R,S]"),
  ind(~name="Plus: after left operand",
    ~input={|1¦ + 2|}, ~indicated="1 [L,S]"),
  ind(~name="Plus: before operator",
    ~input={|1 ¦+ 2|}, ~indicated="+ [R,S]"),
  ind(~name="Plus: after operator",
    ~input={|1 +¦ 2|}, ~indicated="+ [L,S]"),
  ind(~name="Plus: before right operand",
    ~input={|1 + ¦2|}, ~indicated="2 [R,S]"),
  ind(~name="Plus: after right operand",
    ~input={|1 + 2¦|}, ~indicated="2 [L,S]"),
  /* --- Subtraction with spaces --- */
  ind(~name="Minus: after left operand",
    ~input={|3¦ - 1|}, ~indicated="3 [L,S]"),
  ind(~name="Minus: before operator",
    ~input={|3 ¦- 1|}, ~indicated="- [R,S]"),
  ind(~name="Minus: after operator",
    ~input={|3 -¦ 1|}, ~indicated="- [L,S]"),
  ind(~name="Minus: before right operand",
    ~input={|3 - ¦1|}, ~indicated="1 [R,S]"),
  /* --- Multi-char operator: ++ (string concat) --- */
  ind(~name="Concat: after left operand",
    ~input={|"a"¦ ++ "b"|}, ~indicated={|"a" [L,S]|}),
  ind(~name="Concat: before operator",
    ~input={|"a" ¦++ "b"|}, ~indicated="++ [R,S]"),
  ind(~name="Concat: inside operator (inner caret)",
    ~input={|"a" +¦+ "b"|}, ~indicated="++ [R,S]"),
  ind(~name="Concat: after operator",
    ~input={|"a" ++¦ "b"|}, ~indicated="++ [L,S]"),
  ind(~name="Concat: before right operand",
    ~input={|"a" ++ ¦"b"|}, ~indicated={|"b" [R,S]|}),
  /* --- Comparison: == (multi-char) --- */
  ind(~name="Equals: after left",
    ~input={|1¦ == 2|}, ~indicated="1 [L,S]"),
  ind(~name="Equals: before right",
    ~input={|1 == ¦2|}, ~indicated="2 [R,S]"),
  /* --- Logical operators --- */
  ind(~name="And: before operator",
    ~input={|true ¦&& false|}, ~indicated="&& [R,S]"),
  ind(~name="Or: before operator",
    ~input={|true ¦|| false|}, ~indicated={||| [R,S]|}),
  /* --- List cons --- */
  ind(~name="Cons: after head",
    ~input={|1¦ :: []|}, ~indicated="1 [L,S]"),
  ind(~name="Cons: before tail",
    ~input={|1 :: ¦[]|}, ~indicated="[] [R,S]"),
  /* --- List concat --- */
  ind(~name="List concat: after left",
    ~input={|[1]¦ @ [2]|}, ~indicated="[1] [L,S]"),
  ind(~name="List concat: before right",
    ~input={|[1] @ ¦[2]|}, ~indicated="[2] [R,S]"),
  /* --- Pipeline --- */
  ind(~name="Pipeline: after left",
    ~input={|1¦ |> f|}, ~indicated="1 [L,S]"),
  ind(~name="Pipeline: before right",
    ~input={|1 |> ¦f|}, ~indicated="f [R,S]"),
];

let binary_op_no_spaces_tests = [
  /* --- Without spaces: inward bias + single-token infix special case
   * At 1¦+2: R is single-token infix +, so + gets its left position
   * At 1+¦2: R is 2 (Convex left nib), inward bias picks 2 */
  ind(~name="Plus no space: after left operand (infix special case)",
    ~input={|1¦+2|}, ~indicated="+ [R,S]"),
  ind(~name="Plus no space: before right operand (inward bias)",
    ~input={|1+¦2|}, ~indicated="2 [R,S]"),
  /* --- Times no space --- */
  ind(~name="Times no space: after left (infix special case)",
    ~input={|2¦*3|}, ~indicated="* [R,S]"),
  ind(~name="Times no space: before right (inward bias)",
    ~input={|2*¦3|}, ~indicated="3 [R,S]"),
];

let type_annotation_tests = [
  /* --- Type ascription: 1 : Int
   * The colon is a sibling tile, not parent --- */
  ind(~name="Ascription: after expr",
    ~input={|1¦ : Int|}, ~indicated="1 [L,S]"),
  ind(~name="Ascription: before colon",
    ~input={|1 ¦: Int|}, ~indicated=": [R,S]"),
  ind(~name="Ascription: after colon",
    ~input={|1 :¦ Int|}, ~indicated=": [L,S]"),
  ind(~name="Ascription: before type",
    ~input={|1 : ¦Int|}, ~indicated="Int [R,S]"),
  ind(~name="Ascription: after type",
    ~input={|1 : Int¦|}, ~indicated="Int [L,S]"),
];

let let_binding_tests = [
  /* --- let x = 1 in x
   * The let tile includes its children (pattern, bound expr)
   * but the body is a sibling (right operand of prefix form).
   * Spaces after keywords are inside the child, so "let ¦x"
   * has caret after the space, indicating x not the let tile. */
  ind(~name="Let: before let keyword",
    ~input={|¦let x = 1 in x|},
    ~indicated="let x = 1 in [R,S]"),
  ind(~name="Let: after in keyword",
    ~input={|let x = 1 in¦ x|},
    ~indicated="let x = 1 in [L,S]"),
  ind(~name="Let: before body (after space)",
    ~input={|let x = 1 in ¦x|}, ~indicated="x [R,S]"),
  ind(~name="Let: after body",
    ~input={|let x = 1 in x¦|}, ~indicated="x [L,S]"),
  /* --- Inside the let binding's children --- */
  ind(~name="Let: before pattern (after space)",
    ~input={|let ¦x = 1 in x|}, ~indicated="x [R,S]"),
  ind(~name="Let: after pattern",
    ~input={|let x¦ = 1 in x|}, ~indicated="x [L,S]"),
  ind(~name="Let: before bound expr (after space)",
    ~input={|let x = ¦1 in x|}, ~indicated="1 [R,S]"),
  ind(~name="Let: after bound expr",
    ~input={|let x = 1¦ in x|}, ~indicated="1 [L,S]"),
];

let fun_tests = [
  /* --- fun x -> x
   * The fun tile includes pattern child; body is right operand --- */
  ind(~name="Fun: before fun keyword",
    ~input={|¦fun x -> x|}, ~indicated="fun x -> [R,S]"),
  ind(~name="Fun: before pattern (after space)",
    ~input={|fun ¦x -> x|}, ~indicated="x [R,S]"),
  ind(~name="Fun: after pattern",
    ~input={|fun x¦ -> x|}, ~indicated="x [L,S]"),
  ind(~name="Fun: after arrow",
    ~input={|fun x ->¦ x|}, ~indicated="fun x -> [L,S]"),
  ind(~name="Fun: before body (after space)",
    ~input={|fun x -> ¦x|}, ~indicated="x [R,S]"),
  ind(~name="Fun: after body",
    ~input={|fun x -> x¦|}, ~indicated="x [L,S]"),
];

let if_tests = [
  /* --- if true then 1 else 2
   * The if tile includes condition, then-branch children
   * but body (after else) is right operand --- */
  ind(~name="If: before if keyword",
    ~input={|¦if true then 1 else 2|},
    ~indicated="if true then 1 else [R,S]"),
  ind(~name="If: before condition (after space)",
    ~input={|if ¦true then 1 else 2|},
    ~indicated="true [R,S]"),
  ind(~name="If: after condition",
    ~input={|if true¦ then 1 else 2|},
    ~indicated="true [L,S]"),
  ind(~name="If: before then branch (after space)",
    ~input={|if true then ¦1 else 2|},
    ~indicated="1 [R,S]"),
  ind(~name="If: after then branch",
    ~input={|if true then 1¦ else 2|},
    ~indicated="1 [L,S]"),
  ind(~name="If: before else branch (after space)",
    ~input={|if true then 1 else ¦2|},
    ~indicated="2 [R,S]"),
  ind(~name="If: after else branch",
    ~input={|if true then 1 else 2¦|},
    ~indicated="2 [L,S]"),
];

let application_tests = [
  /* --- f(1)
   * Application is two tiles: Tile(f) and postfix Tile("("...")").
   * The parent inside (...) is the parens tile, NOT f(...) --- */
  ind(~name="App: before function",
    ~input={|¦f(1)|}, ~indicated="f [R,S]"),
  ind(~name="App: after function name",
    ~input={|f¦(1)|}, ~indicated="f [L,S]"),
  ind(~name="App: inside app before arg (inward: child over parent)",
    ~input={|f(¦1)|}, ~indicated="1 [R,S]"),
  ind(~name="App: inside app after arg",
    ~input={|f(1¦)|}, ~indicated="1 [L,S]"),
  ind(~name="App: after close paren",
    ~input={|f(1)¦|}, ~indicated="(1) [L,S]"),
  /* --- Nested: f(g(1)) --- */
  ind(~name="Nested app: before inner arg (inward: child over parent)",
    ~input={|f(g(¦1))|}, ~indicated="1 [R,S]"),
  ind(~name="Nested app: after inner arg",
    ~input={|f(g(1¦))|}, ~indicated="1 [L,S]"),
];

let list_tests = [
  /* --- [1, 2, 3] --- */
  ind(~name="List: before bracket",
    ~input={|¦[1, 2, 3]|}, ~indicated="[1, 2, 3] [R,S]"),
  ind(~name="List: before first element (inward: child over parent)",
    ~input={|[¦1, 2, 3]|}, ~indicated="1 [R,S]"),
  ind(~name="List: after first element (comma infix special case)",
    ~input={|[1¦, 2, 3]|}, ~indicated=", [R,S]"),
  ind(~name="List: before second element (after space)",
    ~input={|[1, ¦2, 3]|}, ~indicated="2 [R,S]"),
  ind(~name="List: after last element",
    ~input={|[1, 2, 3¦]|}, ~indicated="3 [L,S]"),
  ind(~name="List: after bracket",
    ~input={|[1, 2, 3]¦|}, ~indicated="[1, 2, 3] [L,S]"),
  /* --- Empty list --- */
  ind(~name="Empty list: before",
    ~input={|¦[]|}, ~indicated="[] [R,S]"),
  ind(~name="Empty list: after",
    ~input={|[]¦|}, ~indicated="[] [L,S]"),
];

let tuple_tests = [
  /* --- (1, 2)
   * Commas are sibling tiles inside the parens child --- */
  ind(~name="Tuple: before parens",
    ~input={|¦(1, 2)|}, ~indicated="(1, 2) [R,S]"),
  ind(~name="Tuple: before first element (inward: child over parent)",
    ~input={|(¦1, 2)|}, ~indicated="1 [R,S]"),
  ind(~name="Tuple: after first element (comma infix special case)",
    ~input={|(1¦, 2)|}, ~indicated=", [R,S]"),
  ind(~name="Tuple: before second element (after space)",
    ~input={|(1, ¦2)|}, ~indicated="2 [R,S]"),
  ind(~name="Tuple: after second element",
    ~input={|(1, 2¦)|}, ~indicated="2 [L,S]"),
  ind(~name="Tuple: after parens",
    ~input={|(1, 2)¦|}, ~indicated="(1, 2) [L,S]"),
];

let constructor_tests = [
  /* --- Some(1)
   * The constructor name is one tile, (...) is the postfix app tile --- */
  ind(~name="Constructor: before name",
    ~input={|¦Some(1)|}, ~indicated="Some [R,S]"),
  ind(~name="Constructor: after name",
    ~input={|Some¦(1)|}, ~indicated="Some [L,S]"),
  ind(~name="Constructor: before arg (inward: child over parent)",
    ~input={|Some(¦1)|}, ~indicated="1 [R,S]"),
  ind(~name="Constructor: after arg",
    ~input={|Some(1¦)|}, ~indicated="1 [L,S]"),
  ind(~name="Constructor: after close paren",
    ~input={|Some(1)¦|}, ~indicated="(1) [L,S]"),
];

let prefix_op_tests = [
  /* --- Negation: -1
   * Prefix operators are siblings, not parents.
   * Inward bias: at -¦1, the 1 (Convex left nib) is inward. --- */
  ind(~name="Negation: before minus",
    ~input={|¦-1|}, ~indicated="- [R,S]"),
  ind(~name="Negation: between minus and operand (inward bias)",
    ~input={|-¦1|}, ~indicated="1 [R,S]"),
  ind(~name="Negation: after operand",
    ~input={|-1¦|}, ~indicated="1 [L,S]"),
  /* --- Logical not: !true --- */
  ind(~name="Not: before bang",
    ~input={|¦!true|}, ~indicated="! [R,S]"),
  ind(~name="Not: between bang and operand (inward bias)",
    ~input={|!¦true|}, ~indicated="true [R,S]"),
  ind(~name="Not: after operand",
    ~input={|!true¦|}, ~indicated="true [L,S]"),
];

let precedence_tests = [
  /* --- 1 + 2 * 3 (parsed as 1 + (2 * 3) by precedence)
   * All operators and operands are siblings --- */
  ind(~name="Precedence: before 1",
    ~input={|¦1 + 2 * 3|}, ~indicated="1 [R,S]"),
  ind(~name="Precedence: after 1",
    ~input={|1¦ + 2 * 3|}, ~indicated="1 [L,S]"),
  ind(~name="Precedence: before 2 (after +space)",
    ~input={|1 + ¦2 * 3|}, ~indicated="2 [R,S]"),
  ind(~name="Precedence: after 2",
    ~input={|1 + 2¦ * 3|}, ~indicated="2 [L,S]"),
  ind(~name="Precedence: before 3 (after *space)",
    ~input={|1 + 2 * ¦3|}, ~indicated="3 [R,S]"),
  ind(~name="Precedence: after 3",
    ~input={|1 + 2 * 3¦|}, ~indicated="3 [L,S]"),
];

let case_tests = [
  /* --- case x | 1 => 2 | 3 => 4 end --- */
  ind(~name="Case: before case keyword",
    ~input={|¦case x | 1 => 2 | 3 => 4 end|},
    ~indicated="case x | 1 => 2 | 3 => 4 end [R,S]"),
  ind(~name="Case: before scrutinee (after space)",
    ~input={|case ¦x | 1 => 2 | 3 => 4 end|},
    ~indicated="x [R,S]"),
  ind(~name="Case: after scrutinee",
    ~input={|case x¦ | 1 => 2 | 3 => 4 end|},
    ~indicated="x [L,S]"),
];

let whitespace_tests = [
  /* --- Spaces around operands --- */
  ind(~name="Space before operand",
    ~input={| ¦1|}, ~indicated="1 [R,S]"),
  ind(~name="Trailing space after operand",
    ~input={|1 ¦|}, ~indicated="<none>"),
  /* --- Multiple spaces between operands --- */
  ind(~name="Between two spaces",
    ~input={|1  ¦ + 2|}, ~indicated="<none>"),
];

let inner_caret_tests = [
  /* --- Inner caret positions (inside tokens)
   * For Inner caret, the Outer bias lines don't fire,
   * so direction is often R (falls to default cases) --- */
  ind(~name="Inner: middle of variable",
    ~input={|f¦oo|}, ~indicated="foo [R,S]"),
  ind(~name="Inner: middle of keyword let",
    ~input={|l¦et x = 1 in x|}, ~indicated="let x = 1 in [R,S]"),
  ind(~name="Inner: middle of int",
    ~input={|1¦23|}, ~indicated="123 [R,S]"),
  ind(~name="Inner: middle of multi-char op",
    ~input={|1 =¦= 2|}, ~indicated="== [R,S]"),
];

let complex_tests = [
  /* --- let f = fun x -> x + 1 in f(2) --- */
  ind(~name="Complex: before let",
    ~input={|¦let f = fun x -> x + 1 in f(2)|},
    ~indicated="let f = fun x -> x + 1 in [R,S]"),
  ind(~name="Complex: function body operand",
    ~input={|let f = fun x -> x¦ + 1 in f(2)|},
    ~indicated="x [L,S]"),
  ind(~name="Complex: app arg (inward: child over parent)",
    ~input={|let f = fun x -> x + 1 in f(¦2)|},
    ~indicated="2 [R,S]"),
  /* --- Nested lets --- */
  ind(~name="Nested let: inner body",
    ~input={|let x = 1 in let y = 2 in x¦ + y|},
    ~indicated="x [L,S]"),
];

let type_arrow_tests = [
  /* --- 1 : Int -> Int
   * The -> is a sibling inside the type annotation --- */
  ind(~name="Type arrow: before left type (after :space)",
    ~input={|1 : ¦Int -> Int|}, ~indicated="Int [R,S]"),
  ind(~name="Type arrow: after left type",
    ~input={|1 : Int¦ -> Int|}, ~indicated="Int [L,S]"),
  ind(~name="Type arrow: before right type (after ->space)",
    ~input={|1 : Int -> ¦Int|}, ~indicated="Int [R,S]"),
  ind(~name="Type arrow: after right type",
    ~input={|1 : Int -> Int¦|}, ~indicated="Int [L,S]"),
];

let semicolon_tests = [
  /* --- Cell join: 1; 2
   * Semicolon is a single-token infix, gets its left position --- */
  ind(~name="Semi: after left expr (semicolon infix special case)",
    ~input={|1¦; 2|}, ~indicated="; [R,S]"),
  ind(~name="Semi: before right expr (after ;space)",
    ~input={|1; ¦2|}, ~indicated="2 [R,S]"),
  ind(~name="Semi: after right expr",
    ~input={|1; 2¦|}, ~indicated="2 [L,S]"),
];

/* ==================================================================
 * Inward bias specific tests — scenarios that specifically test
 * the difference between left-bias and inward-bias behavior
 * ================================================================== */

let inward_bias_tests = [
  /* --- Core motivation: inside delimiters, indicate child not parent --- */
  ind(~name="Parens hole: (¦?) indicates hole for type feedback",
    ~input={|(¦?)|}, ~indicated="? [R,S]"),
  ind(~name="App hole: f(¦?) indicates hole",
    ~input={|f(¦?)|}, ~indicated="? [R,S]"),
  ind(~name="List hole: [¦?] indicates hole",
    ~input={|[¦?]|}, ~indicated="? [R,S]"),
  /* --- Postfix tile right edge: after content, indicate content not parent --- */
  ind(~name="Inside parens at right edge: (1¦) indicates 1",
    ~input={|(1¦)|}, ~indicated="1 [L,S]"),
  ind(~name="Inside list at right edge: [1¦] indicates 1",
    ~input={|[1¦]|}, ~indicated="1 [L,S]"),
  /* --- Prefix operators: inward means toward the operand --- */
  ind(~name="Prefix -: inward picks operand",
    ~input={|-¦42|}, ~indicated="42 [R,S]"),
  ind(~name="Prefix !: inward picks operand",
    ~input={|!¦false|}, ~indicated="false [R,S]"),
  /* --- Single-token infix special case --- */
  ind(~name="Plus gets left position in 1¦+2",
    ~input={|1¦+2|}, ~indicated="+ [R,S]"),
  ind(~name="Minus gets left position in 3¦-1",
    ~input={|3¦-1|}, ~indicated="- [R,S]"),
  ind(~name="Comma gets left position in (1¦,2)",
    ~input={|(1¦,2)|}, ~indicated=", [R,S]"),
  /* --- Multi-token operators: NOT single-token, so no special case --- */
  ind(~name="Concat ++: not single-token, inner caret available",
    ~input={|"a"¦++"b"|}, ~indicated={|"a" [L,S]|}),
  /* --- Inward at operator right edge picks operand --- */
  ind(~name="After + before 2 (no space): inward picks 2",
    ~input={|1+¦2|}, ~indicated="2 [R,S]"),
  ind(~name="After * before 3 (no space): inward picks 3",
    ~input={|2*¦3|}, ~indicated="3 [R,S]"),
  /* --- Let/fun child boundaries: spaces put you inside child --- */
  ind(~name="Let before pattern: in child, indicates pattern",
    ~input={|let ¦x = 1 in x|}, ~indicated="x [R,S]"),
  ind(~name="Let before body: after space, indicates body",
    ~input={|let x = 1 in ¦x|}, ~indicated="x [R,S]"),
  ind(~name="Fun before pattern: in child, indicates pattern",
    ~input={|fun ¦x -> x|}, ~indicated="x [R,S]"),
  ind(~name="If before condition: in child, indicates condition",
    ~input={|if ¦true then 1 else 2|}, ~indicated="true [R,S]"),
  /* --- Case scrutinee: in child, indicates scrutinee --- */
  ind(~name="Case before scrutinee: in child, indicates scrutinee",
    ~input={|case ¦x | 1 => 2 end|}, ~indicated="x [R,S]"),
  /* --- Nested delimiters: each level prefers its own child --- */
  ind(~name="Nested: outer paren left edge indicates inner paren",
    ~input={|(¦(1))|}, ~indicated="(1) [R,S]"),
  ind(~name="Nested: inner paren left edge indicates content",
    ~input={|((¦1))|}, ~indicated="1 [R,S]"),
];

let tests = [
  ("Indication.Literals", literal_tests),
  ("Indication.Parens", parens_tests),
  ("Indication.BinaryOpsSpaces", binary_op_with_spaces_tests),
  ("Indication.BinaryOpsNoSpaces", binary_op_no_spaces_tests),
  ("Indication.TypeAnnotation", type_annotation_tests),
  ("Indication.LetBinding", let_binding_tests),
  ("Indication.Fun", fun_tests),
  ("Indication.If", if_tests),
  ("Indication.Application", application_tests),
  ("Indication.Lists", list_tests),
  ("Indication.Tuples", tuple_tests),
  ("Indication.Constructors", constructor_tests),
  ("Indication.PrefixOps", prefix_op_tests),
  ("Indication.Precedence", precedence_tests),
  ("Indication.Case", case_tests),
  ("Indication.Whitespace", whitespace_tests),
  ("Indication.InnerCaret", inner_caret_tests),
  ("Indication.Complex", complex_tests),
  ("Indication.TypeArrow", type_arrow_tests),
  ("Indication.Semicolon", semicolon_tests),
  ("Indication.InwardBias", inward_bias_tests),
];
