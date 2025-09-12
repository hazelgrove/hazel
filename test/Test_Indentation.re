open Util;
open Alcotest;
open Haz3lcore;
open Test_Editing;

let test_indent = (~name, ~init, ~goal): test_case(_) => {
  /* Here, we trim trailing whitespace as current regrouting may
     introduce extraneous trailing whitespace during entry */
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, (a, b) => {
        String.equal(
          StringUtil.trim_trailing_whitespace(a),
          StringUtil.trim_trailing_whitespace(b),
        )
      }),
      goal,
      goal,
      init
      |> string_to_ltr_actions
      |> perform(Zipper.init(~root=Exp))
      |> Printer.of_zipper(
           ~holes=convex_char,
           ~concave_holes=concave_char,
           /* No caret for now */
           ~indent=" ",
         ),
    )
  );
};

let indentation_tests = [
  /* INDENTATION OF COMPLETE SYNTAX */
  test_indent(
    ~name="Top level doesn't auto indent",
    ~init={|
  1|},
    ~goal={|
  1|},
  ),
  test_indent(
    ~name="Bidelimited context same-line terminator indents",
    ~init={|(
1)|},
    ~goal={|(
  1)|},
  ),
  test_indent(
    ~name=
      "Double bidelimited context same-line terminator doesn't double indents",
    ~init={|((
1))|},
    ~goal={|((
  1))|},
  ),
  test_indent(
    ~name="Nested bidelimited contex with linebreaks",
    ~init={|(
(
1
)
)|},
    ~goal={|(
  (
    1
  )
)|},
  ),
  test_indent(
    ~name="let",
    ~init={|let a =
b
in 1|},
    ~goal={|let a =
  b
in 1|},
  ),
  test_indent(
    ~name="if then else",
    ~init={|if true
then
1
else 2|},
    ~goal={|if true
then
  1
else 2|},
  ),
  test_indent(
    ~name="if expression in bidelimited context",
    ~init={|(
if true
then
1
else
2
)|},
    ~goal={|(
  if true
  then
    1
  else
    2
)|},
  ),
  test_indent(
    ~name="Bidelimited context indents",
    ~init={|(
1
)|},
    ~goal={|(
  1
)|},
  ),
  test_indent(
    ~name="Double bidelimited context doesn't double indents",
    ~init={|((
1
))|},
    ~goal={|((
  1
))|},
  ),
  test_indent(
    ~name="Operators don't indent 1",
    ~init={|1 +
2|},
    ~goal={|1 +
2|},
  ),
  test_indent(
    ~name="Operators don't indent 2",
    ~init={|1
+ 2|},
    ~goal={|1
+ 2|},
  ),
  test_indent(
    ~name="Operators in nested context",
    ~init={|(
1+
2
)|},
    ~goal={|(
  1+
  2
)|},
  ),
  test_indent(
    ~name="Function application",
    ~init={|go(
1,
2
)|},
    ~goal={|go(
  1,
  2
)|},
  ),
  test_indent(
    ~name="Nested function application",
    ~init={|go(
Ap(
Lam(
"yo",
Var("yo")),
Lam(
"bro",
Var("bro")))
)|},
    ~goal=
      {|go(
  Ap(
    Lam(
      "yo",
      Var("yo")),
    Lam(
      "bro",
      Var("bro")))
)|},
  ),
  test_indent(
    ~name="Case rules with and without linebreaks after `=>`",
    ~init=
      {|let length : [Int] -> Int =
fun xs ->
case xs
| [] => 0
| hd::tl =>
1 + length(tl)
end
in 1|},
    ~goal=
      {|let length : [Int] -> Int =
  fun xs ->
    case xs
    | [] => 0
    | hd::tl =>
      1 + length(tl)
    end
in 1|},
  ),
  test_indent(
    ~name="Nested cases",
    ~init=
      {|let go: Exp -> Result =
fun e ->
case e
| Var(n) =>
Error("Free Variable")
| Lam(x, body) =>
Ok(Lam(x, body))
| Ap(e1,e2) =>
case go(e1)
| Ok(Lam(x, body)) =>
case go(e2)
| Error(err) =>  Error(err)
| Ok(arg) =>
go(subst(arg, x, body)) end
| _ => Error("Not a Function") end end in go|},
    ~goal=
      {|let go: Exp -> Result =
  fun e ->
    case e
    | Var(n) =>
      Error("Free Variable")
    | Lam(x, body) =>
      Ok(Lam(x, body))
    | Ap(e1,e2) =>
      case go(e1)
      | Ok(Lam(x, body)) =>
        case go(e2)
        | Error(err) =>  Error(err)
        | Ok(arg) =>
          go(subst(arg, x, body)) end
      | _ => Error("Not a Function") end end in go|},
  ),
  test_indent(
    ~name="Even: commas, function literals, if expression",
    ~init=
      {|let (even : Int -> Bool, odd : Int -> Bool) = (
fun n ->
if n == 0
then
true
else
odd(n - 1),
fun n ->
if n == 0 then false else even(n - 1)
) in 1|},
    ~goal=
      {|let (even : Int -> Bool, odd : Int -> Bool) = (
  fun n ->
    if n == 0
    then
      true
    else
      odd(n - 1),
  fun n ->
    if n == 0 then false else even(n - 1)
) in 1|},
  ),
  test_indent(
    ~name="Indentation of Complete Tuples 1",
    ~init={|let a = (
1,
2
) in 1|},
    ~goal={|let a = (
  1,
  2
) in 1|},
  ),
  test_indent(
    ~name="Indentation of Complete Tuples 2",
    ~init={|let a = (
fun x -> x,
1
) in 1|},
    ~goal={|let a = (
  fun x -> x,
  1
) in 1|},
  ),
  test_indent(
    ~name="Indentation of Complete Tuples 3 (Commas reset)",
    ~init={|let a = (
fun x ->
x,
1
) in 1|},
    ~goal={|let a = (
  fun x ->
    x,
  1
) in 1|},
  ),
  test_indent(
    ~name="Indentation of Complete Tuples 3 (Commas on own linereset)",
    ~init={|let a =
(
fun x ->
x
,
2
) in 1|},
    ~goal={|let a =
  (
    fun x ->
      x
    ,
    2
  ) in 1|},
  ),
  /* INDENTATION OF INCOMPLETE SYNTAX */
  test_indent(
    ~name="Indentation Incomplete Flow 0",
    ~init={|let
a|},
    ~goal={|let
  a|},
  ),
  test_indent(
    ~name="Indentation Incomplete Flow 1",
    ~init={|let a =
    1|},
    ~goal={|let a =
      1|},
  ),
  test_indent(
    ~name="Indentation Incomplete Flow 2",
    ~init={|let a =
fun x ->
|},
    ~goal={|let a =
  fun x ->
    ?|},
  ),
  test_indent(
    ~name="Indentation Incomplete Flow 3",
    ~init={|let a =
fun x ->
case x
||},
    ~goal={|let a =
  fun x ->
    case x
    |?|},
  ),
  test_indent(
    ~name="Indentation Incomplete Flow 4",
    ~init={|let a =
fun x ->
case x
| _ =>
|},
    ~goal={|let a =
  fun x ->
    case x
    | _ =>
      ?|},
  ),
  test_indent(
    ~name="Indentation - Wrapping immediate next lines",
    ~init={|let a =
let b = 2 in
b|},
    ~goal={|let a =
  let b = 2 in
  b|},
  ),
  test_indent(
    ~name="Indentation - Don't wrap over blank line 1",
    ~init={|let a =

let b = 2 in
b|},
    ~goal={|let a =

let b = 2 in
b|},
  ),
  test_indent(
    ~name="Indentation - Don't wrap over blank line 2",
    ~init={|let a = fun x ->

let b = 2 in
b|},
    ~goal={|let a = fun x ->

let b = 2 in
b|},
  ),
  test_indent(
    ~name="Commas should reset indentation",
    ~init={|let a = (
fun x ->
1,
|},
    ~goal={|let a = (
  fun x ->
    1,
  ?|},
  ),
];

let tests = [("Editing.Indentation", indentation_tests)];
