open DrvSyntax;

module SymbolMap =
  SymbolMap.M({
    type target = t;
    let f: string => target = s => Var(s) |> fresh;
  });
open SymbolMap;

let scan1 = msg => Printf.sprintf(Scanf.format_from_string(msg, "%s"));
let scan2 = msg => Printf.sprintf(Scanf.format_from_string(msg, "%s%s"));
let scan3 = msg => Printf.sprintf(Scanf.format_from_string(msg, "%s%s%s"));
let scan4 = msg => Printf.sprintf(Scanf.format_from_string(msg, "%s%s%s%s"));
let exp_form: DrvTermBase.exp_term => (t, string) =
  fun
  | Hole(_) => (Hole("") |> fresh, "")
  | Var(x) => (Var(x) |> fresh, scan1("The variable `%s`", x))
  | Abbr(_) => (
      Var("$e") |> fresh,
      "The abbreviation represents the definition of $e.",
    )
  | Parens(_) => (
      Var("(e)") |> fresh,
      "The parenthesis is used to explicitly group expressions. This does not carry other semantic meaning.",
    )
  | Val(_) => (
      Val(v) |> fresh,
      scan1(
        "The value judgement defines the values in ALFA, i.e. `%s` is a value.",
        show(v),
      ),
    )
  | Eval(_) => (
      Eval(e, v) |> fresh,
      scan2(
        "The evaluation judgement defines the evaluation behavior of ALFA expressions, i.e. it relates an expression `%s` to its value `%s`.",
        show(e),
        show(v),
      ),
    )
  | Entail(_) => (
      Entail(gamma, a) |> fresh,
      scan2(
        "The judgement defines that the context `%s` entails the proposition `%s`.",
        show(gamma),
        show(a),
      ),
    )
  | Consistent(_) => (
      Consistent(t1, t2) |> fresh,
      scan2(
        "A Type consistency judgement is a weakened form of equivalence: `%s` and `%s` are consistent if they differ only up to the appearance of an unknown type.",
        show(t1),
        show(t2),
      ),
    )
  | MatchedArrow(_) => (
      MatchedArrow(t, Arrow(t1, t2) |> fresh) |> fresh,
      scan4(
        "The matched arrow judgement defines that the type `%s` matches the arrow type `%s`. When `%s` is already an arrow type, it matches to itself. When `%s` is the unknown type, then it gets matched to `? -> f`.",
        show(t),
        show(Arrow(t1, t2) |> fresh),
        show(t),
        show(t),
      ),
    )
  | MatchedProd(_) => (
      MatchedProd(t, Prod(t1, t2) |> fresh) |> fresh,
      scan4(
        "The matched product judgement defines that the type `%s` matches the product type `%s`. When `%s` is already a product type, it matches to itself. When `%s` is the unknown type, then it gets matched to `? * ?`.",
        show(t),
        show(Prod(t1, t2) |> fresh),
        show(t),
        show(t),
      ),
    )
  | MatchedSum(_) => (
      MatchedSum(t, Sum(t1, t2) |> fresh) |> fresh,
      scan4(
        "The matched sum judgement defines that the type `%s` matches the sum type `%s`. When `%s` is already a sum type, it matches to itself. When `%s` is the unknown type, then it gets matched to `? + ?`.",
        show(t),
        show(Sum(t1, t2) |> fresh),
        show(t),
        show(t),
      ),
    )
  | Type(_) => (
      Entail(delta, Type(t) |> fresh) |> fresh,
      scan2(
        "The type validity judgement defines that the type validation context `%s` entails that the type variable `%s` does actually stand for a valid type.",
        show(delta),
        show(t),
      ),
    )
  | HasType(_) => (
      Entail(gamma, HasType(e, t) |> fresh) |> fresh,
      scan3(
        "The type judgement defines that the expression `%s` has type `%s` assuming the context `%s`.",
        show(e),
        show(t),
        show(gamma),
      ),
    )
  | Syn(_) => (
      Entail(gamma, Syn(e, t) |> fresh) |> fresh,
      scan3(
        "The type synthesis judgement defines that the expression `%s` synthesizes type `%s` assuming the context `%s`.",
        show(e),
        show(t),
        show(gamma),
      ),
    )
  | Ana(_) => (
      Entail(gamma, Ana(e, t) |> fresh) |> fresh,
      scan3(
        "The type analysis judgement defines that the expression `%s` analyzes against type `%s` assuming the context `%s`.",
        show(e),
        show(t),
        show(gamma),
      ),
    )
  | And(_) => (
      And(a, b) |> fresh,
      scan2(
        "The conjunction proposition is true if both `%s` and `%s` are true assuming the given hypothesis.",
        show(a),
        show(b),
      ),
    )
  | Or(_) => (
      Or(a, b) |> fresh,
      scan2(
        "The disjunction proposition is true if either `%s` or `%s` is true assuming the given hypothesis.",
        show(a),
        show(b),
      ),
    )
  | Impl(_) => (
      Impl(a, b) |> fresh,
      scan2(
        "The implication proposition is true if whenever `%s` is true, `%s` is also true assuming the given hypothesis.",
        show(a),
        show(b),
      ),
    )
  | Truth => (
      Truth |> fresh,
      "The tautology proposition is true under any hypothesis.",
    )
  | Falsity => (
      Falsity |> fresh,
      "The absurdity proposition is false under any hypothesis.",
    )
  | Ctx(_) => (
      Var("[A, B, ...]") |> fresh,
      "The context literal is a list of propositions written in Hazel syntax.",
    )
  | Cons(_) => (
      Var("A :: [B, ...]") |> fresh,
      scan2(
        "The context extension proposition defines that the context `%s` extended with `%s`.",
        show(a),
        show(b),
      ),
    )
  | Concat(_) => (
      Var("[A, B] @ [C, ...]") |> fresh,
      scan2(
        "The context concatenation proposition defines that the context `%s` concatenated with `%s`.",
        show(a),
        show(b),
      ),
    )
  | NumLit(n) => (NumLit(n) |> fresh, "A number literal")
  | Neg(_) => (
      Neg(n) |> fresh,
      scan1(
        "The negation expression defines the negation of numlit `%s`.",
        show(n),
      ),
    )
  | Plus(_) => (
      Plus(n1, n2) |> fresh,
      scan2(
        "The addition expression defines the sum of numlit `%s` and `%s`.",
        show(n1),
        show(n2),
      ),
    )
  | Minus(_) => (
      Minus(n1, n2) |> fresh,
      scan2(
        "The subtraction expression defines the difference between numlit `%s` and `%s`.",
        show(n1),
        show(n2),
      ),
    )
  | Times(_) => (
      Times(n1, n2) |> fresh,
      scan2(
        "The multiplication expression defines the product of numlit `%s` and `%s`.",
        show(n1),
        show(n2),
      ),
    )
  | Lt(_) => (
      Lt(n1, n2) |> fresh,
      scan2(
        "The less-than expression defines the comparison between numlit `%s` and `%s`.",
        show(n1),
        show(n2),
      ),
    )
  | Gt(_) => (
      Gt(n1, n2) |> fresh,
      scan2(
        "The greater-than expression defines the comparison between numlit `%s` and `%s`.",
        show(n1),
        show(n2),
      ),
    )
  | Eq(_) => (
      Eq(n1, n2) |> fresh,
      scan2(
        "The equal-to expression defines the comparison between numlit `%s` and `%s`.",
        show(n1),
        show(n2),
      ),
    )
  | True => (True |> fresh, "The boolean literal true")
  | False => (False |> fresh, "The boolean literal false")
  | If(_) => (
      If(e, e1, e2) |> fresh,
      scan3(
        "The if-then-else expression defines that if `%s` is true, then the result is `%s`, otherwise the result is `%s`.",
        show(e),
        show(e1),
        show(e2),
      ),
    )
  | Let(_) => (
      Let(x, e_def, e_body) |> fresh,
      scan3(
        "The let expression defines the local binding of `%s` to `%s` in the body `%s`.",
        show(e_def),
        show(x),
        show(e_body),
      ),
    )
  | Fix(_) => (
      Fix(x, e) |> fresh,
      scan2(
        "The fix expression defines the fixed-point of `%s` as the value of `%s`.",
        show(x),
        show(e),
      ),
    )
  | Fun(_) => (
      Fun(x, e) |> fresh,
      scan2(
        "The function expression defines the function with parameter `%s` and body `%s`.",
        show(x),
        show(e),
      ),
    )
  | Ap(_) => (
      Ap(e1, e2) |> fresh,
      scan2(
        "The application expression defines the application of function `%s` to argument `%s`.",
        show(e1),
        show(e2),
      ),
    )
  | Tuple(_) => (
      Pair(e1, e2) |> fresh,
      scan2(
        "The pair expression defines the product of `%s` and `%s`.",
        show(e1),
        show(e2),
      ),
    )
  | Triv => (Triv |> fresh, "The unit expression defines the unit literal.")
  | PrjL(_) => (
      PrjL(e) |> fresh,
      scan1(
        "The projection-left expression defines the left projection of `%s`.",
        show(e),
      ),
    )
  | PrjR(_) => (
      PrjR(e) |> fresh,
      scan1(
        "The projection-right expression defines the right projection of `%s`.",
        show(e),
      ),
    )
  | InjL(_) => (
      InjL(e) |> fresh,
      scan1(
        "The injection-left expression defines the left injection of `%s`.",
        show(e),
      ),
    )
  | InjR(_) => (
      InjR(e) |> fresh,
      scan1(
        "The injection-right expression defines the right injection of `%s`.",
        show(e),
      ),
    )
  | Case(_) => (
      Case(e, InjL(x) |> fresh, e1, InjL(y) |> fresh, e2) |> fresh,
      scan3(
        "The case expression defines the pattern matching of `%s` against `%s` and `%s`.",
        show(e),
        show(InjL(x) |> fresh),
        show(InjL(y) |> fresh),
      ),
    )
  | Roll(_) => (
      Roll(e) |> fresh,
      scan1("The roll expression defines the roll of `%s`.", show(e)),
    )
  | Unroll(_) => (
      Unroll(e) |> fresh,
      scan1("The unroll expression defines the unroll of `%s`.", show(e)),
    )
  | ExpHole => (ExpHole |> fresh, "The expression hole");

let typ_form: DrvTermBase.typ_term => (t, string) =
  fun
  | Hole(_) => (Hole("") |> fresh, "")
  | Var(x) => (TVar(x) |> fresh, scan1("The type variable `%s`", x))
  | Abbr(_) => (
      Var("$t") |> fresh,
      "The abbreviation represents the definition of type $t.",
    )
  | Num => (Num |> fresh, "The numlit type defines the type of numlit")
  | Bool => (Bool |> fresh, "The bool type defines the type of boolean")
  | Arrow(_) => (
      Arrow(t1, t2) |> fresh,
      scan2(
        "This arrow type defines the type of function that takes an argument of type `%s` and returns a value of type `%s`.",
        show(t1),
        show(t2),
      ),
    )
  | Prod(_) => (
      Prod(t1, t2) |> fresh,
      scan2(
        "The product type defines the type of pair of `%s` and `%s`.",
        show(t1),
        show(t2),
      ),
    )
  | Unit => (Unit |> fresh, "The unit type defines the type of unit literal")
  | Sum(_) => (
      Sum(t1, t2) |> fresh,
      scan2(
        "The sum type defines the type of either `%s` or `%s`.",
        show(t1),
        show(t2),
      ),
    )
  | Rec(_) => (
      Rec(x, t) |> fresh,
      scan2(
        "This recursive type defines the type of `%s` that is recursively defined by `%s`.",
        show(x),
        show(t),
      ),
    )
  | TypHole => (TypHole |> fresh, "The type hole")
  | Parens(_) => (
      Var("(t)") |> fresh,
      "The parenthesis type is used to explicitly group types. This does not carry other semantic meaning.",
    );

let pat_form: DrvTermBase.pat_term => (t, string) =
  fun
  | Hole(_) => (Hole("") |> fresh, "")
  | Var(x) => (Pat(x) |> fresh, "The pattern variable `%s`")
  | Cast(_) => (
      Cast(x, t) |> fresh,
      scan2(
        "Only expression that matches the pattern `%s` and have the type `%s` match this type annotation pattern.",
        show(x),
        show(t),
      ),
    )
  | InjL(_) => (
      InjL(x) |> fresh,
      scan1(
        "The left injection pattern matches any expression that is injected to the left.",
        show(x),
      ),
    )
  | InjR(_) => (
      InjR(x) |> fresh,
      scan1(
        "The right injection pattern matches any expression that is injected to the right.",
        show(x),
      ),
    )
  | Pair(_) => (
      PatPair(x, y) |> fresh,
      scan2(
        "The pair pattern matches any expression that matches both patterns `%s` and `%s`.",
        show(x),
        show(y),
      ),
    )
  | Parens(_) => (
      Var("(p)") |> fresh,
      "The parenthesis pattern is used to explicitly group patterns. This does not carry other semantic meaning.",
    );

let tpat_form: DrvTermBase.tpat_term => (t, string) =
  fun
  | Hole(_) => (Hole("") |> fresh, "")
  | Var(x) => (Var(x) |> fresh, "The type pattern variable `%s`");
