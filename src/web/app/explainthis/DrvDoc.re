open Language;
open DrvTermBase;

module SymbolMap =
  SymbolMap.M({
    type exp = exp_t;
    type pat = pat_t;
    type typ = typ_t;
    type tpat = tpat_t;
    let exp: string => exp = s => Var(s) |> Drv.Exp.fresh;
    let pat: string => pat = s => Var(s) |> Drv.Pat.fresh;
    let typ: string => typ = s => Var(s) |> Drv.Typ.fresh;
    let tpat: string => tpat = s => Var(s) |> Drv.TPat.fresh;
  });
open SymbolMap;
open Haz3lcore;

let settings =
  ExpToSegment.Settings.{
    inline: true,
    fold_case_clauses: false,
    fold_fn_bodies: false,
    hide_fixpoints: false,
    show_filters: false,
    show_unknown_as_hole: false,
  };

let f_jdmt: exp_t => Segment.t =
  ExpToSegment.drv_exp_to_pretty(~settings, ~sort=Jdmt);
let f_ctx: exp_t => Segment.t =
  ExpToSegment.drv_exp_to_pretty(~settings, ~sort=Ctx);
let f_prop: exp_t => Segment.t =
  ExpToSegment.drv_exp_to_pretty(~settings, ~sort=Prop);
let f_exp: exp_t => Segment.t =
  ExpToSegment.drv_exp_to_pretty(~settings, ~sort=Exp);
let f_pat: pat_t => Segment.t = ExpToSegment.drv_pat_to_pretty(~settings);
let f_typ: typ_t => Segment.t = ExpToSegment.drv_typ_to_pretty(~settings);
let f_tpat: tpat_t => Segment.t = ExpToSegment.drv_tpat_to_pretty(~settings);

let exp_form: exp_t => (Segment.t, string) =
  exp =>
    switch (Drv.Exp.term_of(exp)) {
    | Hole(_) => (exp |> f_exp, "")
    | Var(_) => (exp |> f_exp, "The variable represents the expression.")
    | Quote(_) => (
        Var("$x") |> Drv.Exp.fresh |> f_exp,
        "The abbreviation represents the definition of $x.",
      )
    | Parens(_) => (
        Parens(e) |> Drv.Exp.fresh |> f_exp,
        "The parenthesis is used to explicitly group expressions. This does not carry other semantic meaning.",
      )
    | Val(_) => (
        Val(e) |> Drv.Exp.fresh |> f_jdmt,
        "The value judgement defines the values in ALFA, i.e. v is a value",
      )
    | Eval(_) => (
        Eval(e, v) |> Drv.Exp.fresh |> f_jdmt,
        "The evaluation judgement defines the evaluation behavior of ALFA expressions, i.e. it relates an expression e to its value v.",
      )
    | Entail(_) => (
        Entail(gamma, a) |> Drv.Exp.fresh |> f_jdmt,
        "The judgement defines that the context gamma entails the proposition a.",
      )
    | Consistent(_) => (
        Consistent(t1, t2) |> Drv.Exp.fresh |> f_jdmt,
        "A Type consistency judgement is a weakened form of equivalence: t1 and t2 are consistent if they differ only up to the appearance of an unknown type.",
      )
    | MatchedArrow(_) => (
        MatchedArrow(t, Arrow(t1, t2) |> Drv.Typ.fresh)
        |> Drv.Exp.fresh
        |> f_jdmt,
        "The matched arrow judgement defines that the type t matches the arrow type Arrow(t1, t2). When t is already an arrow type, it matches to itself. When t is the unknown type, then it gets matched to ? -> f.",
      )
    | MatchedProd(_) => (
        MatchedProd(t, Prod(t1, t2) |> Drv.Typ.fresh)
        |> Drv.Exp.fresh
        |> f_jdmt,
        "The matched product judgement defines that the type t matches the product type Prod(t1, t2). When t is already a product type, it matches to itself. When t is the unknown type, then it gets matched to ? * ?.",
      )
    | MatchedSum(_) => (
        MatchedSum(t, Sum(t1, t2) |> Drv.Typ.fresh)
        |> Drv.Exp.fresh
        |> f_jdmt,
        "The matched sum judgement defines that the type t matches the sum type Sum(t1, t2). When t is already a sum type, it matches to itself. When t is the unknown type, then it gets matched to ? + ?.",
      )
    | Ctx([]) => (Ctx([]) |> Drv.Exp.fresh |> f_ctx, "The empty context.")
    | Ctx(_) => (
        Ctx([a, b, Var("...") |> Drv.Exp.fresh]) |> Drv.Exp.fresh |> f_ctx,
        "The context is a list of propositions A, B, ... The order does not matter.",
      )
    | Cons(_, _) => (
        Cons(a, Ctx([a, Var("...") |> Drv.Exp.fresh]) |> Drv.Exp.fresh)
        |> Drv.Exp.fresh
        |> f_ctx,
        "The context cons operation adds the proposition A to the context. The order does not matter.",
      )
    | Concat(_, _) => (
        Concat(Ctx([a, Var("...") |> Drv.Exp.fresh]) |> Drv.Exp.fresh, b)
        |> Drv.Exp.fresh
        |> f_ctx,
        "The context concatenation operation appends the proposition B to the context. The order does not matter.",
      )
    | Type(_) => (
        Type(t) |> Drv.Exp.fresh |> f_prop,
        "The type validity proposition defines that the type variable t does actually stand for a valid type.",
      )
    | HasType(_) => (
        HasType(e, t) |> Drv.Exp.fresh |> f_prop,
        "The type proposition defines that the expression e has type t",
      )
    | Syn(_) => (
        Syn(e, t) |> Drv.Exp.fresh |> f_prop,
        "The type synthesis proposition defines that the expression e synthesizes type t",
      )
    | Ana(_) => (
        Ana(e, t) |> Drv.Exp.fresh |> f_prop,
        "The type analysis proposition defines that the expression e analyzes against type t",
      )
    | And(_) => (
        And(a, b) |> Drv.Exp.fresh |> f_prop,
        "The conjunction proposition is true if both a and b are true assuming the given hypothesis.",
      )
    | Or(_) => (
        Or(a, b) |> Drv.Exp.fresh |> f_prop,
        "The disjunction proposition is true if either a or b is true assuming the given hypothesis.",
      )
    | Impl(_) => (
        Impl(a, b) |> Drv.Exp.fresh |> f_prop,
        "The implication proposition is true if a implies b assuming the given hypothesis.",
      )
    | Truth => (
        Truth |> Drv.Exp.fresh |> f_prop,
        "The truth proposition is always true.",
      )
    | Falsity => (
        Falsity |> Drv.Exp.fresh |> f_prop,
        "The falsity proposition is always false.",
      )
    | NumLit(_) => (n |> f_exp, "The numeric literal represents the number.")
    | Neg(_) => (
        Neg(e) |> Drv.Exp.fresh |> f_exp,
        "The negation of the expression e.",
      )
    | BinOp(op, _, _) => (
        BinOp(op, e1, e2) |> Drv.Exp.fresh |> f_exp,
        "The binary operation "
        ++ Grammar.Drv.show_op_bin(op)
        ++ " of the expressions e1 and e2.",
      )
    | True => (True |> Drv.Exp.fresh |> f_exp, "The boolean literal true.")
    | False => (False |> Drv.Exp.fresh |> f_exp, "The boolean literal false.")
    | If(_, _, _) => (
        If(e, e1, e2) |> Drv.Exp.fresh |> f_exp,
        "The conditional expression if e is true then e1 else e2.",
      )
    | Let(_, _, _) => (
        Let(x, e_def, e_body) |> Drv.Exp.fresh |> f_exp,
        "The let expression defines a binding of the variable x to the expression e_body in the expression e.",
      )
    | Fix(_, _) => (
        Fix(x, e) |> Drv.Exp.fresh |> f_exp,
        "The fixpoint expression defines a recursive binding of the variable x to the expression e.",
      )
    | Fun(_, _) => (
        Fun(x, e) |> Drv.Exp.fresh |> f_exp,
        "The function expression defines a lambda abstraction of the variable x in the expression e.",
      )
    | Ap(_, _) => (
        Ap(e1, e2) |> Drv.Exp.fresh |> f_exp,
        "The application of the expression e1 to the expression e2.",
      )
    | Tuple(_)
    | Pair(_, _) => (
        Pair(e1, e2) |> Drv.Exp.fresh |> f_exp,
        "The pair expression.",
      )
    | Triv => (Triv |> Drv.Exp.fresh |> f_exp, "The unit literal expression.")
    | PrjL(_) => (
        PrjL(e) |> Drv.Exp.fresh |> f_exp,
        "The projection of the left component of the pair expression e.",
      )
    | PrjR(_) => (
        PrjR(e) |> Drv.Exp.fresh |> f_exp,
        "The projection of the right component of the pair expression e.",
      )
    | InjL(_) => (
        InjL(e) |> Drv.Exp.fresh |> f_exp,
        "The injection of the left component of the sum expression e.",
      )
    | InjR(_) => (
        InjR(e) |> Drv.Exp.fresh |> f_exp,
        "The injection of the right component of the sum expression e.",
      )
    | Case(_) => (
        Case(e, x, e1, y, e2) |> Drv.Exp.fresh |> f_exp,
        "The case expression of the expression e with the patterns x and y and the expressions e1 and e2.",
      )
    | Roll(_) => (
        Roll(e) |> Drv.Exp.fresh |> f_exp,
        "The roll expression of the expression e.",
      )
    | Unroll(_) => (
        Unroll(e) |> Drv.Exp.fresh |> f_exp,
        "The unroll expression of the expression e.",
      )
    | ExpHole => (ExpHole |> Drv.Exp.fresh |> f_exp, "The expression hole.")
    };

let typ_form: typ_t => (Segment.t, string) =
  typ =>
    switch (Drv.Typ.term_of(typ)) {
    | Hole(_) => (typ |> f_typ, "")
    | Var(_) => (typ |> f_typ, "The type variable represents the type.")
    | Quote(_) => (
        Var("$x") |> Drv.Typ.fresh |> f_typ,
        "The abbreviation represents the definition of type $x.",
      )
    | Num => (
        Num |> Drv.Typ.fresh |> f_typ,
        "The numlit type defines the type of numlit",
      )
    | Bool => (
        Bool |> Drv.Typ.fresh |> f_typ,
        "The bool type defines the type of boolean",
      )
    | Arrow(_) => (
        Arrow(t1, t2) |> Drv.Typ.fresh |> f_typ,
        "This arrow type defines the type of function that takes an argument of type t1 and returns a value of type t2.",
      )
    | Prod(_) => (
        Prod(t1, t2) |> Drv.Typ.fresh |> f_typ,
        "The product type defines the type of pair of t1 and t2.",
      )
    | Unit => (
        Unit |> Drv.Typ.fresh |> f_typ,
        "The unit type defines the type of unit literal",
      )
    | Sum(_) => (
        Sum(t1, t2) |> Drv.Typ.fresh |> f_typ,
        "The sum type defines the type of either t1 or t2.",
      )
    | Rec(_) => (
        Rec(tpat, t) |> Drv.Typ.fresh |> f_typ,
        "This recursive type defines the type of t that is recursively defined by a.",
      )
    | TypHole => (TypHole |> Drv.Typ.fresh |> f_typ, "The type hole")
    | Parens(_) => (
        Var("(t)") |> Drv.Typ.fresh |> f_typ,
        "The parenthesis type is used to explicitly group types. This does not carry other semantic meaning.",
      )
    };

let pat_form: pat_t => (Segment.t, string) =
  pat =>
    switch (Drv.Pat.term_of(pat)) {
    | Hole(_) => (pat |> f_pat, "")
    | Quote(_) => (
        Var("$x") |> Drv.Pat.fresh |> f_pat,
        "The abbreviation represents the definition of pattern $x.",
      )
    | Var(_) => (pat |> f_pat, "The pattern variable represents the pattern.")
    | Cast(_) => (
        Cast(x, t) |> Drv.Pat.fresh |> f_pat,
        "Only expression that matches the pattern x and have the type t match this type annotation pattern.",
      )
    | InjL(_) => (
        InjL(x) |> Drv.Pat.fresh |> f_pat,
        "The left injection pattern matches any expression that is injected to L(x).",
      )
    | InjR(_) => (
        InjR(x) |> Drv.Pat.fresh |> f_pat,
        "The right injection pattern matches any expression that is injected to R(x).",
      )
    | Pair(_) => (
        Pair(x, y) |> Drv.Pat.fresh |> f_pat,
        "The pair pattern matches any expression that matches both patterns x and y.",
      )
    | Parens(_) => (
        Var("(x)") |> Drv.Pat.fresh |> f_pat,
        "The parenthesis pattern is used to explicitly group patterns. This does not carry other semantic meaning.",
      )
    };

let tpat_form: tpat_t => (Segment.t, string) =
  tpat =>
    switch (Drv.TPat.term_of(tpat)) {
    | Hole(_) => (tpat |> f_tpat, "")
    | Quote(_) => (
        Var("$x") |> Drv.TPat.fresh |> f_tpat,
        "The abbreviation represents the definition of type pattern $x.",
      )
    | Var(_) => (
        tpat |> f_tpat,
        "The type pattern variable represents the type pattern.",
      )
    };
