open PrettySegment;
open Base;

let text_to_pretty = (id, sort, str): pretty => {
  p_just([
    Tile({
      id,
      label: [str],
      mold: Mold.mk_op(sort, []),
      shards: [0],
      children: [],
    }),
  ]);
};

// /* We assume that parentheses have already been added as necessary, and
//       that the expression has no DynamicErrorHoles, Casts, or FailedCasts
//    */
// let rec exp_to_pretty = (~inline, exp: Exp.t): pretty => {
//   let go = exp_to_pretty(~inline);
//   switch (exp |> Exp.term_of) {
//   | Invalid(x) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, x)
//   | EmptyHole =>
//     let id = exp |> Exp.rep_id;
//     p_just([Grout({id, shape: Convex})]);
//   | Bool(b) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, Bool.to_string(b))
//   | Int(n) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, Int.to_string(n))
//   // TODO: do floats print right?
//   | Float(f) =>
//     text_to_pretty(exp |> Exp.rep_id, Sort.Exp, Float.to_string(f))
//   | String(s) =>
//     text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "\"" ++ s ++ "\"")
//   | Var(v) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, v)
//   // TODO: add multi-line binop support?
//   | BinOp(op, l, r) =>
//     let id = exp |> Exp.rep_id;
//     let+ l = go(l)
//     and+ r = go(r);
//     l
//     @ [
//       Tile({
//         id,
//         label: [Operators.bin_op_to_string(op)],
//         mold: Mold.mk_bin(Precedence.of_bin_op(op), Sort.Exp, []),
//         shards: [0],
//         children: [l, r],
//       }),
//     ]
//     @ r;
//   };
// };

// Use Precedence.re to work out where your construct goes here.
let rec external_precedence = (exp: Exp.t): Precedence.t => {
  switch (Exp.term_of(exp)) {
  // Forms which we are about to strip, so we just look inside
  | Closure(_, x)
  | DynamicErrorHole(x, _) => external_precedence(x)

  // Binary operations are handled in Precedence.re
  | BinOp(op, _, _) => Precedence.of_bin_op(op)

  // Indivisible forms never need parentheses around them
  | Var(_)
  | Invalid(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | EmptyHole
  | Constructor(_)
  | Deferral(_)
  | BuiltinFun(_) => Precedence.max

  // Same goes for forms which are already surrounded
  | Parens(_)
  | ListLit(_)
  | Test(_)
  | Match(_) => Precedence.max

  // Other forms
  | UnOp(Meta(Unquote), _) => Precedence.unquote
  | Cast(_)
  | FailedCast(_) => Precedence.cast
  | Ap(Forward, _, _)
  | DeferredAp(_)
  | TypAp(_) => Precedence.ap
  | UnOp(Bool(Not), _) => Precedence.not_
  | UnOp(Int(Minus), _) => Precedence.neg
  | Cons(_) => Precedence.cons
  | Ap(Reverse, _, _) => Precedence.eqs
  | ListConcat(_) => Precedence.concat
  | If(_) => Precedence.if_
  | TypFun(_)
  | Fun(_)
  | FixF(_) => Precedence.fun_
  | Tuple(_) => Precedence.prod
  | Seq(_) => Precedence.semi

  // Top-level things
  | Filter(_)
  | TyAlias(_)
  | Let(_) => Precedence.let_

  // I think multiholes are min because we don't know the precedence of the `⟩?⟨`s
  | MultiHole(_) => Precedence.min
  };
};

let external_precedence_pat = (dp: Pat.t) =>
  switch (DHPat.term_of(dp)) {
  // Indivisible forms never need parentheses around them
  | EmptyHole
  | Wild
  | Invalid(_)
  | Var(_)
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Constructor(_) => Precedence.max

  // Same goes for forms which are already surrounded
  | ListLit(_)
  | Parens(_) => Precedence.max

  // Other forms
  | Cons(_) => Precedence.cons
  | Ap(_) => Precedence.ap
  | Cast(_) => Precedence.ann
  | Tuple(_) => Precedence.prod

  // I think multiholes are min because we don't know the precedence of the `⟩?⟨`s
  | MultiHole(_) => Precedence.min
  };

let paren_at = (internal_precedence: Precedence.t, exp: Exp.t): Exp.t =>
  external_precedence(exp) >= internal_precedence
    ? Exp.fresh(Parens(exp)) : exp;

let paren_assoc_at = (internal_precedence: Precedence.t, exp: Exp.t): Exp.t =>
  external_precedence(exp) > internal_precedence
    ? Exp.fresh(Parens(exp)) : exp;

let paren_pat_at = (internal_precedence: Precedence.t, pat: Pat.t): Pat.t =>
  external_precedence_pat(pat) >= internal_precedence
    ? Pat.fresh(Parens(pat)) : pat;

let rec parenthesize = (exp: Exp.t): Exp.t => {
  let (term, rewrap) = Exp.unwrap(exp);
  switch (term) {
  // Indivisible forms dont' change
  | Var(_)
  | Invalid(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | EmptyHole
  | Constructor(_)
  | Deferral(_)
  | BuiltinFun(_) => exp

  // Forms that currently need to stripped before oututting
  | Closure(_, x) => x
  | DynamicErrorHole(x, _) => x

  // Other forms
  | Fun(p, e, c, n) =>
    Fun(
      parenthesize_pat(p) |> paren_pat_at(Precedence.min),
      parenthesize(e) |> paren_assoc_at(Precedence.fun_),
      c, // TODO: Parenthesize through closure
      n,
    )
    |> rewrap
  | TypFun(tp, e, n) =>
    TypFun(tp, parenthesize(e) |> paren_assoc_at(Precedence.fun_), n)
    |> rewrap
  | Tuple(es) =>
    Tuple(
      es |> List.map(parenthesize) |> List.map(paren_at(Precedence.prod)),
    )
    |> rewrap
  | ListLit(es) =>
    ListLit(
      es |> List.map(parenthesize) |> List.map(paren_at(Precedence.concat)),
    )
    |> rewrap
  | Let(p, e1, e2) =>
    Let(
      parenthesize_pat(p) |> paren_pat_at(Precedence.min),
      parenthesize(e1) |> paren_at(Precedence.min),
      parenthesize(e2) |> paren_assoc_at(Precedence.let_),
    )
    |> rewrap
  | FixF(p, e, c) =>
    FixF(
      parenthesize_pat(p) |> paren_pat_at(Precedence.min),
      parenthesize(e) |> paren_assoc_at(Precedence.fun_),
      c // TODO: Parenthesize through closure
    )
    |> rewrap
  | TyAlias(tp, t, e) =>
    TyAlias(
      tp,
      t, // TODO: Types
      parenthesize(e) |> paren_assoc_at(Precedence.let_),
    )
    |> rewrap
  | Ap(Forward, e1, e2) =>
    Ap(
      Forward,
      parenthesize(e1) |> paren_at(Precedence.min),
      parenthesize(e2) |> paren_at(Precedence.min),
    )
    |> rewrap
  | Ap(Reverse, e1, e2) =>
    Ap(
      Reverse,
      parenthesize(e1) |> paren_assoc_at(Precedence.eqs),
      parenthesize(e2) |> paren_at(Precedence.eqs),
    )
    |> rewrap
  | TypAp(e, tp) =>
    TypAp(
      parenthesize(e) |> paren_assoc_at(Precedence.ap),
      tp // TODO: Types
    )
    |> rewrap
  | DeferredAp(e, es) =>
    DeferredAp(
      parenthesize(e) |> paren_assoc_at(Precedence.ap),
      es |> List.map(parenthesize) |> List.map(paren_at(Precedence.prod)),
    )
    |> rewrap
  | If(e1, e2, e3) =>
    If(
      parenthesize(e1) |> paren_at(Precedence.min),
      parenthesize(e2) |> paren_at(Precedence.min),
      parenthesize(e3) |> paren_assoc_at(Precedence.if_),
    )
    |> rewrap
  | Seq(e1, e2) =>
    Seq(
      parenthesize(e1) |> paren_at(Precedence.semi), // tempting to make this one assoc too
      parenthesize(e2) |> paren_assoc_at(Precedence.semi),
    )
    |> rewrap
  | Test(e) => Test(parenthesize(e) |> paren_at(Precedence.min)) |> rewrap
  | Filter(f, e) =>
    Filter(
      f, // TODO: Filters
      parenthesize(e) |> paren_at(Precedence.min),
    )
    |> rewrap
  | Parens(e) =>
    Parens(parenthesize(e) |> paren_at(Precedence.min)) |> rewrap
  | Cons(e1, e2) =>
    Cons(
      parenthesize(e1) |> paren_at(Precedence.cons),
      parenthesize(e2) |> paren_assoc_at(Precedence.cons),
    )
    |> rewrap
  | ListConcat(e1, e2) =>
    ListConcat(
      parenthesize(e1) |> paren_at(Precedence.concat),
      parenthesize(e2) |> paren_assoc_at(Precedence.concat),
    )
    |> rewrap
  | UnOp(Meta(Unquote), e) =>
    UnOp(Meta(Unquote), parenthesize(e) |> paren_at(Precedence.unquote))
    |> rewrap
  | UnOp(Bool(Not), e) =>
    UnOp(Bool(Not), parenthesize(e) |> paren_at(Precedence.not_)) |> rewrap
  | UnOp(Int(Minus), e) =>
    UnOp(Int(Minus), parenthesize(e) |> paren_at(Precedence.neg)) |> rewrap
  | BinOp(op, e1, e2) =>
    BinOp(
      op,
      parenthesize(e1) |> paren_assoc_at(Precedence.of_bin_op(op)),
      parenthesize(e2) |> paren_at(Precedence.of_bin_op(op)),
    )
    |> rewrap
  | Match(e, rs) =>
    Match(
      parenthesize(e) |> paren_at(Precedence.min),
      rs
      |> List.map(((p, e)) =>
           (
             parenthesize_pat(p) |> paren_pat_at(Precedence.min),
             parenthesize(e) |> paren_assoc_at(Precedence.case_),
           )
         ),
    )
    |> rewrap
  | MultiHole(_) => exp // TODO: Parenthesize through multiholes
  | Cast(e, t1, t2) =>
    // TODO: Types
    Cast(parenthesize(e) |> paren_assoc_at(Precedence.cast), t1, t2)
    |> rewrap
  | FailedCast(e, t1, t2) =>
    // TODO: Types
    FailedCast(parenthesize(e) |> paren_assoc_at(Precedence.cast), t1, t2)
    |> rewrap
  };
}
and parenthesize_pat = (pat: Pat.t): Pat.t => {
  // TODO: patterns
  pat;
};
