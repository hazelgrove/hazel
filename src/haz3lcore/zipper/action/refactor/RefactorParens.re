/* Static splice parenthesization: does a term spliced at a given
 * position need wrapping parens? Table-driven over Precedence (no
 * printing), with reparses_region as the escape hatch. Split out of
 * RefactorBase; consumed by the inline/feed/reduce transforms. */
open Language;
open RefactorBase;

/* === Static splice parenthesization ===
   The parser molds by precedence, so whether a spliced term needs
   parens is decidable from the same tables — no printing, no
   reparse. Two-sided:

   EXPOSURE (per side of the spliced term d): the loosest precedence
   at which d's structure is exposed to capture on that side; None
   when that edge is delimited (keyword, parens, case..end).

   BOUND (per side of the target slot): each operator that will sit
   adjacent on that side in the print, found by walking up the
   ancestor fringe (a child on its parent's left fringe adjoins, on
   its left, the operator of the first ancestor where the fringe
   breaks); a delimiter edge ends the walk.

   Parens are needed iff some bound CAPTURES that side's exposure:
   bound tighter than exposure, or equal without associativity on
   d's side. Anything not understood (MultiHole, Invalid, modules)
   exposes at Precedence.min => conservative parens. Reparse
   identity remains the TEST oracle (reparse-safety + fuzz). */

/* looser = higher int (Precedence: higher precedence = lower int) */
let sp_loosest = (a: option(Precedence.t), b: option(Precedence.t)) =>
  switch (a, b) {
  | (None, x)
  | (x, None) => x
  | (Some(a), Some(b)) => Some(a > b ? a : b)
  };

/* per-side exposure of a term: None = that edge is delimited */
let rec sp_exposure =
        (~side: Util.Direction.t, e: Exp.t): option(Precedence.t) => {
  let root = ExpToSegment.external_precedence(e);
  let open_child: option(Exp.t) =
    switch (IdTagged.term_of(e), side) {
    /* both edges are operands */
    | (BinOp(_, l, _), Util.Direction.Left)
    | (Cons(l, _), Util.Direction.Left)
    | (ListConcat(l, _), Util.Direction.Left)
    | (Seq(l, _), Util.Direction.Left)
    | (TupleExtension(l, _), Util.Direction.Left)
    | (Asc(l, _), Util.Direction.Left)
    | (Dot(l, _), Util.Direction.Left) => Some(l)
    | (BinOp(_, _, r), Util.Direction.Right)
    | (Cons(_, r), Util.Direction.Right)
    | (ListConcat(_, r), Util.Direction.Right)
    | (Seq(_, r), Util.Direction.Right)
    | (TupleExtension(_, r), Util.Direction.Right) => Some(r)
    /* tuple: first/last element */
    | (Tuple([x, ..._]), Util.Direction.Left) => Some(x)
    | (Tuple(xs), Util.Direction.Right) when xs != [] =>
      Some(List.nth(xs, List.length(xs) - 1))
    /* ap: fn side open, arg side closed by its parens */
    | (Ap(Forward, f, _), Util.Direction.Left) => Some(f)
    | (Ap(Forward, _, _), Util.Direction.Right) => None
    | (Ap(Reverse, l, _), Util.Direction.Left) => Some(l)
    | (Ap(Reverse, _, r), Util.Direction.Right) => Some(r)
    /* right-open keyword forms: left edge is the keyword */
    | (Let(_, _, b), Util.Direction.Right)
    | (TyAlias(_, _, b), Util.Direction.Right)
    | (Use(_, b), Util.Direction.Right)
    | (Theorem(_, _, b), Util.Direction.Right)
    | (Fun(_, b, _, _), Util.Direction.Right)
    | (TypFun(_, b, _), Util.Direction.Right)
    | (FixF(_, b, _), Util.Direction.Right)
    | (Forall(_, b), Util.Direction.Right)
    | (If(_, _, b), Util.Direction.Right)
    | (UnOp(_, b), Util.Direction.Right)
    | (Filter(_, b), Util.Direction.Right) => Some(b)
    | (Let(_), Util.Direction.Left)
    | (TyAlias(_), Util.Direction.Left)
    | (Use(_), Util.Direction.Left)
    | (Theorem(_), Util.Direction.Left)
    | (Fun(_), Util.Direction.Left)
    | (TypFun(_), Util.Direction.Left)
    | (FixF(_), Util.Direction.Left)
    | (Forall(_), Util.Direction.Left)
    | (If(_), Util.Direction.Left)
    | (UnOp(_), Util.Direction.Left)
    | (Filter(_), Util.Direction.Left) => None
    /* Asc right side is a TYPE: expose at the asc level itself
       (type-side capture, e.g. `1 : Int , Bool`, is guarded by the
       root exposure; type operators looser than comma don't exist
       in exp contexts) */
    | (Asc(_), Util.Direction.Right) => None
    | (Dot(_), Util.Direction.Right) => None
    | (TupLabel(_, x), Util.Direction.Right) => Some(x)
    | (TupLabel(_), Util.Direction.Left) => None
    | _ => None
    };
  let self =
    switch (IdTagged.term_of(e)) {
    /* delimited-both forms expose nothing themselves */
    | Var(_)
    | Atom(_)
    | EmptyHole
    | Constructor(_)
    | Label(_)
    | BuiltinFun(_)
    | Undefined
    | Deferral(_)
    | LivelitName(_)
    | Parens(_)
    | ListLit(_)
    | Test(_)
    | HintedTest(_)
    | Match(_)
    | Tuple([]) => None
    /* keyword forms: delimited on the left only */
    | Let(_)
    | TyAlias(_)
    | Use(_)
    | Theorem(_)
    | Fun(_)
    | TypFun(_)
    | FixF(_)
    | Forall(_)
    | If(_)
    | UnOp(_)
    | Filter(_) => side == Left ? None : Some(root)
    /* ap exposes fn-side only */
    | Ap(Forward, _, _) => side == Left ? Some(root) : None
    | _ => Some(root)
    };
  let deeper =
    switch (open_child) {
    | Some(c) => sp_exposure(~side, c)
    | None => None
    };
  sp_loosest(self, deeper);
};

/* the slot's adjacent-operator bounds on one side: walk up from the
   spliced position; each (parent, child) step either ends at a
   delimiter, contributes the parent operator's bound, or continues
   up an open fringe. Bounds carry which side of THAT OPERATOR the
   spliced material sits on (for the associativity tie-break). */
type sp_bound = {
  prec: Precedence.t,
  /* d sits on this side of the adjacent operator */
  d_side: Util.Direction.t,
};

let sp_bounds = (~side: Util.Direction.t, path: list(Exp.t)): list(sp_bound) => {
  /* innermost-first pairs: (parent, child) */
  let rec pairs = (path: list(Exp.t)) =>
    switch (path) {
    | [p, c, ...rest] => [(p, c), ...pairs([c, ...rest])]
    | _ => []
    };
  let steps = List.rev(pairs(path));
  let rec walk = (steps: list((Exp.t, Exp.t)), acc: list(sp_bound)) =>
    switch (steps) {
    | [] => acc
    | [(p, c), ...rest] =>
      let cid = Exp.rep_id(c);
      let is = (x: Exp.t) => Exp.rep_id(x) == cid;
      /* (this side's classification) */
      let cls: [
        | `Delim
        | `Op(Precedence.t, Util.Direction.t)
        | `Fringe
        | `Opaque
      ] = {
        let op = prec =>
          `Op((
            prec,
            side == Util.Direction.Left
              ? Util.Direction.Right : Util.Direction.Left,
          ));
        /* d_side: bound found on d's LEFT means d is on that
           operator's RIGHT, and vice versa */
        switch (IdTagged.term_of(p), side) {
        | (BinOp(o, l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.of_bin_op(o)) : `Opaque
        | (BinOp(o, l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.of_bin_op(o)) : `Opaque
        | (Cons(l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.cons) : `Opaque
        | (Cons(l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.cons) : `Opaque
        | (ListConcat(l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.concat) : `Opaque
        | (ListConcat(l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.concat) : `Opaque
        | (Seq(l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.semi) : `Opaque
        | (Seq(l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.semi) : `Opaque
        | (TupleExtension(l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.plus) : `Opaque
        | (TupleExtension(l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.plus) : `Opaque
        | (Tuple(xs), Util.Direction.Left) =>
          xs != [] && is(List.hd(xs))
            ? `Fringe : List.exists(is, xs) ? op(Precedence.comma) : `Opaque
        | (Tuple(xs), Util.Direction.Right) =>
          xs != [] && is(List.nth(xs, List.length(xs) - 1))
            ? `Fringe : List.exists(is, xs) ? op(Precedence.comma) : `Opaque
        | (Asc(l, _), Util.Direction.Left) => is(l) ? `Fringe : `Delim
        | (Asc(l, _), Util.Direction.Right) =>
          is(l) ? op(Precedence.asc) : `Delim
        | (Dot(l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.dot) : `Opaque
        | (Dot(l, _), Util.Direction.Right) =>
          is(l) ? op(Precedence.dot) : `Delim
        /* ap: fn slot is left-fringe / right-delimited (the arg
           parens); arg slot fully delimited */
        | (Ap(Forward, f, _), Util.Direction.Left) =>
          is(f) ? `Fringe : `Delim
        | (Ap(Forward, f, _), Util.Direction.Right) =>
          is(f) ? op(Precedence.ap) : `Delim
        | (Ap(Reverse, l, r), Util.Direction.Left) =>
          is(l) ? `Fringe : is(r) ? op(Precedence.eqs) : `Opaque
        | (Ap(Reverse, l, r), Util.Direction.Right) =>
          is(r) ? `Fringe : is(l) ? op(Precedence.eqs) : `Opaque
        /* keyword forms: interior slots delimited; final body is
           left-delimited, right-fringe */
        | (Let(_, d, b), _) =>
          if (is(d)) {
            `Delim; /* = ... in */
          } else if (is(b)) {
            side == Left ? `Delim : `Fringe;
          } else {
            `Opaque;
          }
        | (TyAlias(_, _, b), _)
        | (Use(_, b), _)
        | (Filter(_, b), _) =>
          is(b) ? side == Left ? `Delim : `Fringe : `Delim
        | (Theorem(_, d, b), _) =>
          if (is(d)) {
            `Delim;
          } else if (is(b)) {
            side == Left ? `Delim : `Fringe;
          } else {
            `Opaque;
          }
        | (Fun(_, b, _, _), _)
        | (TypFun(_, b, _), _)
        | (FixF(_, b, _), _)
        | (Forall(_, b), _) =>
          is(b) ? side == Left ? `Delim : `Fringe : `Opaque
        | (If(c1, t, alt), _) =>
          if (is(c1) || is(t)) {
            `Delim;
          } else if (is(alt)) {
            side == Left ? `Delim : `Fringe;
          } else {
            `Opaque;
          }
        | (Match(scrut, rules), _) =>
          is(scrut) || List.exists(((_, b)) => is(b), rules)
            ? `Delim : `Opaque
        | (Parens(_), _)
        | (ListLit(_), _)
        | (Test(_), _)
        | (HintedTest(_), _) => `Delim
        | (TupLabel(_, x), _) =>
          is(x) ? side == Left ? `Delim : `Fringe : `Delim
        | (UnOp(_, x), _) =>
          is(x) ? side == Left ? `Delim : `Fringe : `Opaque
        | _ => `Opaque
        };
      };
      switch (cls) {
      | `Delim => acc
      | `Op(p_, ds) =>
        walk(
          rest,
          [
            {
              prec: p_,
              d_side: ds,
            },
            ...acc,
          ],
        )
      | `Fringe => walk(rest, acc)
      | `Opaque => [
          {
            prec: Precedence.max,
            d_side: side == Left ? Util.Direction.Right : Util.Direction.Left,
          },
          ...acc,
        ] /* unknown parent: tightest bound = conservative parens */
      };
    };
  walk(steps, []);
};

let sp_captures = (bound: sp_bound, exposure: option(Precedence.t)): bool =>
  switch (exposure) {
  | None => false
  | Some(x) =>
    if (bound.prec < x) {
      true; /* adjacent op binds tighter: it steals */
    } else if (bound.prec > x) {
      false;
    } else {
      /* equal precedence: safe only on the operator's associative
         side */
      switch (Precedence.associativity(bound.prec)) {
      | Some(a) => a != bound.d_side
      | None => true
      };
    }
  };

/* parens needed to splice d at the position of `at` in program —
   static, table-driven, no printing */
let splice_parens_needed = (~program: Exp.t, ~at: Id.t, d: Exp.t): bool =>
  switch (find_path(~hit=e => Exp.rep_id(e) == at, program)) {
  | None
  | Some([_]) => false /* root: nothing adjacent */
  | Some(path) =>
    let check = side =>
      sp_bounds(~side, path)
      |> List.exists(b => sp_captures(b, sp_exposure(~side, d)));
    check(Util.Direction.Left) || check(Util.Direction.Right);
  };

let reparses_region = (region: Exp.t): bool => {
  let seg =
    ExpToSegment.exp_to_segment(~settings=roundtrip_settings, region)
    |> SpaceNormalize.go;
  let text = Printer.of_segment(~holes="?", ~refractors=[], seg);
  switch (Parser.to_segment(text, ~root=Exp)) {
  | None => false
  | Some(seg2) => Exp.fast_equal(MakeTerm.go(seg2).term, region)
  };
};
