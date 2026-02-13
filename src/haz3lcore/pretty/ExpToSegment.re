open Util;
open PrettySegment;
open Base;
let mk_space = Secondary.mk_space;
let mk_newline = Secondary.mk_newline;
open Language;

/* Convert a list of Secondary.t to a Segment.t */
let secondary_to_segment = (secondaries: list(Secondary.t)): Segment.t =>
  List.map(s => Piece.Secondary(s), secondaries);

module Settings = {
  /* How to handle secondary (whitespace/comments) in output */
  type secondary_handling =
    | PreserveExact /* Use exactly what's stored in term annotations (for round-tripping) */
    | AutoFormat; /* Generate heuristically (original behavior) */

  /* How to handle parenthesization during output.
     See plans/secondary-in-terms-v2.md for detailed analysis. */
  type parenthesization =
    | Defensive /* Add parens based on precedence to ensure correct re-parsing (original behavior) */
    | Structural; /* Only emit parens that exist in term structure (for round-tripping) */

  /* How to format labels (backtick quoting).
     Note: Neither option gives perfect round-tripping because the original
     quoting information is lost during parsing. Labels like `a` (quoted but
     unnecessary) become just "a" in the term, so we can't know to re-quote them. */
  type label_format =
    | QuoteWhenNecessary /* Only add backticks for non-identifiers (original behavior) */
    | AlwaysQuote; /* Always add backticks to labels */

  type t = {
    secondary: secondary_handling,
    parenthesization,
    label_format,
    inline: bool, /* Only applies when secondary = AutoFormat */
    fold_case_clauses: bool,
    fold_fn_bodies: [
      | `Fold
      | `Text
      | `NoFold
    ],
    hide_fixpoints: bool,
    show_filters: bool,
    show_unknown_as_hole: bool,
  };

  let of_core = (~inline, ~fold_fn_bodies=?, settings: CoreSettings.t) => {
    secondary: AutoFormat,
    parenthesization: Defensive,
    label_format: QuoteWhenNecessary,
    inline,
    fold_case_clauses: !settings.evaluation.show_case_clauses,
    fold_fn_bodies:
      fold_fn_bodies
      |> Option.value(
           ~default=settings.evaluation.show_fn_bodies ? `NoFold : `Fold,
         ),
    hide_fixpoints: !settings.evaluation.show_fixpoints,
    show_filters: settings.evaluation.show_stepper_filters,
    show_unknown_as_hole: true,
  };

  let editable = (~inline) => {
    {
      secondary: AutoFormat,
      parenthesization: Defensive,
      label_format: QuoteWhenNecessary,
      inline,
      fold_case_clauses: false,
      fold_fn_bodies: `NoFold,
      hide_fixpoints: false,
      show_filters: true,
      show_unknown_as_hole: true,
    };
  };
};

/* Wrap segment content with secondary from a term's annotation.
   With PreserveExact: always emit stored secondary (even if empty).
   With AutoFormat: return content unchanged (heuristic spacing applies elsewhere). */
let wrap_with_secondary =
    (
      ~secondary: Settings.secondary_handling,
      term: IdTagged.t('a),
      content: Segment.t,
    )
    : Segment.t =>
  switch (secondary) {
  | PreserveExact =>
    let (before, after) = term.annotation.secondary;
    /* Always emit stored secondary, even if empty ([] @ content @ [] = content) */
    secondary_to_segment(before) @ content @ secondary_to_segment(after);
  | AutoFormat => content
  };

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
  | Atom(Bool(_) | Int(_) | SInt(_) | Float(_) | String(_) | Nat(_))
  | EmptyHole
  | Deferral(_)
  | ExplicitNonlabel
  | BuiltinFun(_)
  | Undefined
  | Label(_)
  | Constructor(_)
  | LivelitName(_)
  | TupLabel(_) => Precedence.max

  // Same goes for forms which are already surrounded
  | Parens(_)
  | Projector(_)
  | ListLit(_)
  | Test(_)
  | HintedTest(_)
  | ProofObject(_)
  | Match(_) => Precedence.max

  // Other forms
  | UnOp(Meta(Unquote), _) => Precedence.unquote

  | Asc(_) => Precedence.asc
  | Ap(Forward, _, _)
  | DeferredAp(_)
  | TypAp(_) => Precedence.ap
  | UnOp(Bool(Not), _) => Precedence.not_
  | UnOp(Int(Minus) | Nat(Minus) | Float(Minus) | SInt(Minus), _) => Precedence.neg
  | Cons(_) => Precedence.cons
  | Ap(Reverse, _, _) => Precedence.eqs
  | ListConcat(_) => Precedence.concat
  | If(_) => Precedence.if_
  | TypFun(_)
  | Fun(_)
  | FixF(_)
  | Forall(_) => Precedence.fun_
  | Tuple(_) => Precedence.comma
  | Seq(_) => Precedence.semi
  | TupleExtension(_, _) => Precedence.plus
  | Dot(_) => Precedence.dot

  // Top-level things
  | Filter(_)
  | TyAlias(_)
  | Use(_)
  | Let(_)
  | Theorem(_) => Precedence.let_

  // Matt: I think multiholes are min because we don't know the precedence of the `⟩?⟨`s
  | MultiHole(_) => Precedence.min
  | Module(_) => Precedence.min
  };
};

let external_precedence_pat = (dp: Pat.t) =>
  switch (DHPat.term_of(dp)) {
  // Indivisible forms never need parentheses around them
  | EmptyHole
  | Wild
  | ExplicitNonlabel
  | Invalid(_)
  | Var(_)
  | Atom(Bool(_) | Int(_) | SInt(_) | Float(_) | String(_) | Nat(_))
  | Constructor(_)
  | Label(_)
  | TupLabel(_) => Precedence.max

  // Same goes for forms which are already surrounded
  | ListLit(_)
  | Parens(_)
  | Projector(_) => Precedence.max

  // Other forms
  | Cons(_) => Precedence.cons
  | Ap(_) => Precedence.ap
  | Asc(_) => Precedence.asc
  | Tuple(_) => Precedence.comma

  // Matt: I think multiholes are min because we don't know the precedence of the `⟩?⟨`s
  | MultiHole(_) => Precedence.min
  };

let external_precedence_typ = (tp: Typ.t) =>
  switch (Typ.term_of(tp)) {
  // Indivisible forms never need parentheses around them
  | Unknown(Hole(Invalid(_)))
  | Unknown(Internal)
  | Unknown(SynSwitch)
  | Unknown(Hole(EmptyHole))
  | Var(_)
  | Atom(_)
  | Label(_)
  | ExplicitNonlabel
  | TupLabel(_) => Precedence.max
  | ProdProjection(_) => Precedence.dot
  | ProdExtension(_) => Precedence.ap
  // Same goes for forms which are already surrounded
  | Parens(_)
  | Projector(_)
  | ProofOf(_)
  | List(_) => Precedence.max

  // Other forms
  | Prod(_) => Precedence.comma
  | Arrow(_, _) => Precedence.type_arrow
  | Sum(_) => Precedence.type_plus
  | Rec(_, _) => Precedence.let_
  | Poly(_, _) => Precedence.let_

  // Matt: I think multiholes are min because we don't know the precedence of the `⟩?⟨`s
  | Unknown(Hole(MultiHole(_))) => Precedence.min
  | Sig(_) => Precedence.min
  };

/* Conditional parenthesization helpers.
   With Defensive: add parens based on precedence comparison (original behavior).
   With Structural: never add parens here; only explicit Parens nodes in the term are emitted. */
let paren_at =
    (
      ~parenthesization: Settings.parenthesization,
      internal_precedence: Precedence.t,
      exp: Exp.t,
    )
    : Exp.t =>
  switch (parenthesization) {
  | Structural => exp
  | Defensive =>
    external_precedence(exp) >= internal_precedence
      ? Exp.fresh(Parens(exp)) : exp
  };

let paren_assoc_at =
    (
      ~parenthesization: Settings.parenthesization,
      internal_precedence: Precedence.t,
      exp: Exp.t,
    )
    : Exp.t =>
  switch (parenthesization) {
  | Structural => exp
  | Defensive =>
    external_precedence(exp) > internal_precedence
      ? Exp.fresh(Parens(exp)) : exp
  };

let paren_pat_at =
    (
      ~parenthesization: Settings.parenthesization,
      internal_precedence: Precedence.t,
      pat: Pat.t,
    )
    : Pat.t =>
  switch (parenthesization) {
  | Structural => pat
  | Defensive =>
    external_precedence_pat(pat) >= internal_precedence
      ? Pat.fresh(Parens(pat)) : pat
  };

let paren_pat_assoc_at =
    (
      ~parenthesization: Settings.parenthesization,
      internal_precedence: Precedence.t,
      pat: Pat.t,
    )
    : Pat.t =>
  switch (parenthesization) {
  | Structural => pat
  | Defensive =>
    external_precedence_pat(pat) > internal_precedence
      ? Pat.fresh(Parens(pat)) : pat
  };

let paren_typ_at =
    (
      ~parenthesization: Settings.parenthesization,
      internal_precedence: Precedence.t,
      typ: Typ.t,
    )
    : Typ.t =>
  switch (parenthesization) {
  | Structural => typ
  | Defensive =>
    external_precedence_typ(typ) >= internal_precedence
      ? Typ.fresh(Parens(typ)) : typ
  };

let paren_typ_assoc_at =
    (
      ~parenthesization: Settings.parenthesization,
      internal_precedence: Precedence.t,
      typ: Typ.t,
    )
    : Typ.t =>
  switch (parenthesization) {
  | Structural => typ
  | Defensive =>
    external_precedence_typ(typ) > internal_precedence
      ? Typ.fresh(Parens(typ)) : typ
  };

let rec parenthesize =
        (
          ~parenthesization: Settings.parenthesization,
          ~show_filters: bool,
          ~already_paren=false,
          exp: Exp.t,
        )
        : Exp.t => {
  let parenthesize = parenthesize(~parenthesization, ~show_filters);
  let parenthesize_pat = parenthesize_pat(~parenthesization, ~show_filters);
  let parenthesize_typ = parenthesize_typ(~parenthesization, ~show_filters);
  let paren_at = paren_at(~parenthesization);
  let paren_assoc_at = paren_assoc_at(~parenthesization);
  let paren_pat_at = paren_pat_at(~parenthesization);
  let paren_typ_at = paren_typ_at(~parenthesization);
  /* For tuples: with Structural, don't auto-wrap in parens */
  let should_auto_wrap_tuple = parenthesization == Defensive;
  let (term, rewrap) = Exp.unwrap(exp);
  switch (term) {
  // Indivisible forms dont' change
  | Var(_)
  | Invalid(_)
  | Atom(_)
  | EmptyHole
  | LivelitName(_)
  //| Constructor(_) // Not indivisible because of the type annotation!
  | Deferral(_)
  | ExplicitNonlabel
  | BuiltinFun(_)
  | Tuple([])
  | Label(_)
  | Undefined => exp

  // Forms that currently need to stripped before outputting
  | Closure(_, x)
  | DynamicErrorHole(x, _) => parenthesize(x)
  | Filter(Filter({pat, act}), x) =>
    Filter(
      Filter({
        pat: parenthesize(pat),
        act,
      }),
      parenthesize(x),
    )
    |> rewrap
  | Filter(Residue(_), x) => x |> parenthesize
  // Other forms
  | Constructor(c, t) =>
    Constructor(c, Option.map(Option.map(paren_typ_at(Precedence.asc)), t))
    |> rewrap
  | Fun(p, e, typ, n) =>
    Fun(
      parenthesize_pat(p) |> paren_pat_at(Precedence.min),
      parenthesize(e) |> paren_assoc_at(Precedence.fun_),
      typ, // this typ is currently never output
      n,
    )
    |> rewrap
  | Forall(p, e) =>
    Forall(
      parenthesize_pat(p) |> paren_pat_at(Precedence.min),
      parenthesize(e) |> paren_assoc_at(Precedence.fun_),
    )
    |> rewrap
  | TypFun(tp, e, n) =>
    TypFun(tp, parenthesize(e) |> paren_assoc_at(Precedence.fun_), n)
    |> rewrap
  | Tuple([e])
      when
        switch (e.term) {
        | TupLabel(_) => false
        | _ => true
        } =>
    // Single-element tuples are printed as (_ = e)
    let inner =
      TupLabel(
        ExplicitNonlabel |> Exp.temp,
        parenthesize(e) |> paren_at(Precedence.comma),
      )
      |> rewrap;

    if (already_paren || !should_auto_wrap_tuple) {
      inner;
    } else {
      Parens(inner) |> Exp.fresh;
    };
  | Tuple(es) =>
    let inner =
      Tuple(
        es |> List.map(parenthesize) |> List.map(paren_at(Precedence.comma)),
      )
      |> rewrap;

    if (already_paren || !should_auto_wrap_tuple) {
      inner;
    } else {
      Parens(inner) |> Exp.fresh;
    };
  | TupLabel(l, e) =>
    TupLabel(l, parenthesize(e) |> paren_at(Precedence.min)) |> rewrap
  | Dot(e, l) =>
    Dot(
      parenthesize(e) |> paren_at(Precedence.dot),
      parenthesize(l) |> paren_at(Precedence.dot),
    )
    |> rewrap
  | TupleExtension(l, r) =>
    TupleExtension(
      parenthesize(l) |> paren_at(Precedence.dot),
      parenthesize(r) |> paren_assoc_at(Precedence.dot),
    )
    |> rewrap
  | ListLit(es) =>
    ListLit(
      es |> List.map(parenthesize) |> List.map(paren_at(Precedence.comma)),
    )
    |> rewrap
  | Let(p, e1, e2) =>
    Let(
      parenthesize_pat(p) |> paren_pat_at(Precedence.min),
      parenthesize(e1) |> paren_at(Precedence.min),
      parenthesize(e2) |> paren_assoc_at(Precedence.let_),
    )
    |> rewrap
  | Theorem(p, thm, e) =>
    Theorem(
      parenthesize_pat(p) |> paren_pat_at(Precedence.min),
      parenthesize(thm) |> paren_at(Precedence.min),
      parenthesize(e) |> paren_assoc_at(Precedence.let_),
    )
    |> rewrap
  | ProofObject(t) =>
    ProofObject(parenthesize(t) |> paren_at(Precedence.min)) |> rewrap
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
      parenthesize_typ(t) |> paren_typ_at(Precedence.min),
      parenthesize(e) |> paren_assoc_at(Precedence.let_),
    )
    |> rewrap
  | Use(t, e) =>
    Use(
      parenthesize_typ(t) |> paren_typ_at(Precedence.min),
      parenthesize(e) |> paren_assoc_at(Precedence.let_),
    )
    |> rewrap
  | Ap(Forward, e1, e2) =>
    Ap(
      Forward,
      parenthesize(e1) |> paren_assoc_at(Precedence.ap),
      parenthesize(~already_paren=true, e2) |> paren_at(Precedence.min),
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
      parenthesize_typ(tp) |> paren_typ_at(Precedence.min),
    )
    |> rewrap
  | DeferredAp(e, es) =>
    DeferredAp(
      parenthesize(e) |> paren_assoc_at(Precedence.ap),
      es |> List.map(parenthesize) |> List.map(paren_at(Precedence.comma)),
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
  | Asc(e, t) =>
    Asc(
      parenthesize(e) |> paren_assoc_at(Precedence.asc),
      parenthesize_typ(t) |> paren_typ_at(Precedence.asc),
    )
    |> rewrap
  | Test(e) => Test(parenthesize(e) |> paren_at(Precedence.min)) |> rewrap
  | HintedTest(e, hint) =>
    HintedTest(parenthesize(e) |> paren_at(Precedence.min), hint) |> rewrap
  | Parens(e) =>
    Parens(parenthesize(~already_paren=true, e) |> paren_at(Precedence.min))
    |> rewrap
  | Projector(data, e) =>
    Projector(data, parenthesize(e) |> paren_at(Precedence.min)) |> rewrap
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
  | UnOp((Int(Minus) | Nat(Minus) | Float(Minus) | SInt(Minus)) as op, e) =>
    UnOp(op, parenthesize(e) |> paren_at(Precedence.neg)) |> rewrap
  | BinOp(op, e1, e2) =>
    (
      switch (Precedence.of_bin_op(op) |> Precedence.associativity) {
      | Some(Left)
      | None =>
        BinOp(
          op,
          parenthesize(e1) |> paren_assoc_at(Precedence.of_bin_op(op)),
          parenthesize(e2) |> paren_at(Precedence.of_bin_op(op)),
        )
      | Some(Right) =>
        BinOp(
          op,
          parenthesize(e1) |> paren_at(Precedence.of_bin_op(op)),
          parenthesize(e2) |> paren_assoc_at(Precedence.of_bin_op(op)),
        )
      }
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
  | MultiHole(xs) =>
    MultiHole(
      List.map(parenthesize_any(~parenthesization, ~show_filters), xs),
    )
    |> rewrap
  | Module(_) => exp /* Phase 1.2: proper module parenthesization */
  };
}
and parenthesize_pat =
    (
      ~parenthesization: Settings.parenthesization,
      ~show_filters: bool,
      ~already_paren=false,
      pat: Pat.t,
    )
    : Pat.t => {
  let parenthesize_pat = parenthesize_pat(~parenthesization, ~show_filters);
  let parenthesize_typ = parenthesize_typ(~parenthesization, ~show_filters);
  let paren_pat_at = paren_pat_at(~parenthesization);
  let paren_pat_assoc_at = paren_pat_assoc_at(~parenthesization);
  let paren_typ_at = paren_typ_at(~parenthesization);
  let should_auto_wrap_tuple = parenthesization == Defensive;
  let (term, rewrap) = Pat.unwrap(pat);
  switch (term) {
  // Indivisible forms dont' change
  | Var(_)
  | Invalid(_)
  | Atom(_)
  | EmptyHole
  | Constructor(_)
  | Tuple([]) => pat

  // Other forms
  | Wild => pat
  | ExplicitNonlabel => pat
  | Parens(p) =>
    Parens(
      parenthesize_pat(~already_paren=true, p)
      |> paren_pat_at(Precedence.min),
    )
    |> rewrap
  | Projector(data, p) =>
    Projector(data, parenthesize_pat(p) |> paren_pat_at(Precedence.min))
    |> rewrap
  | Cons(p1, p2) =>
    Cons(
      parenthesize_pat(p1) |> paren_pat_at(Precedence.cons),
      parenthesize_pat(p2) |> paren_pat_assoc_at(Precedence.cons),
    )
    |> rewrap
  | Tuple(ps) =>
    let inner =
      Tuple(
        ps
        |> List.map(parenthesize_pat)
        |> List.map(paren_pat_at(Precedence.comma)),
      )
      |> rewrap;
    already_paren || !should_auto_wrap_tuple
      ? inner : Parens(inner) |> Pat.fresh;
  | Label(_) => pat
  | TupLabel(l, p) =>
    TupLabel(l, parenthesize_pat(p) |> paren_pat_at(Precedence.min))
    |> rewrap
  | ListLit(ps) =>
    ListLit(
      ps
      |> List.map(parenthesize_pat)
      |> List.map(paren_pat_at(Precedence.comma)),
    )
    |> rewrap
  | Ap(p1, p2) =>
    Ap(
      parenthesize_pat(p1) |> paren_pat_assoc_at(Precedence.ap),
      parenthesize_pat(p2) |> paren_pat_at(Precedence.min),
    )
    |> rewrap
  | MultiHole(xs) =>
    MultiHole(
      List.map(parenthesize_any(~parenthesization, ~show_filters), xs),
    )
    |> rewrap
  | Asc(p, t) =>
    Asc(
      parenthesize_pat(p) |> paren_pat_assoc_at(Precedence.asc),
      parenthesize_typ(t) |> paren_typ_at(Precedence.max) // Hack[Matt]: always add parens to get the arrows right
    )
    |> rewrap
  };
}

and parenthesize_typ =
    (
      ~parenthesization: Settings.parenthesization,
      ~show_filters: bool,
      ~already_paren=false,
      typ: Typ.t,
    )
    : Typ.t => {
  let parenthesize_typ = parenthesize_typ(~parenthesization, ~show_filters);
  let paren_typ_at = paren_typ_at(~parenthesization);
  let paren_typ_assoc_at = paren_typ_assoc_at(~parenthesization);
  let paren_at = paren_at(~parenthesization);
  let should_auto_wrap_tuple = parenthesization == Defensive;
  let (term, rewrap) = Typ.unwrap(typ);
  switch (term) {
  // Indivisible forms dont' change
  | Var(_)
  | Unknown(Hole(Invalid(_)))
  | Unknown(Internal)
  | Unknown(SynSwitch)
  | Unknown(Hole(EmptyHole))
  | Atom(_) => typ

  // Other forms
  | Parens(t) =>
    Parens(
      parenthesize_typ(~already_paren=true, t)
      |> paren_typ_at(Precedence.min),
    )
    |> rewrap
  | Projector(data, t) =>
    Projector(data, parenthesize_typ(t) |> paren_typ_at(Precedence.min))
    |> rewrap
  | List(t) =>
    List(parenthesize_typ(t) |> paren_typ_at(Precedence.min)) |> rewrap
  | Prod([]) => typ
  | Prod([t])
      when
        switch (t.term) {
        | TupLabel(_) => false
        | _ => true
        } =>
    // Single-element tuples are printed as (_ = e)
    let inner =
      TupLabel(
        ExplicitNonlabel |> Typ.temp,
        parenthesize_typ(t) |> paren_typ_at(Precedence.comma),
      )
      |> rewrap;

    if (already_paren || !should_auto_wrap_tuple) {
      inner;
    } else {
      Parens(inner) |> Typ.fresh;
    };
  | Prod(ts) =>
    let inner =
      Prod(
        ts
        |> List.map(parenthesize_typ)
        |> List.map(paren_typ_at(Precedence.comma)),
      )
      |> rewrap;
    already_paren || !should_auto_wrap_tuple
      ? inner : Parens(inner) |> Typ.fresh;
  | ExplicitNonlabel => typ
  | Label(_) => typ
  | TupLabel(l, t) =>
    TupLabel(l, parenthesize_typ(t) |> paren_typ_at(Precedence.min))
    |> rewrap
  | ProdProjection(t1, t2) =>
    ProdProjection(
      parenthesize_typ(t1) |> paren_typ_at(Precedence.dot),
      parenthesize_typ(t2) |> paren_typ_at(Precedence.dot),
    )
    |> rewrap
  | ProdExtension(t1, t2) =>
    ProdExtension(
      parenthesize_typ(t1) |> paren_typ_assoc_at(Precedence.plus),
      parenthesize_typ(t2) |> paren_typ_at(Precedence.plus),
    )
    |> rewrap
  | Rec(tp, t) =>
    Rec(
      tp,
      parenthesize_typ(t) |> paren_typ_assoc_at(Precedence.type_binder),
    )
    |> rewrap
  | Poly(tp, t) =>
    Poly(
      tp,
      parenthesize_typ(t) |> paren_typ_assoc_at(Precedence.type_binder),
    )
    |> rewrap
  | ProofOf(e) =>
    ProofOf(
      parenthesize(~parenthesization, ~show_filters, e)
      |> paren_at(Precedence.min),
    )
    |> rewrap
  | Arrow(t1, t2) =>
    Arrow(
      parenthesize_typ(t1) |> paren_typ_at(Precedence.type_arrow),
      parenthesize_typ(t2) |> paren_typ_assoc_at(Precedence.type_arrow),
    )
    |> rewrap
  | Sum(ts) =>
    Sum(
      ConstructorMap.map(
        ts =>
          ts
          |> Option.map(parenthesize_typ)
          |> Option.map(paren_typ_at(Precedence.type_plus)),
        ts,
      ),
    )
    |> rewrap
  | Unknown(Hole(MultiHole(xs))) =>
    Unknown(
      Hole(
        MultiHole(
          List.map(parenthesize_any(~parenthesization, ~show_filters), xs),
        ),
      ),
    )
    |> rewrap
  | Sig(_) => term |> rewrap
  };
}

and parenthesize_tpat =
    (
      ~parenthesization: Settings.parenthesization,
      ~show_filters: bool,
      tpat: TPat.t,
    )
    : TPat.t => {
  let (term, rewrap: TPat.term => TPat.t) = IdTagged.unwrap(tpat);
  switch (term) {
  // Indivisible forms dont' change
  | Var(_)
  | Invalid(_)
  | EmptyHole => tpat

  // Other forms
  | MultiHole(xs) =>
    MultiHole(
      List.map(parenthesize_any(~parenthesization, ~show_filters), xs),
    )
    |> rewrap
  };
}

and parenthesize_rul =
    (
      ~parenthesization: Settings.parenthesization,
      ~show_filters: bool,
      rul: Rul.t,
    )
    : Rul.t => {
  let (term, rewrap: Rul.term => Rul.t) = IdTagged.unwrap(rul);
  switch (term) {
  // Indivisible forms dont' change
  | Invalid(_) => rul

  // Other forms
  | Rules(e, ps) =>
    Rules(
      parenthesize(~parenthesization, ~show_filters, e),
      List.map(
        ((p, e)) =>
          (
            parenthesize_pat(~parenthesization, ~show_filters, p),
            parenthesize(~parenthesization, ~show_filters, e),
          ),
        ps,
      ),
    )
    |> rewrap
  | MultiHole(xs) =>
    MultiHole(
      List.map(parenthesize_any(~parenthesization, ~show_filters), xs),
    )
    |> rewrap
  };
}

and parenthesize_any =
    (
      ~parenthesization: Settings.parenthesization,
      ~already_paren=false,
      ~show_filters: bool,
      any: Any.t,
    )
    : Any.t =>
  switch (any) {
  | Exp(e) =>
    Exp(parenthesize(~parenthesization, ~already_paren, ~show_filters, e))
  | Pat(p) =>
    Pat(
      parenthesize_pat(~parenthesization, ~already_paren, ~show_filters, p),
    )
  | Typ(t) =>
    Typ(
      parenthesize_typ(~parenthesization, ~already_paren, ~show_filters, t),
    )
  | TPat(tp) => TPat(parenthesize_tpat(~parenthesization, ~show_filters, tp))
  | Rul(r) => Rul(parenthesize_rul(~parenthesization, ~show_filters, r))
  | Mod(_) => any /* Phase 1.2: proper module parenthesization */
  | Sig(_) => any
  | Any(_) => any
  };

let should_add_space = (s1, s2) =>
  switch () {
  | _ when String.ends_with(s1, ~suffix="(") => false
  | _ when String.ends_with(s1, ~suffix="[") => false
  | _ when String.starts_with(s2, ~prefix=")") => false
  | _ when String.starts_with(s2, ~prefix="]") => false
  | _ when String.starts_with(s2, ~prefix=",") => false
  | _ when String.starts_with(s2, ~prefix=";") => false
  | _ when String.starts_with(s2, ~prefix=":") => false
  | _ when String.ends_with(s1, ~suffix=":") =>
    String.starts_with(s2, ~prefix="$")
    || String.starts_with(s2, ~prefix="!")
  | _ when String.ends_with(s1, ~suffix=" ") => false
  | _ when String.starts_with(s2, ~prefix=" ") => false
  | _ when String.ends_with(s1, ~suffix="\n") => false
  | _ when String.starts_with(s2, ~prefix="\n") => false
  | _
      when
        String.ends_with(s1, ~suffix="PROJECTOR")
        && String.starts_with(s2, ~prefix="(") =>
    false
  | _
      when
        String.ends_with(s1, ~suffix=")")
        && String.starts_with(s2, ~prefix="(") =>
    false
  | _
      when
        Token.is_potential_operand(s1)
        && !Token.is_keyword(s1)
        && String.starts_with(s2, ~prefix="(") =>
    false
  | _ when String.ends_with(s1, ~suffix="…") =>
    /* Hack case for probe projector abbreviations */
    false
  | _ when s1 == "." && (Token.is_quoted_label(s2) || Token.is_var(s2)) =>
    false
  | _
      when
        s2 == "."
        && (
          Token.is_quoted_label(s1)
          || Token.is_var(s1)
          || String.ends_with(s1, ~suffix=")")
        ) =>
    false
  | _ => true
  };

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

/* Settings-aware form builder.
   PreserveExact: no heuristic spacing (children already have stored secondary)
   AutoFormat: add spaces based on heuristics */
let mk_form =
    (
      ~secondary: Settings.secondary_handling,
      form_name: Form.compound_form,
      id,
      children,
    )
    : Piece.t => {
  let form: Form.t = Form.get(form_name);
  assert(List.length(children) == List.length(form.mold.in_));
  // Add whitespaces only in AutoFormat mode
  let children =
    switch (secondary) {
    | PreserveExact =>
      /* In PreserveExact mode, children already have stored secondary wrapped */
      children
    | AutoFormat =>
      Aba.map_abas(
        ((l, child, r)) => {
          let lspace = should_add_space(l, child |> Segment.first_string);
          let rspace = should_add_space(child |> Segment.last_string, r);
          (lspace ? [Secondary(mk_space(Id.mk()))] : [])
          @ (rspace ? child @ [Secondary(mk_space(Id.mk()))] : child);
        },
        Aba.mk(form.label, children),
      )
      |> Aba.get_bs
    };
  Tile({
    id,
    label: form.label,
    mold: form.mold,
    shards: List.init(List.length(children) + 1, n => n),
    children,
  });
};

/* HACK[Matt]: Sometimes terms that should have multiple ids won't because
   evaluation only ever gives them one */
let pad_ids = (n: int, ids: list(Id.t)): list(Id.t) => {
  let len = List.length(ids);
  if (len < n) {
    ids @ List.init(n - len, _ => Id.mk());
  } else {
    ListUtil.split_n(n, ids) |> fst;
  };
};

/* Save standard list concatenation before we shadow @ */
let list_append = (@);

/* Settings-aware segment concatenation.
   PreserveExact: no heuristic spacing (rely on stored secondary)
   AutoFormat: add spaces based on heuristics */
let concat_segment =
    (
      ~secondary: Settings.secondary_handling,
      seg1: Segment.t,
      seg2: Segment.t,
    )
    : Segment.t =>
  switch (secondary) {
  | PreserveExact => list_append(seg1, seg2)
  | AutoFormat =>
    switch (seg1, seg2) {
    | ([], _) => seg2
    | (_, []) => seg1
    | _ =>
      if (should_add_space(
            Segment.last_string(seg1),
            Segment.first_string(seg2),
          )) {
        list_append(
          seg1,
          list_append([Secondary(mk_space(Id.mk()))], seg2),
        );
      } else {
        list_append(seg1, seg2);
      }
    }
  };

/* Default @ uses AutoFormat for backward compatibility */
let (@) = (seg1: Segment.t, seg2: Segment.t): Segment.t =>
  concat_segment(~secondary=AutoFormat, seg1, seg2);

let fold_if = (condition, pieces) =>
  if (condition) {
    let syntax =
      mk_form(~secondary=AutoFormat, ParensExp, Id.mk(), [pieces]);
    switch (MakeTerm.for_projection([syntax])) {
    | None => failwith("ExpToSegment.fold_if")
    | Some(any) => [ProjectorInit.init_or_noop(Fold, syntax, any)]
    };
  } else {
    pieces;
  };

let fold_fun_if = (condition, f_name: string, pieces, exp) =>
  switch (condition) {
  | `Fold =>
    let syntax =
      mk_form(~secondary=AutoFormat, ParensExp, Id.mk(), [pieces]);
    let str =
      FoldProj.sexp_of_t({
        text: f_name,
        expanded: false,
        always_render: true,
      })
      |> Sexplib.Sexp.to_string;
    [ProjectorInit.init_or_noop_from_str(Fold, syntax, Exp(exp), str)];
  | `Text =>
    let name =
      if (String.length(f_name) >= 2) {
        let len = String.length(f_name);
        let end_idx =
          if (len >= 3 && f_name.[len - 2] == '+') {
            len - 3;
          } else {
            len - 2;
          };
        String.sub(f_name, 1, max(0, end_idx));
      } else {
        "";
      };
    text_to_pretty(Id.mk(), Sort.Exp, name);
  | `NoFold => pieces
  };

/* We assume that parentheses have already been added as necessary, and
      that the expression has no Closures or DynamicErrorHoles
   */
let rec exp_to_pretty = (~settings: Settings.t, exp: Exp.t): pretty => {
  let go = (~inline=settings.inline) =>
    exp_to_pretty(
      ~settings={
        ...settings,
        inline,
      },
    );
  let wrap = wrap_with_secondary(~secondary=settings.secondary);
  /* Use settings-aware concatenation and form building */
  let (@) = concat_segment(~secondary=settings.secondary);
  let mk_form = mk_form(~secondary=settings.secondary);
  switch (exp |> Exp.term_of) {
  // Assume these have been removed by the parenthesizer
  | DynamicErrorHole(_)
  | Filter(Residue(_), _) => failwith("printing these not implemented yet")
  | Filter(Filter({pat, act}), e) =>
    let id = exp |> Exp.rep_id;
    let* p = go(pat);
    let+ e = go(e);
    wrap(
      exp,
      settings.show_filters
        ? {
          let form =
            switch (act) {
            | (Step, One) => Form.FilterPause
            | (Step, All) => Form.FilterDebug
            | (Eval, One) => Form.FilterHide
            | (Eval, All) => Form.FilterEval
            };
          [mk_form(form, id, [p])] @ e;
        }
        : e,
    );
  // Forms which should be removed by substitute_closures
  | Closure(_, e) =>
    let+ e = go(e);
    wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "<closure>") @ e);
  // Other cases
  | Invalid(x) => wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, x))
  | EmptyHole =>
    let id = exp |> Exp.rep_id;
    wrap(
      exp,
      p_just([
        Grout({
          id,
          shape: Convex,
        }),
      ]),
    );
  | Undefined =>
    wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "undefined"))
  | Atom(c) =>
    wrap(
      exp,
      text_to_pretty(exp |> Exp.rep_id, Sort.Exp, Atom.to_literal(c)),
    )
  // TODO: Make sure types are correct
  | Constructor(c, _t) =>
    // let id = Id.mk();
    let+ e = text_to_pretty(exp |> Exp.rep_id, Sort.Exp, c);
    // and+ t = typ_to_pretty(~settings: Settings.t, t);
    wrap(exp, e);
  // @ [mk_form("typeasc", id, [])]
  // @ (t |> fold_if(settings.fold_cast_types));
  | ListLit([]) =>
    wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "[]"))
  | Deferral(_) =>
    wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "_"))
  | ExplicitNonlabel =>
    wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "_"))
  | ListLit([x, ...xs]) =>
    /* ID order: [bracket_id] @ comma_ids (outer first, then adopted).
       IMPORTANT: Must align with MakeTerm.exp_term ListLit case,
       which produces IDs in this order during absorption. */
    let* x = go(x)
    and* xs = xs |> List.map(go) |> all;
    let (id, ids) = (
      IdTagged.ids(exp) |> List.hd,
      IdTagged.ids(exp) |> List.tl |> pad_ids(List.length(xs)),
    );
    let form = (x, xs) =>
      mk_form(
        ListLitExp,
        id,
        [
          x
          @ List.flatten(
              List.map2(
                (id, x) => [mk_form(CommaExp, id, [])] @ x,
                ids,
                xs,
              ),
            ),
        ],
      );
    wrap(exp, p_just([form(x, xs)]));
  // TODO: Add optional newlines
  | Var(v) => wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, v))
  | BinOp(op, l, r) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ l = go(l)
    and+ r = go(r);
    wrap(
      exp,
      l
      @ [
        Tile({
          id,
          label: [Operators.bin_op_to_string(op)],
          mold: Mold.mk_bin(Precedence.of_bin_op(op), Sort.Exp, []),
          shards: [0],
          children: [],
        }),
      ]
      @ r,
    );
  | TupleExtension(l, r) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ l = go(l)
    and+ r = go(r);
    wrap(
      exp,
      l
      @ [
        Tile({
          id,
          label: ["..."],
          mold: Mold.mk_bin(Precedence.plus, Sort.Exp, []),
          shards: [0],
          children: [],
        }),
      ]
      @ r,
    );
  | MultiHole(es) =>
    // TODO: Add optional newlines
    let+ es = es |> List.map(any_to_pretty(~settings)) |> all;
    /* Use IDs from the term for grout pieces, like Tuple uses for commas.
       For N elements, we need N-1 grout pieces (one between each pair). */
    let num_grouts = max(0, List.length(es) - 1);
    let ids = IdTagged.ids(exp) |> pad_ids(num_grouts);
    let seg =
      switch (es) {
      | [] => []
      | [first, ...rest] =>
        first
        @ List.flatten(
            List.map2(
              (id, e) =>
                [
                  Grout({
                    id,
                    shape: Concave,
                  }),
                  ...e,
                ],
              ids,
              rest,
            ),
          )
      };
    wrap(exp, seg);
  | Parens({term: Fun(p, e, _, _), _} as inner_exp) =>
    // TODO: Add optional newlines
    let id = inner_exp |> Exp.rep_id;
    let+ p = pat_to_pretty(~settings: Settings.t, p)
    and+ e = go(e);
    let name = Exp.get_fn_name(exp) |> Option.value(~default="anon fun");
    let name =
      if (settings.hide_fixpoints && String.ends_with(~suffix="+", name)) {
        String.sub(name, 0, String.length(name) - 1);
      } else {
        name;
      };
    let name = "<" ++ name ++ ">";
    let fun_form = [mk_form(Fun, id, [p])] @ e;
    wrap(
      exp,
      [mk_form(ParensExp, exp |> Exp.rep_id, [fun_form])]
      |> fold_fun_if(settings.fold_fn_bodies, name, _, inner_exp),
    );
  | LivelitName(s) =>
    wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "^" ++ s))
  | Fun(p, e, t, _) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let p =
      switch (t) {
      | None => p
      | Some(t) =>
        let t = t |> Exp.replace_all_ids_typ;
        Pat.fresh(Asc(p, t))
        |> parenthesize_pat(
             ~parenthesization=settings.parenthesization,
             ~show_filters=settings.show_filters,
           );
      };
    let+ p = pat_to_pretty(~settings: Settings.t, p)
    and+ e = go(e);
    let name = Exp.get_fn_name(exp) |> Option.value(~default="anon fun");
    let name =
      if (settings.hide_fixpoints && String.ends_with(~suffix="+", name)) {
        String.sub(name, 0, String.length(name) - 1);
      } else {
        name;
      };
    let name = "<" ++ name ++ ">";
    wrap(
      exp,
      [mk_form(Fun, id, [p])]
      @ e
      |> fold_fun_if(settings.fold_fn_bodies, name, _, exp),
    );
  | Forall(p, e) =>
    let id = exp |> Exp.rep_id;
    let+ p = pat_to_pretty(~settings: Settings.t, p)
    and+ e = go(e);
    wrap(exp, [mk_form(Forall, id, [p])] @ e);
  | TypFun(tp, e, _) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ tp = tpat_to_pretty(~settings: Settings.t, tp)
    and+ e = go(e);
    let name =
      "<"
      ++ (Exp.get_fn_name(exp) |> Option.value(~default="anon typfun"))
      ++ ">";
    wrap(
      exp,
      [mk_form(TypFun, id, [tp])]
      @ e
      |> fold_fun_if(settings.fold_fn_bodies, name, _, exp),
    );
  | Tuple([]) => wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "()"))
  | Tuple([{term: TupLabel(_), _} as le]) => go(le)
  | Tuple([x, ...xs]) =>
    // TODO: Add optional newlines
    let+ x = go(x)
    and+ xs = xs |> List.map(go) |> all;
    let ids = IdTagged.ids(exp) |> pad_ids(List.length(xs));
    wrap(
      exp,
      x
      @ List.flatten(
          List.map2((id, x) => [mk_form(CommaExp, id, [])] @ x, ids, xs),
        ),
    );
  | Label(l) =>
    wrap(
      exp,
      label_to_pretty(
        ~label_format=settings.label_format,
        ~label_only_position=false,
        Sort.Exp,
        Token.label_quote(l),
        exp |> Exp.rep_id,
      ),
    )
  | TupLabel(l, e) =>
    let* l =
      switch (l.term) {
      | Label(l') =>
        wrap(
          l,
          label_to_pretty(
            ~label_format=settings.label_format,
            ~label_only_position=true,
            Sort.Exp,
            l',
            l |> Exp.rep_id,
          ),
        )
      | _ => go(l)
      }
    and* e = go(e);

    wrap(
      exp,
      List.flatten([
        l,
        [
          Tile({
            id: exp |> Exp.rep_id,
            label: ["="],
            mold: Mold.mk_bin(Precedence.lab, Sort.Exp, []),
            shards: [0],
            children: [],
          }),
        ],
        if (Token.begins_with_potential_operator(Segment.first_string(e))) {
          [Secondary(mk_space(Id.mk()))] @ e;
        } else {
          e;
        },
      ]),
    );
  | Dot(e, l) =>
    let* e = go(e)
    and* l =
      switch (l.term) {
      | Label(l') =>
        wrap(
          l,
          label_to_pretty(
            ~label_format=settings.label_format,
            ~label_only_position=true,
            Sort.Exp,
            l',
            l |> Exp.rep_id,
          ),
        )
      | _ => go(l)
      };
    wrap(exp, e @ [mk_form(DotExp, exp |> Exp.rep_id, [])] @ l);
  | Let(p, e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    // This step undoes the adding of fixpoints that happens in elaboration.
    let e1 = settings.hide_fixpoints ? Exp.unfix(e1, p) : e1;
    let+ p = pat_to_pretty(~settings: Settings.t, p)
    and+ e1 = go(e1)
    and+ e2 = go(e2);
    let e2 = settings.inline ? e2 : [Secondary(mk_newline(Id.mk()))] @ e2;
    wrap(exp, [mk_form(Let, id, [p, e1])] @ e2);
  | Theorem(p, thm, e) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ p = pat_to_pretty(~settings: Settings.t, p)
    and+ thm = go(thm)
    and+ e = go(e);
    let e = settings.inline ? e : [Secondary(mk_newline(Id.mk()))] @ e;
    wrap(exp, [mk_form(Theorem, id, [p, thm])] @ e);
  | ProofObject(t) =>
    let id = exp |> Exp.rep_id;
    let+ t = exp_to_pretty(~settings: Settings.t, t);
    wrap(exp, [mk_form(ProofObject, id, [t])]);
  | FixF(p, e, _) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ p = pat_to_pretty(~settings: Settings.t, p)
    and+ e = go(e);
    let name =
      "<" ++ (Exp.get_fn_name(exp) |> Option.value(~default="fun")) ++ ">";
    wrap(
      exp,
      [mk_form(Fix, id, [p])]
      @ e
      |> fold_fun_if(settings.fold_fn_bodies, name, _, exp),
    );
  | TyAlias(tp, t, e) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ tp = tpat_to_pretty(~settings: Settings.t, tp)
    and+ t = typ_to_pretty(~settings: Settings.t, t)
    and+ e = go(e);
    let e = settings.inline ? e : [Secondary(mk_newline(Id.mk()))] @ e;
    wrap(exp, [mk_form(TypeAlias, id, [tp, t])] @ e);
  | Use(t, e) =>
    let id = exp |> Exp.rep_id;
    let+ t = typ_to_pretty(~settings: Settings.t, t)
    and+ e = go(e);
    let e = settings.inline ? e : [Secondary(mk_newline(Id.mk()))] @ e;
    wrap(exp, [mk_form(Use, id, [t])] @ e);
  | Ap(Forward, e1, e2) =>
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    wrap(exp, e1 @ [mk_form(ApExp, id, [e2])]);
  | Ap(Reverse, e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    wrap(
      exp,
      e2
      @ [
        Tile({
          id,
          label: ["|>"],
          mold: Mold.mk_bin(Precedence.eqs, Sort.Exp, []),
          shards: [0],
          children: [],
        }),
      ]
      @ e1,
    );
  | TypAp(e, t) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e = go(e)
    and+ tp = typ_to_pretty(~settings: Settings.t, t);
    wrap(exp, e @ [mk_form(ApExpTyp, id, [tp])]);
  | DeferredAp(e, es) =>
    // TODO: Add optional newlines
    let+ e = go(e)
    and+ es = es |> List.map(go) |> all;
    let (id, ids) = (
      IdTagged.ids(exp) |> List.hd,
      IdTagged.ids(exp) |> List.tl |> pad_ids(List.length(es)),
    );
    wrap(
      exp,
      e
      @ [
        mk_form(
          ApExp,
          id,
          [
            (es |> List.hd)
            @ List.flatten(
                List.map2(
                  (id, e) => [mk_form(CommaExp, id, [])] @ e,
                  ids |> List.tl,
                  es |> List.tl,
                ),
              ),
          ],
        ),
      ],
    );
  | If(e1, e2, e3) =>
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2)
    and+ e3 = go(e3);
    let e2 =
      settings.inline
        ? e2
        : [Secondary(mk_newline(Id.mk()))]
          @ e2
          @ [Secondary(mk_newline(Id.mk()))];
    let e3 = settings.inline ? e3 : [Secondary(mk_newline(Id.mk()))] @ e3;
    wrap(exp, [mk_form(If, id, [e1, e2])] @ e3);
  | Seq(e1, e2) =>
    // TODO: Make newline optional
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    let e2 = settings.inline ? e2 : [Secondary(mk_newline(Id.mk()))] @ e2;
    wrap(exp, e1 @ [mk_form(CellJoin, id, [])] @ e2);
  | Test(e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    wrap(exp, [mk_form(Test, id, [e])]);
  | HintedTest(e, hint) =>
    let id = exp |> Exp.rep_id;
    let* hint = go(hint)
    and* e = go(e);
    wrap(exp, [mk_form(HintedTest, id, [hint, e])]);
  | Parens(e) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    wrap(exp, [mk_form(ParensExp, id, [e])]);
  | Projector({kind, model}, e) =>
    let id = exp |> Exp.rep_id;
    let+ inner_seg = go(e);
    let syntax = Segment.parenthesize(inner_seg);
    wrap(
      exp,
      [Piece.Projector(ProjectorCore.mk(~id, kind, syntax, model))],
    );
  | Cons(e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    wrap(exp, e1 @ [mk_form(ConsExp, id, [])] @ e2);
  | ListConcat(e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    wrap(exp, e1 @ [mk_form(ListConcat, id, [])] @ e2);
  | UnOp(Meta(Unquote), e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    wrap(exp, [mk_form(Unquote, id, [])] @ e);
  | UnOp(Bool(Not), e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    wrap(exp, [mk_form(Not, id, [])] @ e);
  | UnOp(Int(Minus) | Nat(Minus) | Float(Minus) | SInt(Minus), e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    wrap(exp, [mk_form(UnaryMinus, id, [])] @ e);
  /* TODO: this isn't actually correct because we could the builtin
     could have been overriden in this scope; worth fixing when we fix
     closures. */
  | BuiltinFun(f) =>
    wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, f))
  | Asc(e, t) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e)
    and+ t = typ_to_pretty(~settings: Settings.t, t);
    wrap(exp, e @ [mk_form(TypeAsc, id, [])] @ t);
  | Match(e, rs) =>
    /* ID order: [case_end_id] @ rule_ids (outer first, then adopted).
       IMPORTANT: Must align with MakeTerm.exp_term Match case,
       which produces IDs in this order during absorption. */
    let+ e = go(e)
    and+ rs: list((Segment.t, Segment.t)) = {
      rs
      |> List.map(((p, e)) =>
           (pat_to_pretty(~settings: Settings.t, p), go(e))
         )
      |> List.map(((x, y)) => (x, y))
      |> all;
    };
    let (id, ids) = (
      IdTagged.ids(exp) |> List.hd,
      IdTagged.ids(exp) |> List.tl |> pad_ids(List.length(rs)),
    );
    wrap(
      exp,
      [
        mk_form(
          Case,
          id,
          [
            e
            @ (
              List.map2(
                (id, (p, e)) =>
                  (settings.inline ? [] : [Secondary(mk_newline(Id.mk()))])
                  @ [mk_form(Rule, id, [p])]
                  @ (e |> fold_if(settings.fold_case_clauses)),
                ids,
                rs,
              )
              |> List.flatten
            )
            @ (settings.inline ? [] : [Secondary(mk_newline(Id.mk()))]),
          ],
        ),
      ],
    );
  | Module([]) =>
    /* Empty module: {} - output as atomic token like empty tuple () */
    wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "{}"))
  | Module(items) =>
    /* Non-empty module: { item1; item2; ... } */
    let id = exp |> Exp.rep_id;
    let wrap_item = wrap_with_secondary(~secondary=settings.secondary);
    let+ items_pretty =
      items
      |> List.map((item: Mod.t) =>
           switch (item.term) {
           | ModLet(p, e) =>
             let+ p = pat_to_pretty(~settings, p)
             and+ e = go(e);
             wrap_item(
               item,
               [mk_form(ModLet, item |> Mod.rep_id, [p])] @ e,
             );
           | ModType(tp, t) =>
             let+ tp = tpat_to_pretty(~settings, tp)
             and+ t = typ_to_pretty(~settings, t);
             wrap_item(
               item,
               [mk_form(ModType, item |> Mod.rep_id, [tp])] @ t,
             );
           | ModExp(e) =>
             let+ e = go(e);
             wrap_item(item, e);
           | EmptyHole =>
             let item_id = item |> Mod.rep_id;
             p_just(
               wrap_item(
                 item,
                 [
                   Grout({
                     id: item_id,
                     shape: Convex,
                   }),
                 ],
               ),
             );
           | Invalid(s) =>
             p_just(
               wrap_item(
                 item,
                 text_to_pretty(item |> Mod.rep_id, Sort.Mod, s),
               ),
             )
           | MultiHole(es) =>
             let+ es = es |> List.map(any_to_pretty(~settings)) |> all;
             wrap_item(item, List.flatten(es));
           }
         )
      |> all;
    /* Join items with semicolons and wrap in braces */
    let ids =
      IdTagged.ids(exp) |> List.tl |> pad_ids(List.length(items) - 1);
    let body =
      switch (items_pretty) {
      | [] => []
      | [first, ...rest] =>
        first
        @ List.flatten(
            List.map2(
              (semi_id, item) => [mk_form(ModSeq, semi_id, [])] @ item,
              ids,
              rest,
            ),
          )
      };
    wrap(exp, [mk_form(ModBody, id, [body])]);
  };
}
and pat_to_pretty = (~settings: Settings.t, pat: Pat.t): pretty => {
  let go = pat_to_pretty(~settings: Settings.t);
  let wrap = wrap_with_secondary(~secondary=settings.secondary);
  /* Use settings-aware concatenation and form building */
  let (@) = concat_segment(~secondary=settings.secondary);
  let mk_form = mk_form(~secondary=settings.secondary);
  switch (pat |> Pat.term_of) {
  | Invalid(t) => wrap(pat, text_to_pretty(pat |> Pat.rep_id, Sort.Pat, t))
  | EmptyHole =>
    let id = pat |> Pat.rep_id;
    wrap(
      pat,
      p_just([
        Grout({
          id,
          shape: Convex,
        }),
      ]),
    );
  | Wild => wrap(pat, text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "_"))
  | ExplicitNonlabel =>
    wrap(pat, text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "_"))
  | Var(v) => wrap(pat, text_to_pretty(pat |> Pat.rep_id, Sort.Pat, v))
  | Atom(c) =>
    wrap(
      pat,
      text_to_pretty(pat |> Pat.rep_id, Sort.Pat, Atom.to_literal(c)),
    )
  | Constructor(c, _) =>
    wrap(pat, text_to_pretty(pat |> Pat.rep_id, Sort.Pat, c))
  | ListLit([]) =>
    wrap(pat, text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "[]"))
  | ListLit([x, ...xs]) =>
    /* ID order: [bracket_id] @ comma_ids (outer first, then adopted).
       IMPORTANT: Must align with MakeTerm.pat_term ListLit case,
       which produces IDs in this order during absorption. */
    let* x = go(x)
    and* xs = xs |> List.map(go) |> all;
    let (id, ids) = (
      IdTagged.ids(pat) |> List.hd,
      IdTagged.ids(pat) |> List.tl |> pad_ids(List.length(xs)),
    );
    wrap(
      pat,
      p_just([
        mk_form(
          ListLitPat,
          id,
          [
            x
            @ List.flatten(
                List.map2(
                  (id, x) => [mk_form(CommaPat, id, [])] @ x,
                  ids,
                  xs,
                ),
              ),
          ],
        ),
      ]),
    );
  | Cons(p1, p2) =>
    let id = pat |> Pat.rep_id;
    let+ p1 = go(p1)
    and+ p2 = go(p2);
    wrap(pat, p1 @ [mk_form(ConsPat, id, [])] @ p2);
  | Tuple([]) => wrap(pat, text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "()"))
  | Tuple([x, ...xs]) =>
    let+ x = go(x)
    and+ xs = xs |> List.map(go) |> all;
    let ids = IdTagged.ids(pat) |> pad_ids(List.length(xs));
    wrap(
      pat,
      x
      @ List.flatten(
          List.map2((id, x) => [mk_form(CommaPat, id, [])] @ x, ids, xs),
        ),
    );
  | TupLabel(l, p) =>
    let* l =
      switch (l.term) {
      | Label(l') =>
        wrap(
          l,
          label_to_pretty(
            ~label_format=settings.label_format,
            ~label_only_position=true,
            Sort.Pat,
            l',
            l |> Pat.rep_id,
          ),
        )
      | _ => go(l)
      }
    and* p = go(p);
    wrap(
      pat,
      List.flatten([
        l,
        [
          Tile({
            id: pat |> Pat.rep_id,
            label: ["="],
            mold: Mold.mk_bin(Precedence.lab, Sort.Pat, []),
            shards: [0],
            children: [],
          }),
        ],
        if (Token.begins_with_potential_operator(Segment.first_string(p))) {
          [Secondary(mk_space(Id.mk()))] @ p;
        } else {
          p;
        },
      ]),
    );
  | Label(l) =>
    wrap(
      pat,
      text_to_pretty(pat |> Pat.rep_id, Sort.Pat, Token.label_quote(l)),
    )
  | Parens(p) =>
    let id = pat |> Pat.rep_id;
    let+ p = go(p);
    wrap(pat, [mk_form(ParensPat, id, [p])]);
  | Projector({kind, model}, p) =>
    let id = pat |> Pat.rep_id;
    let+ inner_seg = go(p);
    let syntax = Segment.parenthesize(inner_seg);
    wrap(
      pat,
      [Piece.Projector(ProjectorCore.mk(~id, kind, syntax, model))],
    );
  | MultiHole(es) =>
    let+ es = es |> List.map(any_to_pretty(~settings: Settings.t)) |> all;
    /* Use IDs from the term for grout pieces, like Tuple uses for commas. */
    let num_grouts = max(0, List.length(es) - 1);
    let ids = IdTagged.ids(pat) |> pad_ids(num_grouts);
    let seg =
      switch (es) {
      | [] => []
      | [first, ...rest] =>
        first
        @ List.flatten(
            List.map2(
              (id, e) =>
                [
                  Grout({
                    id,
                    shape: Concave,
                  }),
                  ...e,
                ],
              ids,
              rest,
            ),
          )
      };
    wrap(pat, seg);
  | Ap(p1, p2) =>
    let id = pat |> Pat.rep_id;
    let+ p1 = go(p1)
    and+ p2 = go(p2);
    wrap(pat, p1 @ [mk_form(ApPat, id, [p2])]);
  | Asc(p, t) =>
    let id = pat |> Pat.rep_id;
    let+ p = go(p)
    and+ t = typ_to_pretty(~settings: Settings.t, t);
    wrap(pat, p @ [mk_form(Typeann, id, [])] @ t);
  };
}
and typ_to_pretty = (~settings: Settings.t, typ: Typ.t): pretty => {
  let go = typ_to_pretty(~settings: Settings.t);
  let wrap = wrap_with_secondary(~secondary=settings.secondary);
  /* Use settings-aware concatenation and form building */
  let (@) = concat_segment(~secondary=settings.secondary);
  let mk_form = mk_form(~secondary=settings.secondary);
  /* Wrap a segment with secondary from a variant annotation */
  let wrap_variant_secondary =
      (ann: ConstructorMap.variant_ann, seg: Segment.t): Segment.t =>
    switch (settings.secondary) {
    | PreserveExact =>
      let (before, after) = ann.secondary;
      secondary_to_segment(before) @ seg @ secondary_to_segment(after);
    | AutoFormat => seg
    };
  let go_constructor: ConstructorMap.variant(Typ.t) => pretty =
    fun
    | Variant(c, ann, None) => {
        let+ seg =
          text_to_pretty(
            OptUtil.get(() => Id.mk(), ListUtil.hd_opt(ann.ids)),
            Sort.Typ,
            c,
          );
        wrap_variant_secondary(ann, seg);
      }
    | Variant(c, ann, Some(x)) => {
        let+ constructor =
          text_to_pretty(
            OptUtil.get(() => Id.mk(), List.nth_opt(ann.ids, 1)),
            Sort.Typ,
            c,
          );
        wrap_variant_secondary(
          ann,
          constructor
          @ [
            mk_form(
              ApTyp,
              OptUtil.get(() => Id.mk(), ListUtil.hd_opt(ann.ids)),
              [go(x)],
            ),
          ],
        );
      }
    | BadEntry(x) => go(x);
  switch (typ |> Typ.term_of) {
  | Unknown(Hole(Invalid(s))) =>
    wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, s))
  | Unknown(Internal)
  | Unknown(SynSwitch)
  | Unknown(Hole(EmptyHole)) =>
    wrap(
      typ,
      if (settings.show_unknown_as_hole) {
        let id = typ |> Typ.rep_id;
        p_just([
          Grout({
            id,
            shape: Convex,
          }),
        ]);
      } else {
        text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "?");
      },
    )
  | Unknown(Hole(MultiHole(es))) =>
    let+ es = es |> List.map(any_to_pretty(~settings: Settings.t)) |> all;
    /* Use IDs from the term for grout pieces, like Tuple uses for commas. */
    let num_grouts = max(0, List.length(es) - 1);
    let ids = IdTagged.ids(typ) |> pad_ids(num_grouts);
    let seg =
      switch (es) {
      | [] => []
      | [first, ...rest] =>
        first
        @ List.flatten(
            List.map2(
              (id, e) =>
                [
                  Grout({
                    id,
                    shape: Concave,
                  }),
                  ...e,
                ],
              ids,
              rest,
            ),
          )
      };
    wrap(typ, seg);
  | Var(v) => wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, v))
  | Atom(Int) =>
    wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Int"))
  | Atom(SInt) =>
    wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "SInt"))
  | Atom(Float) =>
    wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Float"))
  | Atom(Bool) =>
    wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Bool"))
  | Atom(String) =>
    wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "String"))
  | Atom(Nat) =>
    wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Nat"))
  | List(t) =>
    let id = typ |> Typ.rep_id;
    let+ t = go(t);
    wrap(typ, [mk_form(ListTyp, id, [t])]);
  | Prod([]) => wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "()"))
  | Prod([t, ...ts]) =>
    let+ t = go(t)
    and+ ts = ts |> List.map(go) |> all;
    wrap(
      typ,
      t
      @ List.flatten(
          List.map2(
            (id, t) => [mk_form(CommaTyp, id, [])] @ t,
            IdTagged.ids(typ) |> pad_ids(ts |> List.length),
            ts,
          ),
        ),
    );
  | ExplicitNonlabel =>
    wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "_"))
  | Label(l) =>
    wrap(
      typ,
      text_to_pretty(typ |> Typ.rep_id, Sort.Typ, Token.label_quote(l)),
    )
  | TupLabel(l, t) =>
    let+ l =
      switch (l.term) {
      | Label(l') =>
        wrap(
          l,
          label_to_pretty(
            ~label_format=settings.label_format,
            ~label_only_position=true,
            Sort.Typ,
            l',
            l |> Typ.rep_id,
          ),
        )
      | _ => go(l)
      }
    and+ t = go(t);

    wrap(
      typ,
      List.flatten([
        l,
        [
          Tile({
            id: typ |> Typ.rep_id,
            label: ["="],
            mold: Mold.mk_bin(Precedence.lab, Sort.Typ, []),
            shards: [0],
            children: [],
          }),
        ],
        if (Token.begins_with_potential_operator(Segment.first_string(t))) {
          [Secondary(mk_space(Id.mk()))] @ t;
        } else {
          t;
        },
      ]),
    );
  | ProdProjection(t1, t2) =>
    let* t1 = go(t1)
    and* t2 =
      switch (t2.term) {
      | Label(l') =>
        wrap(
          t2,
          label_to_pretty(
            ~label_format=settings.label_format,
            ~label_only_position=true,
            Sort.Typ,
            l',
            t2 |> Typ.rep_id,
          ),
        )
      | _ => go(t2)
      };
    wrap(typ, t1 @ [mk_form(ProdProjection, typ |> Typ.rep_id, [])] @ t2);
  | ProdExtension(t1, t2) =>
    let+ t1 = go(t1)
    and+ t2 = go(t2);
    wrap(typ, t1 @ [mk_form(ProdExtension, typ |> Typ.rep_id, [])] @ t2);
  | Parens(t) =>
    let id = typ |> Typ.rep_id;
    let+ t = go(t);
    wrap(typ, [mk_form(ParensTyp, id, [t])]);
  | Projector({kind, model}, t) =>
    let id = typ |> Typ.rep_id;
    let+ inner_seg = go(t);
    let syntax = Segment.parenthesize(inner_seg);
    wrap(
      typ,
      [Piece.Projector(ProjectorCore.mk(~id, kind, syntax, model))],
    );
  | Rec(tp, t) =>
    let id = typ |> Typ.rep_id;
    let+ tp = tpat_to_pretty(~settings: Settings.t, tp)
    and+ t = go(t);
    wrap(typ, [mk_form(Rec, id, [tp])] @ t);
  | Poly(tp, t) =>
    let id = typ |> Typ.rep_id;
    let+ tp = tpat_to_pretty(~settings: Settings.t, tp)
    and+ t = go(t);
    wrap(typ, [mk_form(Poly, id, [tp])] @ t);
  | ProofOf(e) =>
    let id = typ |> Typ.rep_id;
    let+ e = exp_to_pretty(~settings, e);
    wrap(typ, [mk_form(ProofOf, id, [e])]);
  | Arrow(t1, t2) =>
    let id = typ |> Typ.rep_id;
    let+ t1 = go(t1)
    and+ t2 = go(t2);
    wrap(typ, t1 @ [mk_form(TypeArrow, id, [])] @ t2);
  | Sum([]) => failwith("Empty Sums are not allowed")
  | Sum([t]) =>
    let id = typ |> Typ.rep_id;
    let+ t = go_constructor(t);
    wrap(typ, [mk_form(TypSumSingle, id, [])] @ t);
  | Sum([t, ...ts]) =>
    let ids = IdTagged.ids(typ) |> pad_ids(List.length(ts) + 1);
    let id = List.hd(ids);
    let ids = List.tl(ids);
    let+ t = go_constructor(t)
    and+ ts = ts |> List.map(go_constructor) |> all;
    wrap(
      typ,
      [mk_form(TypSumSingle, id, [])]
      @ t
      @ List.flatten(
          List.map2((id, t) => [mk_form(TypPlus, id, [])] @ t, ids, ts),
        ),
    );
  | Sig([]) =>
    /* Empty sig: {} */
    wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "{}"))
  | Sig(items) =>
    /* Non-empty sig: { let x : Int; type T = Bool; ... } */
    let id = typ |> Typ.rep_id;
    let wrap_item = wrap_with_secondary(~secondary=settings.secondary);
    let+ items_pretty =
      items
      |> List.map((item: Sig.t) =>
           switch (item.term) {
           | SigLet(p) =>
             let+ p = pat_to_pretty(~settings, p);
             wrap_item(item, [mk_form(SigLet, item |> Sig.rep_id, [])] @ p);
           | SigType(tp, t) =>
             let+ tp = tpat_to_pretty(~settings, tp)
             and+ t = go(t);
             wrap_item(
               item,
               [mk_form(SigType, item |> Sig.rep_id, [tp])] @ t,
             );
           | EmptyHole =>
             let item_id = item |> Sig.rep_id;
             p_just(
               wrap_item(
                 item,
                 [
                   Grout({
                     id: item_id,
                     shape: Convex,
                   }),
                 ],
               ),
             );
           | Invalid(s) =>
             p_just(
               wrap_item(
                 item,
                 text_to_pretty(item |> Sig.rep_id, Sort.Sig, s),
               ),
             )
           | MultiHole(es) =>
             let+ es = es |> List.map(any_to_pretty(~settings)) |> all;
             wrap_item(item, List.flatten(es));
           }
         )
      |> all;
    /* Join items with semicolons and wrap in braces */
    let ids =
      IdTagged.ids(typ) |> List.tl |> pad_ids(List.length(items) - 1);
    let body =
      switch (items_pretty) {
      | [] => []
      | [first, ...rest] =>
        first
        @ List.flatten(
            List.map2(
              (semi_id, item) => [mk_form(SigSeq, semi_id, [])] @ item,
              ids,
              rest,
            ),
          )
      };
    wrap(typ, [mk_form(SigBody, id, [body])]);
  };
}
and tpat_to_pretty = (~settings: Settings.t, tpat: TPat.t): pretty => {
  let wrap = wrap_with_secondary(~secondary=settings.secondary);
  /* Use settings-aware concatenation and form building */
  switch (tpat |> IdTagged.term_of) {
  | Invalid(t) =>
    wrap(tpat, text_to_pretty(tpat |> TPat.rep_id, Sort.TPat, t))
  | EmptyHole =>
    let id = tpat |> TPat.rep_id;
    wrap(
      tpat,
      p_just([
        Grout({
          id,
          shape: Convex,
        }),
      ]),
    );
  | MultiHole(xs) =>
    let+ xs = xs |> List.map(any_to_pretty(~settings: Settings.t)) |> all;
    /* Use IDs from the term for grout pieces, like Tuple uses for commas.
       For N elements, we need N-1 grout pieces (one between each pair). */
    let num_grouts = max(0, List.length(xs) - 1);
    let ids = IdTagged.ids(tpat) |> pad_ids(num_grouts);
    let seg =
      switch (xs) {
      | [] => []
      | [first, ...rest] =>
        first
        @ List.flatten(
            List.map2(
              (id, x) =>
                [
                  Grout({
                    id,
                    shape: Concave,
                  }),
                  ...x,
                ],
              ids,
              rest,
            ),
          )
      };
    wrap(tpat, seg);
  | Var(v) => wrap(tpat, text_to_pretty(tpat |> TPat.rep_id, Sort.TPat, v))
  };
}
and any_to_pretty = (~settings: Settings.t, any: Any.t): pretty => {
  switch (any) {
  | Exp(e) => exp_to_pretty(~settings: Settings.t, e)
  | Pat(p) => pat_to_pretty(~settings: Settings.t, p)
  | Typ(t) => typ_to_pretty(~settings: Settings.t, t)
  | TPat(tp) => tpat_to_pretty(~settings: Settings.t, tp)
  | Mod(_)
  | Sig(_)
  | Any(_)
  | Rul(_) =>
    //TODO: print out invalid rules/modules properly
    let id = any |> Any.rep_id;
    p_just([
      Grout({
        id,
        shape: Convex,
      }),
    ]);
  };
}
and label_to_pretty =
    (
      ~label_format: Settings.label_format,
      ~label_only_position,
      sort: Sort.t,
      label: string,
      id: Uuidm.t,
    )
    : pretty => {
  text_to_pretty(
    id,
    sort,
    if (label_only_position) {
      switch (label_format) {
      | QuoteWhenNecessary => Token.quote_label_when_necessary(label)
      | AlwaysQuote => Token.label_quote(label)
      };
    } else {
      label;
    },
  );
};

let exp_to_segment =
    (~already_paren=false, ~settings: Settings.t, exp: Exp.t): Segment.t => {
  let exp =
    exp
    |> parenthesize(
         ~parenthesization=settings.parenthesization,
         ~already_paren,
         ~show_filters=settings.show_filters,
       );
  let p = exp_to_pretty(~settings, exp);
  p |> PrettySegment.select;
};

let typ_to_segment = (~settings: Settings.t, typ: Typ.t): Segment.t => {
  /* Desugar Sig types to labeled tuples so they display as (x=Int, y=Bool)
     instead of {sig}. Uses empty ctx since we're just displaying. */
  let typ = Typ.desugar_sig(Ctx.empty, typ);
  let typ =
    typ
    |> parenthesize_typ(
         ~parenthesization=settings.parenthesization,
         ~show_filters=settings.show_filters,
       );
  let p = typ_to_pretty(~settings, typ);
  p |> PrettySegment.select;
};

let any_to_segment =
    (~already_paren=false, ~settings: Settings.t, any: Any.t): Segment.t => {
  let any =
    any
    |> parenthesize_any(
         ~parenthesization=settings.parenthesization,
         ~already_paren,
         ~show_filters=settings.show_filters,
       );
  let p = any_to_pretty(~settings, any);
  p |> PrettySegment.select;
};
