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
    project_tables: bool,
    hide_fixpoints: bool,
    show_filters: bool,
    show_ascriptions: bool,
    show_unknown_as_hole: bool,
    /* Emit recorded surface spellings of literals/labels (007, 1e3, `a`).
       On for code display and roundtripping; off for result views, where
       values print canonically (evaluation copies drop lexemes anyway,
       so keeping them would render results inconsistently). Hole and
       unknown-operator lexemes are always emitted. */
    use_literal_lexemes: bool,
    /* Emit lexeme-less EmptyHole as an explicit "?" TILE instead of Grout —
       used by FastParse so a source `?` lands as the tile the typing parser
       would make, keeping explicit `?` holes distinct from Grout
       (whose text serialization is the `¿` marker). A recorded hole
       lexeme still wins: it IS the source spelling. */
    hole_tiles: bool,
  };

  let of_core = (~inline, ~fold_fn_bodies=?, settings: CoreSettings.t) => {
    secondary: AutoFormat,
    use_literal_lexemes: false,
    parenthesization: Defensive,
    label_format: QuoteWhenNecessary,
    inline,
    show_ascriptions: settings.evaluation.show_ascriptions,
    fold_case_clauses: !settings.evaluation.show_case_clauses,
    project_tables: settings.evaluation.project_tables,
    fold_fn_bodies:
      fold_fn_bodies
      |> Option.value(
           ~default=settings.evaluation.show_fn_bodies ? `NoFold : `Fold,
         ),
    hide_fixpoints: !settings.evaluation.show_fixpoints,
    show_filters: settings.evaluation.show_stepper_filters,
    show_unknown_as_hole: true,
    hole_tiles: false,
  };

  let editable = (~inline) => {
    {
      secondary: AutoFormat,
      use_literal_lexemes: true,
      parenthesization: Defensive,
      label_format: QuoteWhenNecessary,
      inline,
      fold_case_clauses: false,
      project_tables: false,
      fold_fn_bodies: `NoFold,
      show_ascriptions: true,
      hide_fixpoints: false,
      show_filters: true,
      show_unknown_as_hole: true,
      hole_tiles: false,
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

/* Lexeme validation: annotations may carry the surface spelling of
   single-token terms (IdTag.lexeme). Use it verbatim only when it still
   denotes the term\'s value — terms rebuilt with stale annotations must
   never misprint. */
let hole_lexeme = (ann: IdTagged.IdTag.t): option(string) =>
  switch (ann.lexeme) {
  | Some(l)
      when
        Token.is_explicit_hole(l)
        || Token.is_llm_hole(l)
        || Token.is_implicit_hole_marker(l) =>
    Some(l)
  | _ => None
  };

let op_lexeme = (ann: IdTagged.IdTag.t): option(string) =>
  switch (ann.lexeme) {
  | Some(l)
      when
        Token.is_potential_operator(l)
        || Form.is_infix_delimiter_op_prefix(l) =>
    Some(l)
  | _ => None
  };

/* Reconstruct an unknown-op tile from its lexeme, mirroring insertion
   molding: a DEFINED token stranded in an unparseable position keeps
   its defined bin mold (the : of `? : ? t ?` keeps Cast's mold);
   operand-shaped keyword prefixes get the concave-grout bin mold of
   the host sort (Form.get_atomic_form); truly unknown ops the
   Any-sorted max-precedence fallback (Form.Molds.get). */
/* Insertion-time sort context is unrecorded, so approximate: host
   sort, then Exp, then the undefined-token fallbacks. Residual sort
   drift is absorbed by equiv_mod_grout(~mold_sorts=false). */
let op_tile_with = (~shape_filter, ~fallback, id, sort, op): Piece.t => {
  let cands = Form.Molds.get_base([op]) |> List.filter(shape_filter);
  let at_sort = srt => List.filter((m: Mold.t) => m.out == srt, cands);
  let mold =
    switch (at_sort(sort), at_sort(Sort.Exp)) {
    | ([m, ..._], _)
    | ([], [m, ..._]) => m
    | ([], []) => fallback
    };
  Tile({
    id,
    label: [op],
    mold,
    shards: [0],
    children: [],
  });
};

let op_tile = (id, sort, op): Piece.t =>
  op_tile_with(
    ~shape_filter=Mold.is_infix_op,
    ~fallback=
      Form.is_infix_delimiter_op_prefix(op)
        ? Mold.mk_bin(Precedence.concave_grout, sort, [])
        : Mold.mk_bin(Precedence.max, Any, []),
    id,
    sort,
    op,
  );

/* Stranded prefix op (the unary - of `? : - ?` on the typ side) */
let pre_op_tile = (id, sort, op): Piece.t =>
  op_tile_with(
    ~shape_filter=Mold.is_prefix_op,
    ~fallback=Mold.mk_pre(Precedence.max, Any, []),
    id,
    sort,
    op,
  );

let quoted_label_lexeme =
    (~settings: Settings.t, ann: IdTagged.IdTag.t, label: string): string =>
  switch (settings.use_literal_lexemes ? ann.lexeme : None) {
  | Some(l)
      when
        Token.is_quoted_label(l)
        && Token.strip_quotes(~quote=Token.label_delim, l) == label => l
  | _ => Token.label_quote(label)
  };

let atom_lexeme =
    (~settings: Settings.t, ann: IdTagged.IdTag.t, c: Atom.t): string => {
  let canonical = Atom.to_literal(c);
  switch (settings.use_literal_lexemes ? ann.lexeme : None) {
  | Some(l) =>
    let valid =
      switch (c) {
      | Int(i) =>
        Token.is_int(l)
        && (
          switch (Bigint.of_string_opt(l)) {
          | Some(v) => Bigint.to_string(v) == Bigint.to_string(i)
          | None => false
          }
        )
      | Float(f) =>
        Token.is_float(l)
        && (
          switch (float_of_string_opt(l)) {
          | Some(v) => v == f
          | None => false
          }
        )
      | _ => false
      };
    valid ? l : canonical;
  | None => canonical
  };
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
  | DrvQuote(_)
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
  | Tuple([]) => Precedence.max // the atomic () token (#2296)
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
  | ModuleExp(_) => Precedence.let_
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
  | Tuple([]) => Precedence.max // the atomic () token (#2296)
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
  | DrvQuoteTy(_)
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
  | Prod([]) => Precedence.max // the atomic () unit-type token (#2296)
  | Prod(_) => Precedence.comma
  | Arrow(_, _) => Precedence.type_arrow
  /* Empty sum prints as the atomic token Void. */
  | Sum([]) => Precedence.max
  /* Loosest, deliberately: a bare sum reparses differently in most
     operator slots (Arrow(Sum(..), t) printed bare comes back as the
     sum swallowing the arrow — caught by the Menhir fuzz property), so
     Defensive mode always parenthesizes sums in operand position. */
  | Sum(_) => Precedence.min
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
          ~show_ascriptions: bool,
          ~already_paren=false,
          exp: Exp.t,
        )
        : Exp.t => {
  let parenthesize =
    parenthesize(~parenthesization, ~show_filters, ~show_ascriptions);
  let parenthesize_pat =
    parenthesize_pat(~parenthesization, ~show_filters, ~show_ascriptions);
  let parenthesize_typ =
    parenthesize_typ(~parenthesization, ~show_filters, ~show_ascriptions);
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
  | DrvQuote(_)
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
    TupLabel(l, parenthesize(e) |> paren_at(Precedence.comma)) |> rewrap
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
      parenthesize(e1) |> paren_at(Precedence.eqs), // Associativity is backwards because e2 goes before e1
      parenthesize(e2) |> paren_assoc_at(Precedence.eqs),
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
  | Asc(e, t) when show_ascriptions =>
    Asc(
      parenthesize(e) |> paren_assoc_at(Precedence.asc),
      parenthesize_typ(t) |> paren_typ_at(Precedence.asc),
    )
    |> rewrap
  | Asc(e, _) => parenthesize(e) // skip ascription if not showing
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
  | UnOp(Bool(Not), e) =>
    UnOp(Bool(Not), parenthesize(e) |> paren_at(Precedence.not_)) |> rewrap
  | UnOp(Float(Minus), e) =>
    /* Rewrite float negation as 0.0 -. e so that the segment round-trips
       correctly through MakeTerm (which parses unary - as Int(Minus)) */
    let zero = Exp.fresh(Atom(Float(0.0)));
    BinOp(
      Float(Minus),
      parenthesize(zero) |> paren_assoc_at(Precedence.plus),
      parenthesize(e) |> paren_at(Precedence.plus),
    )
    |> rewrap;
  | UnOp((Int(Minus) | Nat(Minus) | SInt(Minus)) as op, e) =>
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
      List.map(
        parenthesize_any(~parenthesization, ~show_ascriptions, ~show_filters),
        xs,
      ),
    )
    |> rewrap
  | Module(_) => exp /* Phase 1.2: proper module parenthesization */
  | ModuleExp(_) => exp
  };
}
and parenthesize_pat =
    (
      ~parenthesization: Settings.parenthesization,
      ~show_filters: bool,
      ~show_ascriptions: bool,
      ~already_paren=false,
      pat: Pat.t,
    )
    : Pat.t => {
  let parenthesize_pat =
    parenthesize_pat(~parenthesization, ~show_filters, ~show_ascriptions);
  let parenthesize_typ =
    parenthesize_typ(~parenthesization, ~show_filters, ~show_ascriptions);
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
    TupLabel(l, parenthesize_pat(p) |> paren_pat_at(Precedence.comma))
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
      List.map(
        parenthesize_any(~parenthesization, ~show_ascriptions, ~show_filters),
        xs,
      ),
    )
    |> rewrap
  | Asc(p, t) when show_ascriptions =>
    Asc(
      parenthesize_pat(p) |> paren_pat_assoc_at(Precedence.asc),
      parenthesize_typ(t) |> paren_typ_at(Precedence.max) // Hack[Matt]: always add parens to get the arrows right
    )
    |> rewrap
  | Asc(p, _) => parenthesize_pat(p) // skip ascription if not showing
  };
}

and parenthesize_typ =
    (
      ~parenthesization: Settings.parenthesization,
      ~show_filters: bool,
      ~show_ascriptions: bool,
      ~already_paren=false,
      typ: Typ.t,
    )
    : Typ.t => {
  let parenthesize_typ =
    parenthesize_typ(~parenthesization, ~show_filters, ~show_ascriptions);
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
  | Atom(_)
  | DrvQuoteTy(_) => typ

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
    TupLabel(l, parenthesize_typ(t) |> paren_typ_at(Precedence.type_prod))
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
      parenthesize(~parenthesization, ~show_ascriptions, ~show_filters, e)
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
          List.map(
            parenthesize_any(
              ~parenthesization,
              ~show_ascriptions,
              ~show_filters,
            ),
            xs,
          ),
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
      ~show_ascriptions: bool,
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
      List.map(
        parenthesize_any(~parenthesization, ~show_ascriptions, ~show_filters),
        xs,
      ),
    )
    |> rewrap
  };
}

and parenthesize_rul =
    (
      ~parenthesization: Settings.parenthesization,
      ~show_ascriptions: bool,
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
      parenthesize(~parenthesization, ~show_ascriptions, ~show_filters, e),
      List.map(
        ((p, e)) =>
          (
            parenthesize_pat(
              ~parenthesization,
              ~show_ascriptions,
              ~show_filters,
              p,
            ),
            parenthesize(
              ~parenthesization,
              ~show_ascriptions,
              ~show_filters,
              e,
            ),
          ),
        ps,
      ),
    )
    |> rewrap
  | MultiHole(xs) =>
    MultiHole(
      List.map(
        parenthesize_any(~parenthesization, ~show_ascriptions, ~show_filters),
        xs,
      ),
    )
    |> rewrap
  };
}

and parenthesize_any =
    (
      ~parenthesization: Settings.parenthesization,
      ~already_paren=false,
      ~show_filters: bool,
      ~show_ascriptions: bool,
      any: Any.t,
    )
    : Any.t =>
  switch (any) {
  | Exp(e) =>
    Exp(
      parenthesize(
        ~parenthesization,
        ~already_paren,
        ~show_ascriptions,
        ~show_filters,
        e,
      ),
    )
  | Pat(p) =>
    Pat(
      parenthesize_pat(
        ~parenthesization,
        ~already_paren,
        ~show_ascriptions,
        ~show_filters,
        p,
      ),
    )
  | Typ(t) =>
    Typ(
      parenthesize_typ(
        ~parenthesization,
        ~already_paren,
        ~show_ascriptions,
        ~show_filters,
        t,
      ),
    )
  | TPat(tp) =>
    TPat(
      parenthesize_tpat(
        ~parenthesization,
        ~show_ascriptions,
        ~show_filters,
        tp,
      ),
    )
  | Rul(r) =>
    Rul(
      parenthesize_rul(
        ~parenthesization,
        ~show_ascriptions,
        ~show_filters,
        r,
      ),
    )
  /* Parenthesization for Drv, Mod, Sig, MPat, and Any is not yet defined.
     Pretty-printing produces the term as-is; this is sound (never adds
     invalid parens) but may omit disambiguating parens in nested contexts. */
  | Drv(_) => any
  | Mod(_) => any
  | Sig(_) => any
  | MPat(_) => any
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
  | _ when String.ends_with(s1, ~suffix="::") => true
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
  | _
      when
        s1 == "."
        && (
          Token.is_quoted_label(s2) || Token.is_var(s2) || Token.is_ctr(s2)
        ) =>
    false
  | _
      when
        s2 == "."
        && (
          Token.is_quoted_label(s1)
          || Token.is_var(s1)
          || Token.is_ctr(s1)
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

/* Invalid tokens reprint with the mold the editor leaves on them:
   sort-matched if defined, else the token's base mold (a wrong-sort
   literal keeps it), else the undefined-token fallback. */
let invalid_to_pretty = (id, sort, str): pretty => {
  let mold =
    switch (Form.Molds.get_base([str])) {
    | [] => Form.Molds.get(sort, [str])
    | base =>
      switch (List.filter((m: Mold.t) => m.out == sort, base)) {
      | [m, ..._] => m
      | [] => List.hd(base)
      }
    };
  p_just([
    Tile({
      id,
      label: [str],
      mold,
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
   evaluation only ever gives them one.

   Some upstream producers (e.g., evaluator collapse, certain absorption
   paths) can emit ids lists with duplicates — e.g., [case_id, case_id, ...]
   for a Match where the adoption machinery did not preserve distinct rule
   ids. If we pass duplicates through unchanged, the pretty-printer will
   emit multiple Tile pieces sharing the same id (e.g., the case `[case;end]`
   form and all `[|;=>]` rules all tagged with case_id), and
   Segment.reassemble will group them into a single Aba match and fail
   with an out-of-order combined_shards assertion.

   To prevent that, pad_ids now also ensures the returned list has:
   1. no duplicates within itself;
   2. no id equal to any id in [~forbidden].

   Padding and replacement ids are DERIVED (hash of ~base + counter),
   not minted: printing must be a pure function of the term. Fresh ids
   here made double-prints of the same term differ. ~base defaults to
   the first id; pass it explicitly where ids can be empty. */
let pad_ids =
    (
      ~forbidden: list(Id.t)=[],
      ~base: option(Id.t)=?,
      n: int,
      ids: list(Id.t),
    )
    : list(Id.t) => {
  let base =
    switch (base, ids) {
    | (Some(b), _) => b
    | (None, [id, ..._]) => id
    | (None, []) => Id.invalid
    };
  let counter = ref(0);
  let forbidden_set = ref(Id.Set.of_list(forbidden));
  let rec derived = () => {
    incr(counter);
    let cand = Id.derive(~salt="pad" ++ string_of_int(counter^), base);
    Id.Set.mem(cand, forbidden_set^) ? derived() : cand;
  };
  let replace = id =>
    if (Id.Set.mem(id, forbidden_set^)) {
      let fresh = derived();
      forbidden_set := Id.Set.add(fresh, forbidden_set^);
      fresh;
    } else {
      forbidden_set := Id.Set.add(id, forbidden_set^);
      id;
    };
  let truncated =
    if (List.length(ids) < n) {
      ids @ List.init(n - List.length(ids), _ => derived());
    } else {
      ListUtil.split_n(n, ids) |> fst;
    };
  List.map(replace, truncated);
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

let project_table_if = (should_project, pieces) =>
  if (should_project) {
    switch (MakeTerm.for_projection([pieces])) {
    | None => [pieces]
    | Some(any) => [ProjectorInit.init_or_noop(Table, pieces, any)]
    };
  } else {
    [pieces];
  };

let rec drv_exp_to_pretty =
        (~settings: Settings.t, syntax: Drv.Exp.t, ~sort: DrvSort.t): pretty => {
  let mk_form = mk_form(~secondary=settings.secondary);
  let go = (~inline=settings.inline, ~sort) =>
    drv_exp_to_pretty(
      ~settings={
        ...settings,
        inline,
      },
      ~sort,
    );
  let try_newline = () =>
    settings.inline ? [] : [Secondary(mk_newline(Id.mk()))];
  let id = syntax |> Drv.Exp.rep_id;
  let content =
    switch (syntax |> Drv.Exp.term_of) {
    | Hole(h) => drv_type_hole_to_pretty(~settings, h)
    | Quote(e) => text_to_pretty(id, Sort.Drv(sort), "$" ++ e)
    | Parens(e) =>
      let+ e = go(e, ~sort);
      [mk_form(Drv(ParenExp), id, [e])];
    /* [Tuple] is an intermediate form produced during parsing; it should be
       collapsed into a parenthesised [Pair] before reaching the pretty printer,
       so hitting it here indicates a bug somewhere upstream. */
    | Tuple(_) => text_to_pretty(id, Sort.Drv(sort), "[Tuple]")
    | Cons(e1, e2) =>
      let+ e1 = go(e1, ~sort=Prop)
      and+ e2 = go(e2, ~sort=Ctx);
      e1 @ [mk_form(Drv(Cons), id, [])] @ e2;
    | Concat(e1, e2) =>
      let+ e1 = go(e1, ~sort=Ctx)
      and+ e2 = go(e2, ~sort=Ctx);
      e1 @ [mk_form(Drv(Concat), id, [])] @ e2;
    | And(l, r) =>
      let+ l = go(l, ~sort=Prop)
      and+ r = go(r, ~sort=Prop);
      l @ [mk_form(Drv(And), id, [])] @ r;
    | Or(l, r) =>
      let+ l = go(l, ~sort=Prop)
      and+ r = go(r, ~sort=Prop);
      l @ [mk_form(Drv(Or), id, [])] @ r;
    | Impl(l, {term: Falsity, _}) =>
      let+ l = go(l, ~sort=Prop);
      [mk_form(Drv(Not), id, [])] @ l;
    | Impl(l, r) =>
      let+ l = go(l, ~sort=Prop)
      and+ r = go(r, ~sort=Prop);
      l @ [mk_form(Drv(Impl), id, [])] @ r;
    | Truth => text_to_pretty(id, Sort.Drv(Prop), "Truth")
    | Falsity => text_to_pretty(id, Sort.Drv(Prop), "Falsity")
    | Ctx([]) => text_to_pretty(id, Sort.Drv(Ctx), "[]")
    | Ctx([x, ...xs]) =>
      let* x = go(x, ~sort=Prop)
      and* xs = xs |> List.map(go(~sort=Prop)) |> all;
      let ids =
        syntax
        |> IdTagged.ids
        |> List.tl
        |> pad_ids(~base=id, List.length(xs));
      let map2_safe = (f, l1, l2) =>
        List.length(l1) == List.length(l2)
          ? List.map2(f, l1, l2) : raise(Invalid_argument("map2_safe"));
      [
        mk_form(
          Drv(List),
          id,
          [
            x
            @ List.flatten(
                map2_safe(
                  (id, x) => [mk_form(Drv(CommaExp), id, [])] @ x,
                  ids,
                  xs,
                ),
              ),
          ],
        ),
      ];
    | Val(v) =>
      let+ v = go(v, ~sort=Exp);
      [mk_form(Drv(Val), id, [v])];
    | Eval(l, r) =>
      let+ l = go(l, ~sort=Exp)
      and+ r = go(r, ~sort=Exp);
      l @ [mk_form(Drv(Eval), id, [])] @ r;
    | Entail({term: Ctx([]), _}, r) =>
      let+ r = go(r, ~sort=Prop);
      [mk_form(Drv(UnaryEntail), id, [])] @ r;
    | Entail(l, r) =>
      let+ l = go(l, ~sort=Ctx)
      and+ r = go(r, ~sort=Prop);
      l @ [mk_form(Drv(Entail), id, [])] @ r;
    | Consistent(l, r) =>
      let+ l = drv_typ_to_pretty(~settings, l)
      and+ r = drv_typ_to_pretty(~settings, r);
      [mk_form(Drv(Consistent), id, [l])] @ r;
    | MatchedArrow(l, r) =>
      let+ l = drv_typ_to_pretty(~settings, l)
      and+ r = drv_typ_to_pretty(~settings, r);
      [mk_form(Drv(MatchedArrow), id, [l])] @ r;
    | MatchedProd(l, r) =>
      let+ l = drv_typ_to_pretty(~settings, l)
      and+ r = drv_typ_to_pretty(~settings, r);
      [mk_form(Drv(MatchedProd), id, [l])] @ r;
    | MatchedSum(l, r) =>
      let+ l = drv_typ_to_pretty(~settings, l)
      and+ r = drv_typ_to_pretty(~settings, r);
      [mk_form(Drv(MatchedSum), id, [l])] @ r;
    | Type(t) =>
      let+ t = drv_typ_to_pretty(~settings, t);
      [mk_form(Drv(Valid), id, [t])];
    | HasType(e, t) =>
      let+ e = go(e, ~sort=Exp)
      and+ t = drv_typ_to_pretty(~settings, t);
      e @ [mk_form(Drv(HasType), id, [])] @ t;
    | Syn(e, t) =>
      let+ e = go(e, ~sort=Exp)
      and+ t = drv_typ_to_pretty(~settings, t);
      e @ [mk_form(Drv(Syn), id, [])] @ t;
    | Ana(e, t) =>
      let+ e = go(e, ~sort=Exp)
      and+ t = drv_typ_to_pretty(~settings, t);
      e @ [mk_form(Drv(Ana), id, [])] @ t;
    | Var(x) => text_to_pretty(id, Sort.Drv(sort), x)
    | NumLit(n) => text_to_pretty(id, Sort.Drv(Exp), Int.to_string(n))
    | Neg(e) =>
      let+ e = go(e, ~sort=Exp);
      [mk_form(Drv(Neg), id, [])] @ e;
    | BinOp(op, l, r) =>
      let+ l = go(l, ~sort=Exp)
      and+ r = go(r, ~sort=Exp);
      let cls: Form.drv_compound_form =
        switch (op) {
        | Plus => Plus
        | Minus => Minus
        | Times => Times
        | Eq => Eq
        | Gt => Gt
        | Lt => Lt
        };
      l @ [mk_form(Drv(cls), id, [])] @ r;
    | True => text_to_pretty(id, Sort.Drv(Exp), "True")
    | False => text_to_pretty(id, Sort.Drv(Exp), "False")
    | If(c, t, f) =>
      let+ c = go(c, ~sort=Exp)
      and+ t = go(t, ~sort=Exp)
      and+ f = go(f, ~sort=Exp);
      [mk_form(Drv(If), id, [c, t])] @ f;
    | Let(p, e1, e2) =>
      let+ p = drv_pat_to_pretty(~settings, p)
      and+ e1 = go(e1, ~sort=Exp)
      and+ e2 = go(e2, ~sort=Exp);
      [mk_form(Drv(Let), id, [p, e1])] @ e2;
    | Fix(p, e) =>
      let+ p = drv_pat_to_pretty(~settings, p)
      and+ e = go(e, ~sort=Exp);
      [mk_form(Drv(Fix), id, [p])] @ e;
    | Fun(p, e) =>
      let+ p = drv_pat_to_pretty(~settings, p)
      and+ e = go(e, ~sort=Exp);
      [mk_form(Drv(Fun), id, [p])] @ e;
    | Ap(l, r) =>
      let+ l = go(l, ~sort=Exp)
      and+ r = go(r, ~sort=Exp);
      l @ [mk_form(Drv(ApExp), id, [r])];
    | Pair(l, r) =>
      let+ l = go(l, ~sort=Exp)
      and+ r = go(r, ~sort=Exp);
      [
        mk_form(
          Drv(ParenExp),
          id,
          [l @ [mk_form(Drv(CommaExp), id, [])] @ r],
        ),
      ];
    | Triv => text_to_pretty(id, Sort.Drv(Exp), "()")
    | PrjL(e) =>
      let+ e = go(e, ~sort=Exp);
      e
      @ [mk_form(Drv(Dot), Id.invalid, [])]
      @ text_to_pretty(id, Sort.Drv(Exp), "fst");
    | PrjR(e) =>
      let+ e = go(e, ~sort=Exp);
      e
      @ [mk_form(Drv(Dot), Id.invalid, [])]
      @ text_to_pretty(id, Sort.Drv(Exp), "snd");
    | InjL(e) =>
      let+ e = go(e, ~sort=Exp);
      text_to_pretty(Id.invalid, Sort.Drv(Exp), "L")
      @ [mk_form(Drv(ApExp), id, [e])];
    | InjR(e) =>
      let+ e = go(e, ~sort=Exp);
      text_to_pretty(Id.invalid, Sort.Drv(Exp), "R")
      @ [mk_form(Drv(ApExp), id, [e])];
    | Case(e, x, e1, y, e2) =>
      /* ID order: [case_end_id] @ rule_ids (outer first, then adopted).
         IMPORTANT: Each Rule tile must have its OWN distinct id, not the
         same as the outer Case, otherwise Segment.reassemble will group
         all shards with the same id into one Aba match, producing
         out-of-order combined_shards and an assertion failure. */
      let+ e = go(e, ~sort=Exp)
      and+ x = drv_pat_to_pretty(~settings, x)
      and+ e1 = go(e1, ~sort=Exp)
      and+ y = drv_pat_to_pretty(~settings, y)
      and+ e2 = go(e2, ~sort=Exp);
      let all_ids = IdTagged.ids(syntax);
      let rule_ids =
        pad_ids(
          ~forbidden=[id],
          ~base=id,
          2,
          switch (all_ids) {
          | [_, ...rest] => rest
          | [] => []
          },
        );
      let (rule1_id, rule2_id) =
        switch (rule_ids) {
        | [a, b, ..._] => (a, b)
        | [a] => (a, Id.mk())
        | [] => (Id.mk(), Id.mk())
        };
      [
        mk_form(
          Drv(Case),
          id,
          [
            e
            @ try_newline()
            @ [mk_form(Drv(Rule), rule1_id, [x])]
            @ e1
            @ try_newline()
            @ [mk_form(Drv(Rule), rule2_id, [y])]
            @ e2
            @ try_newline(),
          ],
        ),
      ];
    | Roll(e) =>
      let+ e = go(e, ~sort=Exp);
      text_to_pretty(Id.invalid, Sort.Drv(Exp), "roll")
      @ [mk_form(Drv(ApExp), id, [e])];
    | Unroll(e) =>
      let+ e = go(e, ~sort=Exp);
      text_to_pretty(Id.invalid, Sort.Drv(Exp), "unroll")
      @ [mk_form(Drv(ApExp), id, [e])];
    | ExpHole => text_to_pretty(id, Sort.Drv(Exp), Token.wild)
    };
  wrap_with_secondary(~secondary=settings.secondary, syntax, content);
}
and drv_pat_to_pretty = (~settings: Settings.t, syntax: Drv.Pat.t): pretty => {
  let mk_form = mk_form(~secondary=settings.secondary);
  let go = (~inline=settings.inline) =>
    drv_pat_to_pretty(
      ~settings={
        ...settings,
        inline,
      },
    );
  let id = syntax |> Drv.Pat.rep_id;
  let content =
    switch (syntax |> Drv.Pat.term_of) {
    | Hole(h) => drv_type_hole_to_pretty(~settings, h)
    | Quote(e) => text_to_pretty(id, Sort.Drv(Pat), e)
    | Parens(e) =>
      let+ e = go(e);
      [mk_form(Drv(ParenPat), id, [e])];
    | Var(s) => text_to_pretty(id, Sort.Drv(Pat), s)
    | Cast(e, t) =>
      let+ e = go(e)
      and+ t = drv_typ_to_pretty(~settings, t);
      e @ [mk_form(Drv(Cast), id, [])] @ t;
    | Pair(l, r) =>
      let+ l = go(l)
      and+ r = go(r);
      [
        mk_form(
          Drv(ParenPat),
          id,
          [l @ [mk_form(Drv(CommaPat), id, [])] @ r],
        ),
      ];
    | InjL(p) =>
      let+ p = go(p);
      text_to_pretty(id, Sort.Drv(Pat), "L")
      @ [mk_form(Drv(ApPat), id, [p])];
    | InjR(p) =>
      let+ p = go(p);
      text_to_pretty(id, Sort.Drv(Pat), "R")
      @ [mk_form(Drv(ApPat), id, [p])];
    };
  wrap_with_secondary(~secondary=settings.secondary, syntax, content);
}
and drv_typ_to_pretty = (~settings: Settings.t, syntax: Drv.Typ.t): pretty => {
  let mk_form = mk_form(~secondary=settings.secondary);
  let go = (~inline=settings.inline) =>
    drv_typ_to_pretty(
      ~settings={
        ...settings,
        inline,
      },
    );
  let id = syntax |> Drv.Typ.rep_id;
  let content =
    switch (syntax |> Drv.Typ.term_of) {
    | Hole(h) => drv_type_hole_to_pretty(~settings, h)
    | Quote(e) => text_to_pretty(id, Sort.Drv(Exp), e)
    | Parens(e) =>
      let+ e = go(e);
      [mk_form(Drv(ParenTyp), id, [e])];
    | Num => text_to_pretty(id, Sort.Drv(Typ), "Num")
    | Bool => text_to_pretty(id, Sort.Drv(Typ), "Bool")
    | Arrow(l, r) =>
      let+ l = go(l)
      and+ r = go(r);
      l @ [mk_form(Drv(Arrow), id, [])] @ r;
    | Prod(l, r) =>
      let+ l = go(l)
      and+ r = go(r);
      l @ [mk_form(Drv(Prod), id, [])] @ r;
    | Unit => text_to_pretty(id, Sort.Drv(Typ), "Unit")
    | Sum(l, r) =>
      let+ l = go(l)
      and+ r = go(r);
      l @ [mk_form(Drv(Sum), id, [])] @ r;
    | Var(s) => text_to_pretty(id, Sort.Drv(Typ), s)
    | Rec(l, r) =>
      let+ l = drv_tpat_to_pretty(~settings, l)
      and+ r = go(r);
      [mk_form(Drv(Rec), id, [l])] @ r;
    | TypHole => text_to_pretty(id, Sort.Drv(Typ), Token.explicit_hole)
    };
  wrap_with_secondary(~secondary=settings.secondary, syntax, content);
}
and drv_tpat_to_pretty = (~settings: Settings.t, syntax: Drv.TPat.t): pretty => {
  let id = syntax |> Drv.TPat.rep_id;
  let content =
    switch (syntax |> Drv.TPat.term_of) {
    | Hole(h) => drv_type_hole_to_pretty(~settings, h)
    | Quote(e) => text_to_pretty(id, Sort.Drv(Exp), e)
    | Var(s) => text_to_pretty(id, Sort.Drv(TPat), s)
    };
  wrap_with_secondary(~secondary=settings.secondary, syntax, content);
}
and drv_type_hole_to_pretty =
    (~settings: Settings.t, syntax: DrvTermBase.type_hole): pretty => {
  let id = Id.invalid;
  switch (syntax) {
  | AbbrNotVar =>
    text_to_pretty(id, Sort.Drv(Typ), "Error: Abbreviation not a variable")
  | AbbrNotFound =>
    text_to_pretty(id, Sort.Drv(Typ), "Error: Abbreviation not found")
  | AbbrNotDrvTerm =>
    text_to_pretty(id, Sort.Drv(Typ), "Error: Abbreviation not a drv term")
  | Invalid(s) => text_to_pretty(id, Sort.Drv(Typ), "Error: " ++ s)
  | EmptyHole => text_to_pretty(id, Sort.Drv(Typ), Token.space)
  | MultiHole(tm) =>
    let+ tm =
      tm |> List.map(drv_to_pretty(~settings, ~sort=DrvSort.Exp)) |> all;
    ListUtil.flat_intersperse(
      Grout({
        id,
        shape: Concave,
      }),
      tm,
    );
  };
}
and drv_to_pretty = (~settings: Settings.t, drv: Drv.Any.t, ~sort): pretty => {
  let res =
    switch (drv) {
    | Exp(e) => drv_exp_to_pretty(~settings, e, ~sort)
    | Pat(p) => drv_pat_to_pretty(~settings, p)
    | Typ(t) => drv_typ_to_pretty(~settings, t)
    | TPat(tp) => drv_tpat_to_pretty(~settings, tp)
    };
  res;
};

let rec drv_formula_to_pretty: type a. (RuleFormula.t(a), DrvSort.t) => pretty =
  (formula, sort) => {
    let mk_form = mk_form(~secondary=Settings.AutoFormat);
    let go = drv_formula_to_pretty;
    let id = List.hd(formula.annotation.ids);
    let mk_jdmt_binop = (op, l, r, sort_l, sort_r) => {
      let+ l = go(l, sort_l)
      and+ r = go(r, sort_r);
      l
      @ [
        Tile({
          id,
          label: [op],
          mold: {
            out: Drv(Jdmt),
            in_: [],
            nibs: (
              Nib.{
                shape: Concave(Precedence.min),
                sort: Drv(sort_l),
              },
              Nib.{
                shape: Concave(Precedence.min),
                sort: Drv(sort_r),
              },
            ),
          },
          shards: [0],
          children: [],
        }),
      ]
      @ r;
    };
    switch (formula.term) {
    | LookUpExp(x)
    | LookUpPat(x)
    | LookUpTyp(x)
    | LookUpTPat(x) => text_to_pretty(id, Sort.Drv(sort), x)
    | UnboxCtx(e) => go(e, sort)
    | UnboxNumLit(e) => go(e, sort)
    | UnboxExpVar(e) => go(e, sort)
    | UnboxPatVar(p) => go(p, sort)
    | UnboxTypVar(t) => go(t, sort)
    | UnboxTPatVar(tp) => go(tp, sort)
    | ExpVar(x) => go(x, Exp)
    | HasType(e, t) =>
      let+ e = go(e, Exp)
      and+ t = go(t, Typ);
      [
        mk_form(
          Drv(ParenProp),
          id,
          [e @ [mk_form(Drv(HasType), id, [])] @ t],
        ),
      ];
    | Type(t) =>
      let+ t = go(t, Typ);
      [mk_form(Drv(Valid), id, [t])];
    | Fix(p, e) =>
      let+ p = go(p, Pat)
      and+ e = go(e, Exp);
      [mk_form(Drv(Fix), id, [p])] @ e;
    | Subst(e, x, e') =>
      let+ e = go(e, Exp)
      and+ x = go(x, Pat)
      and+ e' = go(e', Exp);
      [mk_form(Drv(Subst), id, [e, x])] @ e';
    | Ctx(ctx) => go(ctx, Ctx)
    | Cons(e, ctx) =>
      let+ e = go(e, Prop)
      and+ ctx = go(ctx, Ctx);
      e @ [mk_form(Drv(Cons), id, [])] @ ctx;
    | Neg(n) =>
      let+ n = go(n, Exp);
      [mk_form(Drv(Neg), id, [])] @ n;
    | Plus(l, r) =>
      let+ l = go(l, Exp)
      and+ r = go(r, Exp);
      l @ [mk_form(Drv(Plus), id, [])] @ r;
    | Minus(l, r) =>
      let+ l = go(l, Exp)
      and+ r = go(r, Exp);
      l @ [mk_form(Drv(Minus), id, [])] @ r;
    | Times(l, r) =>
      let+ l = go(l, Exp)
      and+ r = go(r, Exp);
      l @ [mk_form(Drv(Times), id, [])] @ r;
    | TypVar(x) => go(x, Typ)
    | Rec(tp, t) =>
      let+ tp = go(tp, TPat)
      and+ t = go(t, Typ);
      [mk_form(Drv(Rec), id, [tp])] @ t;
    | Glb(l, r) =>
      let+ l = go(l, Typ)
      and+ r = go(r, Typ);
      [mk_form(Drv(Glb), id, [l, r])];
    | SubstTy(t, x, t') =>
      let+ t = go(t, Typ)
      and+ x = go(x, TPat)
      and+ t' = go(t', Typ);
      [mk_form(Drv(SubstTy), id, [t, x])] @ t';
    | Ignore(_) => []
    | Gt(l, r) => mk_jdmt_binop(">", l, r, Exp, Exp)
    | Lt(l, r) => mk_jdmt_binop("<", l, r, Exp, Exp)
    | Eq(l, r) => mk_jdmt_binop("=", l, r, Exp, Exp)
    | NotGt(l, r) => mk_jdmt_binop("≯", l, r, Exp, Exp)
    | NotLt(l, r) => mk_jdmt_binop("≮", l, r, Exp, Exp)
    | NotEq(l, r) => mk_jdmt_binop("≠", l, r, Exp, Exp)
    | Mem(p, ctx) => mk_jdmt_binop("∈", p, ctx, Prop, Ctx)
    | Subset(l, r) => mk_jdmt_binop("⊆", l, r, Ctx, Ctx)
    | EqExp(l, r) => mk_jdmt_binop("=", l, r, Exp, Exp)
    | EqCtx(l, r) => mk_jdmt_binop("=", l, r, Ctx, Ctx)
    | EqTyp(l, r) => mk_jdmt_binop("=", l, r, Typ, Typ)
    };
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
  | Invalid(x) =>
    wrap(exp, invalid_to_pretty(exp |> Exp.rep_id, Sort.Exp, x))
  | EmptyHole =>
    let id = exp |> Exp.rep_id;
    let seg =
      switch (hole_lexeme(exp.annotation)) {
      | Some(tok) => text_to_pretty(id, Sort.Exp, tok)
      | None =>
        settings.hole_tiles
          ? text_to_pretty(id, Sort.Exp, "?")
          : p_just([
              Grout({
                id,
                shape: Convex,
              }),
            ])
      };
    wrap(exp, seg);
  | Undefined =>
    wrap(exp, text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "undefined"))
  | Atom(c) =>
    wrap(
      exp,
      text_to_pretty(
        exp |> Exp.rep_id,
        Sort.Exp,
        atom_lexeme(~settings, exp.annotation, c),
      ),
    )
  | DrvQuote(d, sort) =>
    let+ d = drv_to_pretty(~settings, d, ~sort);
    let form: Form.drv_compound_form =
      switch (sort) {
      | Jdmt => OfJdmt
      | Ctx => OfCtx
      | Prop => OfProp
      | Exp => OfAlfaExp
      | Pat => OfAlfaPat
      | Typ => OfAlfaTyp
      | TPat => OfAlfaTPat
      };
    [mk_form(Drv(form), exp |> Exp.rep_id, [d])];
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
      IdTagged.ids(exp)
      |> List.tl
      |> pad_ids(~base=IdTagged.ids(exp) |> List.hd, List.length(xs)),
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
    wrap(
      exp,
      p_just(form(x, xs) |> project_table_if(settings.project_tables)),
    );
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
  | MultiHole([x]) when op_lexeme(exp.annotation) != None =>
    /* Stranded prefix op (see MakeTerm's Pre captures) */
    let op = Option.get(op_lexeme(exp.annotation));
    let+ x = any_to_pretty(~settings, x);
    wrap(exp, [pre_op_tile(exp |> Exp.rep_id, Sort.Exp, op), ...x]);
  | MultiHole([l, r]) when op_lexeme(exp.annotation) != None =>
    /* Unknown infix operator (see MakeTerm's exp Bin fallthrough):
       reconstruct the operator tile from the recorded lexeme, with the
       same Any-sorted max-precedence bin mold Form gives unknown ops */
    let op = Option.get(op_lexeme(exp.annotation));
    let id = exp |> Exp.rep_id;
    let+ l = any_to_pretty(~settings, l)
    and+ r = any_to_pretty(~settings, r);
    wrap(exp, l @ [op_tile(id, Sort.Exp, op)] @ r);
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
  | Parens({term: FixF(p, e, _), _} as inner_exp) =>
    let id = inner_exp |> Exp.rep_id;
    let+ p = pat_to_pretty(~settings: Settings.t, p)
    and+ e = go(e);
    let name =
      "<" ++ (Exp.get_fn_name(exp) |> Option.value(~default="fun")) ++ ">";
    let fix_form = [mk_form(Fix, id, [p])] @ e;
    wrap(
      exp,
      [mk_form(ParensExp, exp |> Exp.rep_id, [fix_form])]
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
             ~show_ascriptions=settings.show_ascriptions,
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
  /* MakeTerm puts the = tile's id on the synthesized singleton Tuple
     (the inner TupLabel is fresh) -- carry it back onto the TupLabel so
     the reprinted = tile keeps the parse's id, mirroring MakeTerm's own
     singleton-unwrap convention for comma children. */
  | Tuple([{term: TupLabel(_), _} as le]) =>
    let (le_term, _) = IdTagged.unwrap(le);
    let (_, rewrap_tup) = IdTagged.unwrap(exp);
    go(rewrap_tup(le_term));
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
        ~lexeme=settings.use_literal_lexemes ? exp.annotation.lexeme : None,
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
            ~lexeme=settings.use_literal_lexemes ? l.annotation.lexeme : None,
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
        switch (settings.secondary) {
        | AutoFormat =>
          let first = Segment.first_string(e);
          if (Token.begins_with_potential_operator(first)
              && !String.starts_with(first, ~prefix="…")) {
            [Secondary(mk_space(Id.mk())), ...e];
          } else {
            e;
          };
        | PreserveExact => e
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
            ~lexeme=settings.use_literal_lexemes ? l.annotation.lexeme : None,
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
  | Ap(Forward, e1, {term: Tuple([]), _} as arg)
      when Id.is_nullary_ap_flag(IdTagged.ids(arg)) =>
    /* f() — the flag id marks the empty argument tuple as coming from
       the single-token nullary-ap form (see MakeTerm.exp_term), not
       from a literal () argument */
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1);
    wrap(exp, e1 @ [mk_form(ApExpEmpty, id, [])]);
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
      @ (settings.inline ? [] : [Secondary(mk_newline(Id.mk()))])
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
    /* ids = [ap tile, comma tiles...]: n args have n-1 commas, so pad
       the tail to n-1 and use it directly (padding to n and dropping
       the head minted a fresh id for the first comma) */
    let (id, comma_ids) = (
      IdTagged.ids(exp) |> List.hd,
      IdTagged.ids(exp)
      |> List.tl
      |> pad_ids(~base=IdTagged.ids(exp) |> List.hd, List.length(es) - 1),
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
                  comma_ids,
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
  | UnOp(Bool(Not), e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    wrap(exp, [mk_form(Not, id, [])] @ e);
  | UnOp(Int(Minus) | Nat(Minus) | SInt(Minus), e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    wrap(exp, [mk_form(UnaryMinus, id, [])] @ e);
  | UnOp(Float(Minus), _) =>
    failwith(
      "ExpToSegment: UnOp(Float(Minus)) should have been rewritten by parenthesize",
    )
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
       which produces IDs in this order during absorption. The
       rule IDs must be distinct from each other AND from the case
       ID — see pad_ids for the deduplication behavior. */
    let+ e = go(e)
    and+ rs: list((Segment.t, Segment.t)) = {
      rs
      |> List.map(((p, e)) =>
           (pat_to_pretty(~settings: Settings.t, p), go(e))
         )
      |> List.map(((x, y)) => (x, y))
      |> all;
    };
    let all_exp_ids = IdTagged.ids(exp);
    let case_id = all_exp_ids |> List.hd;
    let (id, ids) = (
      case_id,
      all_exp_ids
      |> List.tl
      |> pad_ids(~forbidden=[case_id], ~base=case_id, List.length(rs)),
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
             /* No wrap_item: a bare-expression item's ModExp wrapper shares
                its rep id (hence its secondary) with the inner exp, whose own
                wrap already emits it — wrapping both duplicates the runs,
                growing whitespace on every roundtrip. */
             go(e)
           | EmptyHole =>
             let item_id = item |> Mod.rep_id;
             let seg =
               switch (hole_lexeme(item.annotation)) {
               | Some(tok) => text_to_pretty(item_id, Sort.Mod, tok)
               | None => [
                   Grout({
                     id: item_id,
                     shape: Convex,
                   }),
                 ]
               };
             p_just(wrap_item(item, seg));
           | Invalid(s) =>
             p_just(
               wrap_item(
                 item,
                 text_to_pretty(item |> Mod.rep_id, Sort.Mod, s),
               ),
             )
           | ModuleMod(mp, e) =>
             let mp_seg = mpat_to_seg(~settings, mp);
             let+ e = go(e);
             wrap_item(
               item,
               [mk_form(ModuleMod, item |> Mod.rep_id, [mp_seg])] @ e,
             );
           | MultiHole(es) =>
             let+ es = es |> List.map(any_to_pretty(~settings)) |> all;
             wrap_item(item, List.flatten(es));
           }
         )
      |> all;
    /* Join items with semicolons and wrap in braces */
    let ids =
      IdTagged.ids(exp)
      |> List.tl
      |> pad_ids(~base=IdTagged.ids(exp) |> List.hd, List.length(items) - 1);
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
  | ModuleExp(mp, def, body) =>
    let id = exp |> Exp.rep_id;
    let mp_seg = mpat_to_seg(~settings, mp);
    let+ def = go(def)
    and+ body = go(body);
    let body =
      settings.inline ? body : [Secondary(mk_newline(Id.mk()))] @ body;
    wrap(exp, [mk_form(ModuleExp, id, [mp_seg, def])] @ body);
  };
}
and mpat_to_seg = (~settings: Settings.t, mp: MPat.t): Segment.t => {
  let wrap = wrap_with_secondary(~secondary=settings.secondary);
  let content =
    switch (mp.term) {
    | Var(name) => text_to_pretty(mp |> MPat.rep_id, Sort.MPat, name)
    | Asc(inner, typ) =>
      let inner_seg = mpat_to_seg(~settings, inner);
      let typ_seg = typ_to_pretty(~settings, typ);
      inner_seg
      @ [
        Tile({
          /* the Asc term's rep id IS the colon tile id from parsing;
             a fresh id here churned identity on every print */
          id: MPat.rep_id(mp),
          label: [":"],
          mold:
            Mold.mk_bin(
              ~l=Sort.MPat,
              ~r=Sort.Typ,
              Precedence.asc,
              Sort.MPat,
              [],
            ),
          shards: [0],
          children: [],
        }),
      ]
      @ typ_seg;
    | _ => text_to_pretty(mp |> MPat.rep_id, Sort.MPat, "?")
    };
  wrap(mp, content);
}
and pat_to_pretty = (~settings: Settings.t, pat: Pat.t): pretty => {
  let go = pat_to_pretty(~settings: Settings.t);
  let wrap = wrap_with_secondary(~secondary=settings.secondary);
  /* Use settings-aware concatenation and form building */
  let (@) = concat_segment(~secondary=settings.secondary);
  let mk_form = mk_form(~secondary=settings.secondary);
  switch (pat |> Pat.term_of) {
  | Invalid(t) =>
    wrap(pat, invalid_to_pretty(pat |> Pat.rep_id, Sort.Pat, t))
  | EmptyHole =>
    let id = pat |> Pat.rep_id;
    let seg =
      switch (hole_lexeme(pat.annotation)) {
      | Some(tok) => text_to_pretty(id, Sort.Pat, tok)
      | None =>
        settings.hole_tiles
          ? text_to_pretty(id, Sort.Pat, "?")
          : p_just([
              Grout({
                id,
                shape: Convex,
              }),
            ])
      };
    wrap(pat, seg);
  | Wild => wrap(pat, text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "_"))
  | ExplicitNonlabel =>
    wrap(pat, text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "_"))
  | Var(v) => wrap(pat, text_to_pretty(pat |> Pat.rep_id, Sort.Pat, v))
  | Atom(c) =>
    wrap(
      pat,
      text_to_pretty(
        pat |> Pat.rep_id,
        Sort.Pat,
        atom_lexeme(~settings, pat.annotation, c),
      ),
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
      IdTagged.ids(pat)
      |> List.tl
      |> pad_ids(~base=IdTagged.ids(pat) |> List.hd, List.length(xs)),
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
            ~lexeme=settings.use_literal_lexemes ? l.annotation.lexeme : None,
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
        switch (settings.secondary) {
        | AutoFormat =>
          let first = Segment.first_string(p);
          if (Token.begins_with_potential_operator(first)
              && !String.starts_with(first, ~prefix="…")) {
            [Secondary(mk_space(Id.mk())), ...p];
          } else {
            p;
          };
        | PreserveExact => p
        },
      ]),
    );
  | Label(l) =>
    wrap(
      pat,
      text_to_pretty(
        pat |> Pat.rep_id,
        Sort.Pat,
        quoted_label_lexeme(~settings, pat.annotation, l),
      ),
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
  | MultiHole([x]) when op_lexeme(pat.annotation) != None =>
    /* Stranded prefix op (see MakeTerm's Pre captures) */
    let op = Option.get(op_lexeme(pat.annotation));
    let+ x = any_to_pretty(~settings, x);
    wrap(pat, [pre_op_tile(pat |> Pat.rep_id, Sort.Pat, op), ...x]);
  | MultiHole([l, r]) when op_lexeme(pat.annotation) != None =>
    /* Unknown infix operator in pattern position (see MakeTerm's pat
       Bin fallthrough) */
    let op = Option.get(op_lexeme(pat.annotation));
    let id = pat |> Pat.rep_id;
    let+ l = any_to_pretty(~settings, l)
    and+ r = any_to_pretty(~settings, r);
    wrap(pat, l @ [op_tile(id, Sort.Pat, op)] @ r);
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
  | Ap(p1, {term: Tuple([]), _} as arg)
      when Id.is_nullary_ap_flag(IdTagged.ids(arg)) =>
    /* C() in patterns — see the exp Ap case */
    let id = pat |> Pat.rep_id;
    let+ p1 = go(p1);
    wrap(pat, p1 @ [mk_form(ApPatEmpty, id, [])]);
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
        /* MakeTerm.parse_sum_term builds ids as ids_ctr @ ids_ap:
           hd is the constructor tile, nth 1 the ap tile */
        let+ constructor =
          text_to_pretty(
            OptUtil.get(() => Id.mk(), ListUtil.hd_opt(ann.ids)),
            Sort.Typ,
            c,
          );
        wrap_variant_secondary(
          ann,
          constructor
          @ [
            mk_form(
              ApTyp,
              OptUtil.get(() => Id.mk(), List.nth_opt(ann.ids, 1)),
              [go(x)],
            ),
          ],
        );
      }
    | BadEntry(x) => go(x);
  switch (typ |> Typ.term_of) {
  | Unknown(Hole(Invalid(s))) =>
    wrap(typ, invalid_to_pretty(typ |> Typ.rep_id, Sort.Typ, s))
  | Unknown(Internal)
  | Unknown(SynSwitch)
  | Unknown(Hole(EmptyHole)) =>
    wrap(
      typ,
      switch (hole_lexeme(typ.annotation)) {
      | Some(tok) => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, tok)
      | None =>
        if (settings.show_unknown_as_hole && !settings.hole_tiles) {
          let id = typ |> Typ.rep_id;
          p_just([
            Grout({
              id,
              shape: Convex,
            }),
          ]);
        } else {
          text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "?");
        }
      },
    )
  | Unknown(Hole(MultiHole([x]))) when op_lexeme(typ.annotation) != None =>
    /* Stranded prefix op (see MakeTerm's Pre captures) */
    let op = Option.get(op_lexeme(typ.annotation));
    let+ x = any_to_pretty(~settings, x);
    wrap(typ, [pre_op_tile(typ |> Typ.rep_id, Sort.Typ, op), ...x]);
  | Unknown(Hole(MultiHole([l, r]))) when op_lexeme(typ.annotation) != None =>
    /* Unknown infix operator in type position (see MakeTerm's typ Bin
       fallthrough) */
    let op = Option.get(op_lexeme(typ.annotation));
    let id = typ |> Typ.rep_id;
    let+ l = any_to_pretty(~settings, l)
    and+ r = any_to_pretty(~settings, r);
    wrap(typ, l @ [op_tile(id, Sort.Typ, op)] @ r);
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
  | DrvQuoteTy(d) =>
    wrap(
      typ,
      text_to_pretty(typ |> Typ.rep_id, Sort.Typ, DrvSort.to_string(d)),
    )
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
      text_to_pretty(
        typ |> Typ.rep_id,
        Sort.Typ,
        quoted_label_lexeme(~settings, typ.annotation, l),
      ),
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
            ~lexeme=settings.use_literal_lexemes ? l.annotation.lexeme : None,
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
        switch (settings.secondary) {
        | AutoFormat =>
          let first = Segment.first_string(t);
          if (Token.begins_with_potential_operator(first)
              && !String.starts_with(first, ~prefix="…")) {
            [Secondary(mk_space(Id.mk())), ...t];
          } else {
            t;
          };
        | PreserveExact => t
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
            ~lexeme=settings.use_literal_lexemes ? t2.annotation.lexeme : None,
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
  | Sum([]) => wrap(typ, text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Void"))
  | Sum([t]) =>
    let id = typ |> Typ.rep_id;
    let+ t = go_constructor(t);
    wrap(typ, [mk_form(TypSumSingle, id, [])] @ t);
  | Sum([t, ...ts]) =>
    /* A leading-plus parse absorbs the inner sum's separator ids after
       the leading + tile, yielding n ids for n variants; a bare binary
       sum (A + B) carries only the n-1 separators. Evaluator-built sums
       (single id, n >= 2) print bare, which reparses to the same Sum. */
    let n = List.length(ts) + 1;
    let has_leading = List.length(IdTagged.ids(typ)) >= n;
    let+ t = go_constructor(t)
    and+ ts = ts |> List.map(go_constructor) |> all;
    if (has_leading) {
      let ids = IdTagged.ids(typ) |> pad_ids(n);
      let id = List.hd(ids);
      let ids = List.tl(ids);
      wrap(
        typ,
        [mk_form(TypSumSingle, id, [])]
        @ t
        @ List.flatten(
            List.map2((id, t) => [mk_form(TypPlus, id, [])] @ t, ids, ts),
          ),
      );
    } else {
      let ids = IdTagged.ids(typ) |> pad_ids(n - 1);
      wrap(
        typ,
        t
        @ List.flatten(
            List.map2((id, t) => [mk_form(TypPlus, id, [])] @ t, ids, ts),
          ),
      );
    };
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
             let seg =
               switch (hole_lexeme(item.annotation)) {
               | Some(tok) => text_to_pretty(item_id, Sort.Sig, tok)
               | None => [
                   Grout({
                     id: item_id,
                     shape: Convex,
                   }),
                 ]
               };
             p_just(wrap_item(item, seg));
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
      IdTagged.ids(typ)
      |> List.tl
      |> pad_ids(~base=IdTagged.ids(typ) |> List.hd, List.length(items) - 1);
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
    wrap(tpat, invalid_to_pretty(tpat |> TPat.rep_id, Sort.TPat, t))
  | EmptyHole =>
    let id = tpat |> TPat.rep_id;
    let seg =
      switch (hole_lexeme(tpat.annotation)) {
      | Some(tok) => text_to_pretty(id, Sort.TPat, tok)
      | None =>
        p_just([
          Grout({
            id,
            shape: Convex,
          }),
        ])
      };
    wrap(tpat, seg);
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
and mod_to_pretty = (~settings: Settings.t, item: Mod.t): pretty => {
  let wrap_item = wrap_with_secondary(~secondary=settings.secondary);
  let mk_form = mk_form(~secondary=settings.secondary);
  switch (item.term) {
  | ModLet(p, e) =>
    let+ p = pat_to_pretty(~settings, p)
    and+ e = exp_to_pretty(~settings, e);
    wrap_item(item, [mk_form(ModLet, item |> Mod.rep_id, [p])] @ e);
  | ModType(tp, t) =>
    let+ tp = tpat_to_pretty(~settings, tp)
    and+ t = typ_to_pretty(~settings, t);
    wrap_item(item, [mk_form(ModType, item |> Mod.rep_id, [tp])] @ t);
  | ModExp(e) =>
    /* No wrap_item: ModExp shares its rep id (hence secondary) with the
       inner exp; see the Module case in exp_to_pretty. */
    exp_to_pretty(~settings, e)
  | ModuleMod(mp, e) =>
    let mp_seg = mpat_to_seg(~settings, mp);
    let+ e = exp_to_pretty(~settings, e);
    wrap_item(
      item,
      [mk_form(ModuleMod, item |> Mod.rep_id, [mp_seg])] @ e,
    );
  | EmptyHole =>
    let seg =
      switch (hole_lexeme(item.annotation)) {
      | Some(tok) => text_to_pretty(item |> Mod.rep_id, Sort.Mod, tok)
      | None => [
          Grout({
            id: item |> Mod.rep_id,
            shape: Convex,
          }),
        ]
      };
    p_just(wrap_item(item, seg));
  | Invalid(s) =>
    p_just(
      wrap_item(item, invalid_to_pretty(item |> Mod.rep_id, Sort.Mod, s)),
    )
  | MultiHole(_) =>
    p_just(
      wrap_item(item, text_to_pretty(item |> Mod.rep_id, Sort.Mod, "?")),
    )
  };
}
and sig_to_pretty = (~settings: Settings.t, item: Sig.t): pretty => {
  let wrap_item = wrap_with_secondary(~secondary=settings.secondary);
  let mk_form = mk_form(~secondary=settings.secondary);
  switch (item.term) {
  | SigLet(p) =>
    let+ p = pat_to_pretty(~settings, p);
    wrap_item(item, [mk_form(SigLet, item |> Sig.rep_id, [])] @ p);
  | SigType(tp, t) =>
    let+ tp = tpat_to_pretty(~settings, tp)
    and+ t = typ_to_pretty(~settings, t);
    wrap_item(item, [mk_form(SigType, item |> Sig.rep_id, [tp])] @ t);
  | EmptyHole =>
    let seg =
      switch (hole_lexeme(item.annotation)) {
      | Some(tok) => text_to_pretty(item |> Sig.rep_id, Sort.Sig, tok)
      | None => [
          Grout({
            id: item |> Sig.rep_id,
            shape: Convex,
          }),
        ]
      };
    p_just(wrap_item(item, seg));
  | Invalid(s) =>
    p_just(
      wrap_item(item, invalid_to_pretty(item |> Sig.rep_id, Sort.Sig, s)),
    )
  | MultiHole(_) =>
    p_just(
      wrap_item(item, text_to_pretty(item |> Sig.rep_id, Sort.Sig, "?")),
    )
  };
}
and mpat_to_pretty = (~settings: Settings.t, mp: MPat.t): pretty => {
  p_just(mpat_to_seg(~settings, mp));
}
and any_to_pretty = (~settings: Settings.t, any: Any.t): pretty => {
  switch (any) {
  | Exp(e) => exp_to_pretty(~settings: Settings.t, e)
  | Pat(p) => pat_to_pretty(~settings: Settings.t, p)
  | Typ(t) => typ_to_pretty(~settings: Settings.t, t)
  | TPat(tp) => tpat_to_pretty(~settings: Settings.t, tp)
  | Drv(d) => drv_to_pretty(~settings: Settings.t, d, ~sort=Jdmt)
  | Mod(m) => mod_to_pretty(~settings, m)
  | Sig(s) => sig_to_pretty(~settings, s)
  | MPat(mp) => mpat_to_pretty(~settings, mp)
  | Rul(r) => rul_to_pretty(~settings, r)
  | Any(_) =>
    let id = any |> Any.rep_id;
    p_just([
      Grout({
        id,
        shape: Convex,
      }),
    ]);
  };
}
and rul_to_pretty = (~settings: Settings.t, rul: Rul.t): pretty => {
  let wrap = wrap_with_secondary(~secondary=settings.secondary);
  let (@) = concat_segment(~secondary=settings.secondary);
  let mk_form = mk_form(~secondary=settings.secondary);
  let rep_id =
    OptUtil.get(() => Id.mk(), ListUtil.hd_opt(IdTagged.ids(rul)));
  switch (rul |> IdTagged.term_of) {
  | Invalid(t) => wrap(rul, invalid_to_pretty(rep_id, Sort.Rul, t))
  | MultiHole(es) =>
    let+ es = es |> List.map(any_to_pretty(~settings)) |> all;
    let num_grouts = max(0, List.length(es) - 1);
    let ids = IdTagged.ids(rul) |> pad_ids(num_grouts);
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
    wrap(rul, seg);
  | Rules(scrut, rules) =>
    /* A case-less rule chain (scrutinee followed by | p => e clauses),
       reachable as a MultiHole kid. Previously printed as a single
       convex grout, destroying the content. */
    let+ scrut = exp_to_pretty(~settings, scrut)
    and+ rs =
      rules
      |> List.map(((p, e)) =>
           (pat_to_pretty(~settings, p), exp_to_pretty(~settings, e))
         )
      |> all;
    let ids = IdTagged.ids(rul) |> pad_ids(List.length(rs));
    wrap(
      rul,
      scrut
      @ (
        List.map2((id, (p, e)) => [mk_form(Rule, id, [p])] @ e, ids, rs)
        |> List.flatten
      ),
    );
  };
}
and label_to_pretty =
    (
      ~label_format: Settings.label_format,
      ~label_only_position,
      ~lexeme: option(string)=None,
      sort: Sort.t,
      label: string,
      id: Uuidm.t,
    )
    : pretty => {
  /* A recorded lexeme (the token as typed, e.g. unnecessarily-backticked
     `a`) wins when it still denotes this label */
  let lexeme =
    switch (lexeme) {
    | Some(l)
        when
          Token.is_quoted_label(l)
          && (
            Token.strip_quotes(~quote=Token.label_delim, l) == label
            || l == label
          ) =>
      Some(l)
    | _ => None
    };
  text_to_pretty(
    id,
    sort,
    switch (lexeme) {
    | Some(l) => l
    | None =>
      if (label_only_position) {
        switch (label_format) {
        | QuoteWhenNecessary => Token.quote_label_when_necessary(label)
        | AlwaysQuote => Token.label_quote(label)
        };
      } else {
        label;
      }
    },
  );
};

/* === Shard-provenance stripping (canonical-completion roundtrip) ===
   Terms parsed from canonically completed segments record, per completed
   tile, the shard indices physically present in the visible segment
   (IdTag.incomplete). Printing emits complete tiles; this pass truncates
   them back to their original shards, splicing the dropped shards\'
   children into the parent segment, then regrouts. Applies in all print
   modes: the completion is a semantic device, not user-typed syntax.
   V1 limitations: masks on Drv terms are not collected (drv has its own
   traversal machinery), and projector-internal syntax is left alone. */

let collect_shard_masks =
    (any: Any.t): Id.Map.t(IdTagged.IdTag.incomplete_mask) => {
  let acc = ref(Id.Map.empty);
  let record = (ann: IdTagged.IdTag.t) =>
    List.iter(
      ((id, mask)) => acc := Id.Map.add(id, mask, acc^),
      ann.incomplete,
    );
  let f = (continue, t: IdTagged.t(_)) => {
    record(t.annotation);
    continue(t);
  };
  let _ =
    Any.map_term(
      ~f_exp=f,
      ~f_pat=f,
      ~f_typ=f,
      ~f_tpat=f,
      ~f_rul=f,
      ~f_mod=f,
      ~f_sig=f,
      ~f_mpat=f,
      any,
    );
  acc^;
};

let rec strip_synthesized_shards =
        (masks: Id.Map.t(IdTagged.IdTag.incomplete_mask), seg: Segment.t)
        : Segment.t =>
  seg
  |> List.concat_map((p: Piece.t) =>
       switch (p) {
       | Tile(t) =>
         let children =
           List.map(strip_synthesized_shards(masks), t.children);
         let rec is_subsequence = (xs: list(int), ys: list(int)) =>
           switch (xs, ys) {
           | ([], _) => true
           | (_, []) => false
           | ([x, ...xs'], [y, ...ys']) =>
             x == y ? is_subsequence(xs', ys') : is_subsequence(xs, ys')
           };
         switch (Id.Map.find_opt(t.id, masks)) {
         | Some({present: [], _}) =>
           /* Fully synthetic tile (e.g. the case/end wrapped around an
              orphaned rule chain): drop it, splice out all children */
           List.concat(children)
         | Some({present: orig, prefixes})
             when orig != t.shards && is_subsequence(orig, t.shards) =>
           /* General mask (prefix = trailing completion, suffix = leading,
              subsequence = middle): keep the originally-present shards.
              The printed tile is complete, so child i sits between shards
              i and i+1 (shard j sits between children j-1 and j); children
              between consecutive kept shards merge into the truncated
              tile's child slots, and children outside the kept span splice
              out on the matching side. A dropped shard recorded as
              PARTIALLY TYPED re-emits its original prefix token at its
              boundary (`i` where the completed `in` sat). Plain list ops
              throughout — the file-level @ is AutoFormat concat, which
              would insert heuristic spaces. */
           let tok = (j: int): list(Piece.t) =>
             switch (
               List.find_opt(
                 (sp: IdTagged.IdTag.shard_prefix) => sp.shard == j,
                 prefixes,
               )
             ) {
             | None => []
             | Some(sp) =>
               [
                 Piece.Tile({
                   id: sp.token_id,
                   label: [String.sub(List.nth(t.label, j), 0, sp.len)],
                   mold: Mold.mk_bin(Precedence.concave_grout, Exp, []),
                   shards: [0],
                   children: [],
                 }),
               ]
               @ (
                 switch (sp.debris) {
                 | Some(id) => [
                     Piece.Grout({
                       id,
                       shape: Concave,
                     }),
                   ]
                 | None => []
                 }
               )
             };
           let child = i => List.nth(children, i);
           /* children a..b-1 with prefix tokens at interior dropped-
              shard boundaries; ~end_tok also emits the token for
              shard b (used past the last kept shard) */
           let span = (~end_tok, a: int, b: int): list(Piece.t) =>
             List.init(b - a, k => a + k)
             |> List.concat_map(i =>
                  child(i) @ (i + 1 < b || end_tok ? tok(i + 1) : [])
                );
           let first = List.hd(orig);
           let last = List.nth(orig, List.length(orig) - 1);
           let before =
             List.init(first, i => i)
             |> List.concat_map(i => tok(i) @ child(i));
           let after = span(~end_tok=true, last, List.length(children));
           let rec kept_slots = m =>
             switch (m) {
             | [a, b, ...rest] => [
                 span(~end_tok=false, a, b),
                 ...kept_slots([b, ...rest]),
               ]
             | _ => []
             };
           List.concat([
             before,
             [
               Piece.Tile({
                 ...t,
                 shards: orig,
                 children: kept_slots(orig),
               }),
             ],
             after,
           ]);
         | _ => [
             Piece.Tile({
               ...t,
               children,
             }),
           ]
         };
       | p => [p]
       }
     );

let strip_if_incomplete = (any: Any.t, seg: Segment.t): Segment.t => {
  let masks = collect_shard_masks(any);
  Id.Map.is_empty(masks) ? seg : seg |> strip_synthesized_shards(masks);
};

let exp_to_segment =
    (~already_paren=false, ~settings: Settings.t, exp: Exp.t): Segment.t => {
  let exp =
    exp
    |> parenthesize(
         ~parenthesization=settings.parenthesization,
         ~already_paren,
         ~show_filters=settings.show_filters,
         ~show_ascriptions=settings.show_ascriptions,
       );
  let p = exp_to_pretty(~settings, exp);
  p |> PrettySegment.select |> strip_if_incomplete(Exp(exp));
};

let pat_to_segment = (~settings: Settings.t, pat: Pat.t): Segment.t =>
  pat_to_pretty(~settings, pat) |> PrettySegment.select;

let typ_to_segment = (~settings: Settings.t, typ: Typ.t): Segment.t => {
  /* Desugar Sig types to labeled tuples so they display as (x=Int, y=Bool)
     instead of {sig}. Uses empty ctx since we're just displaying. */
  let typ = Typ.desugar_sig(Ctx.empty, typ);
  let typ =
    typ
    |> parenthesize_typ(
         ~parenthesization=settings.parenthesization,
         ~show_filters=settings.show_filters,
         ~show_ascriptions=settings.show_ascriptions,
       );
  let p = typ_to_pretty(~settings, typ);
  p |> PrettySegment.select |> strip_if_incomplete(Typ(typ));
};

let any_to_segment =
    (~already_paren=false, ~settings: Settings.t, any: Any.t): Segment.t => {
  let any =
    any
    |> parenthesize_any(
         ~parenthesization=settings.parenthesization,
         ~already_paren,
         ~show_filters=settings.show_filters,
         ~show_ascriptions=settings.show_ascriptions,
       );
  let p = any_to_pretty(~settings, any);
  p |> PrettySegment.select |> strip_if_incomplete(any);
};
