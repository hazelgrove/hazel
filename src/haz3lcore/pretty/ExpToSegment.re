open Util;
open PrettySegment;
open Base;
let mk_space = Secondary.mk_space;
let mk_newline = Secondary.mk_newline;
open Language;

module Settings = {
  type t = {
    inline: bool,
    fold_case_clauses: bool,
    fold_fn_bodies: bool,
    hide_fixpoints: bool,
    show_filters: bool,
    show_unknown_as_hole: bool,
  };

  let of_core = (~inline, settings: CoreSettings.t) => {
    inline,
    fold_case_clauses: !settings.evaluation.show_case_clauses,
    fold_fn_bodies: !settings.evaluation.show_fn_bodies,
    hide_fixpoints: !settings.evaluation.show_fixpoints,
    show_filters: settings.evaluation.show_stepper_filters,
    show_unknown_as_hole: true,
  };

  let editable = (~inline) => {
    {
      inline,
      fold_case_clauses: false,
      fold_fn_bodies: false,
      hide_fixpoints: false,
      show_filters: true,
      show_unknown_as_hole: true,
    };
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
  | EmptyHole
  | Deferral(_)
  | BuiltinFun(_)
  | Undefined
  | Label(_)
  | Constructor(_)
  | LivelitName(_)
  | TupLabel(_) => Precedence.max

  // Same goes for forms which are already surrounded
  | Parens(_)
  | Probe(_)
  | ListLit(_)
  | Test(_)
  | HintedTest(_)
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
  | FixF(_) => Precedence.fun_
  | Tuple(_) => Precedence.prod
  | Seq(_) => Precedence.semi
  | TupleExtension(_, _) => Precedence.plus
  | Dot(_) => Precedence.dot

  // Top-level things
  | Filter(_)
  | TyAlias(_)
  | Use(_)
  | Let(_) => Precedence.let_

  // Matt: I think multiholes are min because we don't know the precedence of the `⟩?⟨`s
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
  | Atom(Bool(_) | Int(_) | SInt(_) | Float(_) | String(_) | Nat(_))
  | Constructor(_)
  | Label(_)
  | TupLabel(_) => Precedence.max

  // Same goes for forms which are already surrounded
  | ListLit(_)
  | Parens(_)
  | Probe(_) => Precedence.max

  // Other forms
  | Cons(_) => Precedence.cons
  | Ap(_) => Precedence.ap
  | Asc(_) => Precedence.asc
  | Tuple(_) => Precedence.prod

  // Matt: I think multiholes are min because we don't know the precedence of the `⟩?⟨`s
  | MultiHole(_) => Precedence.min
  };

let external_precedence_typ = (tp: Typ.t) =>
  switch (Typ.term_of(tp)) {
  // Indivisible forms never need parentheses around them
  | Unknown({term: Hole(Invalid(_)), _})
  | Unknown({term: Internal, _})
  | Unknown({term: SynSwitch, _})
  | Unknown({term: Hole(EmptyHole), _})
  | Unknown({term: Hole(CycleHole), _})
  | Unknown({term: LArrow(_), _}) // TODO (THI): this probably needs to be recursive?
  | Unknown({term: RArrow(_), _}) // TODO (THI): this probably needs to be recursive?
  | Unknown({term: NProduct(_), _}) // TODO (THI): this probably needs to be recursive?
  | Unknown({term: MList(_), _}) // TODO (THI): this probably needs to be recursive?
  | Unknown({term: RForall(_), _}) // TODO (THI): this probably needs to be recursive?
  | Unknown({term: TupLabel(_), _}) // TODO (THI): this probably needs to be recursive?
  | Unknown({term: TupLabelArg(_), _}) // TODO (THI): this probably needs to be recursive?
  | Unknown({term: Join(_), _}) // TODO (THI): this probably needs to be recursive?
  | Var(_)
  | Atom(_)
  | Label(_)
  | TupLabel(_) => Precedence.max

  // Same goes for forms which are already surrounded
  | Parens(_)
  | List(_) => Precedence.max

  // Other forms
  | Prod(_) => Precedence.comma
  | Arrow(_, _) => Precedence.type_arrow
  | Sum(_) => Precedence.type_plus
  | Rec(_, _) => Precedence.let_
  | Forall(_, _) => Precedence.let_

  // Matt: I think multiholes are min because we don't know the precedence of the `⟩?⟨`s
  | Unknown({term: Hole(MultiHole(_)), _}) => Precedence.min
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

let paren_pat_assoc_at =
    (internal_precedence: Precedence.t, pat: Pat.t): Pat.t =>
  external_precedence_pat(pat) > internal_precedence
    ? Pat.fresh(Parens(pat)) : pat;

let paren_typ_at = (internal_precedence: Precedence.t, typ: Typ.t): Typ.t =>
  external_precedence_typ(typ) >= internal_precedence
    ? Typ.fresh(Parens(typ)) : typ;

let paren_typ_assoc_at =
    (internal_precedence: Precedence.t, typ: Typ.t): Typ.t =>
  external_precedence_typ(typ) > internal_precedence
    ? Typ.fresh(Parens(typ)) : typ;

let rec parenthesize =
        (~show_filters: bool, ~already_paren=false, exp: Exp.t): Exp.t => {
  let parenthesize = parenthesize(~show_filters);
  let parenthesize_pat = parenthesize_pat(~show_filters);
  let parenthesize_typ = parenthesize_typ(~show_filters);
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
  | BuiltinFun(_)
  | Tuple([])
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
  | TypFun(tp, e, n) =>
    TypFun(tp, parenthesize(e) |> paren_assoc_at(Precedence.fun_), n)
    |> rewrap
  | Tuple(es) =>
    let inner =
      Tuple(
        es |> List.map(parenthesize) |> List.map(paren_at(Precedence.prod)),
      )
      |> rewrap;

    if (already_paren) {
      inner;
    } else {
      Parens(inner) |> Exp.fresh;
    };
  | Label(_) => exp
  | TupLabel(l, e) =>
    TupLabel(l, parenthesize(e) |> paren_at(Precedence.min)) |> rewrap
  | Dot(e, l) =>
    Dot(parenthesize(e) |> paren_at(Precedence.min), l) |> rewrap
  | TupleExtension(l, r) =>
    TupleExtension(
      parenthesize(l) |> paren_at(Precedence.dot),
      parenthesize(r) |> paren_assoc_at(Precedence.dot),
    )
    |> rewrap
  | ListLit(es) =>
    ListLit(
      es |> List.map(parenthesize) |> List.map(paren_at(Precedence.prod)),
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
  | Probe(e, pr) =>
    Probe(
      parenthesize(~already_paren=true, e) |> paren_at(Precedence.min),
      pr,
    )
    |> rewrap
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
    MultiHole(List.map(parenthesize_any(~show_filters), xs)) |> rewrap
  };
}
and parenthesize_pat =
    (~show_filters: bool, ~already_paren=false, pat: Pat.t): Pat.t => {
  let parenthesize_pat = parenthesize_pat(~show_filters);
  let parenthesize_typ = parenthesize_typ(~show_filters);
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
  | Parens(p) =>
    Parens(
      parenthesize_pat(~already_paren=true, p)
      |> paren_pat_at(Precedence.min),
    )
    |> rewrap
  | Probe(p, pr) =>
    Probe(
      parenthesize_pat(~already_paren=true, p)
      |> paren_pat_at(Precedence.min),
      pr,
    )
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
        |> List.map(paren_pat_at(Precedence.prod)),
      )
      |> rewrap;
    already_paren ? inner : Parens(inner) |> Pat.fresh;
  | Label(_) => pat
  | TupLabel(l, p) =>
    TupLabel(l, parenthesize_pat(p) |> paren_pat_at(Precedence.min))
    |> rewrap
  | ListLit(ps) =>
    ListLit(
      ps
      |> List.map(parenthesize_pat)
      |> List.map(paren_pat_at(Precedence.prod)),
    )
    |> rewrap
  | Ap(p1, p2) =>
    Ap(
      parenthesize_pat(p1) |> paren_pat_assoc_at(Precedence.ap),
      parenthesize_pat(p2) |> paren_pat_at(Precedence.min),
    )
    |> rewrap
  | MultiHole(xs) =>
    MultiHole(List.map(parenthesize_any(~show_filters), xs)) |> rewrap
  | Asc(p, t) =>
    Asc(
      parenthesize_pat(p) |> paren_pat_assoc_at(Precedence.asc),
      parenthesize_typ(t) |> paren_typ_at(Precedence.max) // Hack[Matt]: always add parens to get the arrows right
    )
    |> rewrap
  };
}

and parenthesize_typ =
    (~show_filters: bool, ~already_paren=false, typ: Typ.t): Typ.t => {
  let parenthesize_typ = parenthesize_typ(~show_filters);
  let (term, rewrap) = Typ.unwrap(typ);
  switch (term) {
  // Indivisible forms dont' change
  | Var(_)
  | Unknown({term: Hole(Invalid(_)), _})
  | Unknown({term: Internal, _})
  | Unknown({term: SynSwitch, _})
  | Unknown({term: Hole(EmptyHole), _})
  | Unknown({term: Hole(CycleHole), _})
  | Unknown({term: LArrow(_), _})
  | Unknown({term: RArrow(_), _})
  | Unknown({term: NProduct(_), _})
  | Unknown({term: MList(_), _})
  | Unknown({term: RForall(_), _})
  | Unknown({term: TupLabel(_), _})
  | Unknown({term: TupLabelArg(_), _})
  | Unknown({term: Join(_), _})
  | Atom(_) => typ

  // Other forms
  | Parens(t) =>
    Parens(
      parenthesize_typ(~already_paren=true, t)
      |> paren_typ_at(Precedence.min),
    )
    |> rewrap
  | List(t) =>
    List(parenthesize_typ(t) |> paren_typ_at(Precedence.min)) |> rewrap
  | Prod([]) => typ
  | Prod(ts) =>
    let inner =
      Prod(
        ts
        |> List.map(parenthesize_typ)
        |> List.map(paren_typ_at(Precedence.comma)),
      )
      |> rewrap;
    already_paren ? inner : Parens(inner) |> Typ.fresh;
  | Label(_) => typ
  | TupLabel(l, t) =>
    TupLabel(l, parenthesize_typ(t) |> paren_typ_at(Precedence.min))
    |> rewrap
  | Rec(tp, t) =>
    Rec(
      tp,
      parenthesize_typ(t) |> paren_typ_assoc_at(Precedence.type_binder),
    )
    |> rewrap
  | Forall(tp, t) =>
    Forall(
      tp,
      parenthesize_typ(t) |> paren_typ_assoc_at(Precedence.type_binder),
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
  | Unknown({term: Hole(MultiHole(xs)), _}) =>
    Unknown(
      Hole(MultiHole(List.map(parenthesize_any(~show_filters), xs)))
      |> Prov.fresh,
    )
    |> rewrap
  };
}

and parenthesize_tpat = (~show_filters: bool, tpat: TPat.t): TPat.t => {
  let (term, rewrap: TPat.term => TPat.t) = IdTagged.unwrap(tpat);
  switch (term) {
  // Indivisible forms dont' change
  | Var(_)
  | Invalid(_)
  | EmptyHole => tpat

  // Other forms
  | MultiHole(xs) =>
    MultiHole(List.map(parenthesize_any(~show_filters), xs)) |> rewrap
  };
}

and parenthesize_rul = (~show_filters: bool, rul: Rul.t): Rul.t => {
  let (term, rewrap: Rul.term => Rul.t) = IdTagged.unwrap(rul);
  switch (term) {
  // Indivisible forms dont' change
  | Invalid(_) => rul

  // Other forms
  | Rules(e, ps) =>
    Rules(
      parenthesize(~show_filters, e),
      List.map(
        ((p, e)) =>
          (
            parenthesize_pat(~show_filters, p),
            parenthesize(~show_filters, e),
          ),
        ps,
      ),
    )
    |> rewrap
  | MultiHole(xs) =>
    MultiHole(List.map(parenthesize_any(~show_filters), xs)) |> rewrap
  };
}

and parenthesize_any =
    (~already_paren=false, ~show_filters: bool, any: Any.t): Any.t =>
  switch (any) {
  | Exp(e) => Exp(parenthesize(~already_paren, ~show_filters, e))
  | Pat(p) => Pat(parenthesize_pat(~already_paren, ~show_filters, p))
  | Typ(t) => Typ(parenthesize_typ(~already_paren, ~show_filters, t))
  | TPat(tp) => TPat(parenthesize_tpat(~show_filters, tp))
  | Rul(r) => Rul(parenthesize_rul(~show_filters, r))
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

let mk_form = (form_name: Form.compound_form, id, children): Piece.t => {
  let form: Form.t = Form.get(form_name);
  assert(List.length(children) == List.length(form.mold.in_));
  // Add whitespaces
  let children =
    Aba.map_abas(
      ((l, child, r)) => {
        let lspace = should_add_space(l, child |> Segment.first_string);
        let rspace = should_add_space(child |> Segment.last_string, r);
        (lspace ? [Secondary(mk_space(Id.mk()))] : [])
        @ (rspace ? child @ [Secondary(mk_space(Id.mk()))] : child);
      },
      Aba.mk(form.label, children),
    )
    |> Aba.get_bs;
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

let (@) = (seg1: Segment.t, seg2: Segment.t): Segment.t =>
  switch (seg1, seg2) {
  | ([], _) => seg2
  | (_, []) => seg1
  | _ =>
    if (should_add_space(
          Segment.last_string(seg1),
          Segment.first_string(seg2),
        )) {
      seg1 @ [Secondary(mk_space(Id.mk()))] @ seg2;
    } else {
      seg1 @ seg2;
    }
  };

let fold_if = (condition, pieces) =>
  if (condition) {
    let syntax = mk_form(ParensExp, Id.mk(), [pieces]);
    switch (MakeTerm.for_projection([syntax])) {
    | None => failwith("ExpToSegment.fold_if")
    | Some(any) => [ProjectorInit.init_or_noop(Fold, syntax, any)]
    };
  } else {
    pieces;
  };

let fold_fun_if = (condition, f_name: string, pieces) =>
  if (condition) {
    let syntax = mk_form(ParensExp, Id.mk(), [pieces]);
    let str = FoldProj.sexp_of_t({text: f_name}) |> Sexplib.Sexp.to_string;
    switch (MakeTerm.for_projection([syntax])) {
    | None => failwith("ExpToSegment.fold_fun_if")
    | Some(any) => [
        ProjectorInit.init_or_noop_from_str(Fold, syntax, any, str),
      ]
    };
  } else {
    pieces;
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
  switch (exp |> Exp.term_of) {
  // Assume these have been removed by the parenthesizer
  | DynamicErrorHole(_)
  | Filter(Residue(_), _) => failwith("printing these not implemented yet")
  | Filter(Filter({pat, act}), e) =>
    let id = exp |> Exp.rep_id;
    let* p = go(pat);
    let+ e = go(e);
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
      : e;
  // Forms which should be removed by substitute_closures
  | Closure(_) => failwith("closure not removed before printing")
  // Other cases
  | Invalid(x) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, x)
  | EmptyHole =>
    let id = exp |> Exp.rep_id;
    p_just([
      Grout({
        id,
        shape: Convex,
      }),
    ]);
  | Undefined => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "undefined")
  | Atom(c) =>
    text_to_pretty(exp |> Exp.rep_id, Sort.Exp, Atom.to_literal(c))
  // TODO: Make sure types are correct
  | Constructor(c, _t) =>
    // let id = Id.mk();
    let+ e = text_to_pretty(exp |> Exp.rep_id, Sort.Exp, c);
    // and+ t = typ_to_pretty(~settings: Settings.t, t);
    e;
  // @ [mk_form("typeasc", id, [])]
  // @ (t |> fold_if(settings.fold_cast_types));
  | ListLit([]) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "[]")
  | Deferral(_) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "_")
  | ListLit([x, ...xs]) =>
    // TODO: Add optional newlines
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
    p_just([form(x, xs)]);
  | Var(v) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, v)
  | BinOp(op, l, r) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ l = go(l)
    and+ r = go(r);
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
    @ r;
  | TupleExtension(l, r) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ l = go(l)
    and+ r = go(r);
    l
    @ [
      Tile({
        id,
        label: ["..."],
        mold: Mold.mk_bin(Precedence.dot, Sort.Exp, []),
        shards: [0],
        children: [],
      }),
    ]
    @ r;
  | MultiHole(es) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ es = es |> List.map(any_to_pretty(~settings)) |> all;
    ListUtil.flat_intersperse(
      Grout({
        id,
        shape: Concave,
      }),
      es,
    );
  | Parens({term: Fun(p, e, _, _), _} as inner_exp)
  | Probe({term: Fun(p, e, _, _), _} as inner_exp, _) =>
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
    [mk_form(ParensExp, exp |> Exp.rep_id, [fun_form])]
    |> fold_fun_if(settings.fold_fn_bodies, name);
  | LivelitName(s) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "^" ++ s)
  | Fun(p, e, t, _) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let p =
      switch (t) {
      | None => p
      | Some(t) =>
        let t = t |> Typ.replace_temp;
        Pat.fresh(Asc(p, t))
        |> parenthesize_pat(~show_filters=settings.show_filters);
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
    [mk_form(Fun, id, [p])]
    @ e
    |> fold_fun_if(settings.fold_fn_bodies, name);
  | TypFun(tp, e, _) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ tp = tpat_to_pretty(~settings: Settings.t, tp)
    and+ e = go(e);
    let name =
      "<"
      ++ (Exp.get_fn_name(exp) |> Option.value(~default="anon typfun"))
      ++ ">";
    [mk_form(TypFun, id, [tp])]
    @ e
    |> fold_fun_if(settings.fold_fn_bodies, name);
  | Tuple([]) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, "()")
  | Tuple([{term: TupLabel(_), _} as le]) => go(le)
  | Tuple([x, ...xs]) =>
    // TODO: Add optional newlines
    let+ x = go(x)
    and+ xs = xs |> List.map(go) |> all;
    let ids = IdTagged.ids(exp) |> pad_ids(List.length(xs));
    x
    @ List.flatten(
        List.map2((id, x) => [mk_form(CommaExp, id, [])] @ x, ids, xs),
      );
  | Label(l) =>
    label_to_pretty(
      ~label_only_position=false,
      Sort.Exp,
      Token.label_quote(l),
      exp |> Exp.rep_id,
    )
  | TupLabel(l, e) =>
    let* l =
      switch (l.term) {
      | Label(l') =>
        label_to_pretty(
          ~label_only_position=true,
          Sort.Exp,
          l',
          l |> Exp.rep_id,
        )
      | _ => go(l)
      }
    and* e = go(e);

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
    ]);
  | Dot(e, l) =>
    let* e = go(e)
    and* l =
      switch (l.term) {
      | Label(l') =>
        label_to_pretty(
          ~label_only_position=true,
          Sort.Exp,
          l',
          l |> Exp.rep_id,
        )
      | _ => go(l)
      };
    List.flatten([e, [mk_form(DotExp, exp |> Exp.rep_id, [])], l]);
  | Let(p, e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    // This step undoes the adding of fixpoints that happens in elaboration.
    let e1 = settings.hide_fixpoints ? Exp.unfix(e1, p) : e1;
    let+ p = pat_to_pretty(~settings: Settings.t, p)
    and+ e1 = go(e1)
    and+ e2 = go(e2);
    let e2 = settings.inline ? e2 : [Secondary(mk_newline(Id.mk()))] @ e2;
    [mk_form(Let, id, [p, e1])] @ e2;
  | FixF(p, e, _) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ p = pat_to_pretty(~settings: Settings.t, p)
    and+ e = go(e);
    let name =
      "<" ++ (Exp.get_fn_name(exp) |> Option.value(~default="fun")) ++ ">";
    [mk_form(Fix, id, [p])]
    @ e
    |> fold_fun_if(settings.fold_fn_bodies, name);
  | TyAlias(tp, t, e) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ tp = tpat_to_pretty(~settings: Settings.t, tp)
    and+ t = typ_to_pretty(~settings: Settings.t, t)
    and+ e = go(e);
    let e = settings.inline ? e : [Secondary(mk_newline(Id.mk()))] @ e;
    [mk_form(TypeAlias, id, [tp, t])] @ e;
  | Use(t, e) =>
    let id = exp |> Exp.rep_id;
    let+ t = typ_to_pretty(~settings: Settings.t, t)
    and+ e = go(e);
    let e = settings.inline ? e : [Secondary(mk_newline(Id.mk()))] @ e;
    [mk_form(Use, id, [t])] @ e;
  | Ap(Forward, e1, e2) =>
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    e1 @ [mk_form(ApExp, id, [e2])];
  | Ap(Reverse, e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
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
    @ e1;
  | TypAp(e, t) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e = go(e)
    and+ tp = typ_to_pretty(~settings: Settings.t, t);
    e @ [mk_form(ApExpTyp, id, [tp])];
  | DeferredAp(e, es) =>
    // TODO: Add optional newlines
    let+ e = go(e)
    and+ es = es |> List.map(go) |> all;
    let (id, ids) = (
      IdTagged.ids(exp) |> List.hd,
      IdTagged.ids(exp) |> List.tl |> pad_ids(List.length(es)),
    );
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
    ];
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
    [mk_form(If, id, [e1, e2])] @ e3;
  | Seq(e1, e2) =>
    // TODO: Make newline optional
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    let e2 = settings.inline ? e2 : [Secondary(mk_newline(Id.mk()))] @ e2;
    e1 @ [mk_form(CellJoin, id, [])] @ e2;
  | Test(e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    [mk_form(Test, id, [e])];
  | HintedTest(e, hint) =>
    let id = exp |> Exp.rep_id;
    let* hint = go(hint)
    and* e = go(e);
    [mk_form(HintedTest, id, [hint, e])];
  | Parens(e) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    [mk_form(ParensExp, id, [e])];
  | Probe(e, _) =>
    /* Not sure about this case*/
    go(e)
  | Cons(e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    e1 @ [mk_form(ConsExp, id, [])] @ e2;
  | ListConcat(e1, e2) =>
    // TODO: Add optional newlines
    let id = exp |> Exp.rep_id;
    let+ e1 = go(e1)
    and+ e2 = go(e2);
    e1 @ [mk_form(ListConcat, id, [])] @ e2;
  | UnOp(Meta(Unquote), e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    [mk_form(Unquote, id, [])] @ e;
  | UnOp(Bool(Not), e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    [mk_form(Not, id, [])] @ e;
  | UnOp(Int(Minus) | Nat(Minus) | Float(Minus) | SInt(Minus), e) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e);
    [mk_form(UnaryMinus, id, [])] @ e;
  /* TODO: this isn't actually correct because we could the builtin
     could have been overriden in this scope; worth fixing when we fix
     closures. */
  | BuiltinFun(f) => text_to_pretty(exp |> Exp.rep_id, Sort.Exp, f)
  | Asc(e, t) =>
    let id = exp |> Exp.rep_id;
    let+ e = go(e)
    and+ t = typ_to_pretty(~settings: Settings.t, t);
    e @ [mk_form(TypeAsc, id, [])] @ t;
  | Match(e, rs) =>
    // TODO: Add newlines
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
    ];
  };
}
and pat_to_pretty = (~settings: Settings.t, pat: Pat.t): pretty => {
  let go = pat_to_pretty(~settings: Settings.t);
  switch (pat |> Pat.term_of) {
  | Invalid(t) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, t)
  | EmptyHole =>
    let id = pat |> Pat.rep_id;
    p_just([
      Grout({
        id,
        shape: Convex,
      }),
    ]);
  | Wild => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "_")
  | Var(v) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, v)
  | Atom(c) =>
    text_to_pretty(pat |> Pat.rep_id, Sort.Pat, Atom.to_literal(c))
  | Constructor(c, _) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, c)
  | ListLit([]) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "[]")
  | ListLit([x, ...xs]) =>
    let* x = go(x)
    and* xs = xs |> List.map(go) |> all;
    let (id, ids) = (
      IdTagged.ids(pat) |> List.hd,
      IdTagged.ids(pat) |> List.tl |> pad_ids(List.length(xs)),
    );
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
    ]);
  | Cons(p1, p2) =>
    let id = pat |> Pat.rep_id;
    let+ p1 = go(p1)
    and+ p2 = go(p2);
    p1 @ [mk_form(ConsPat, id, [])] @ p2;
  | Tuple([]) => text_to_pretty(pat |> Pat.rep_id, Sort.Pat, "()")
  | Tuple([x, ...xs]) =>
    let+ x = go(x)
    and+ xs = xs |> List.map(go) |> all;
    let ids = IdTagged.ids(pat) |> pad_ids(List.length(xs));
    x
    @ List.flatten(
        List.map2((id, x) => [mk_form(CommaPat, id, [])] @ x, ids, xs),
      );
  | TupLabel(l, p) =>
    let* l =
      switch (l.term) {
      | Label(l') =>
        label_to_pretty(
          ~label_only_position=true,
          Sort.Pat,
          l',
          l |> Pat.rep_id,
        )
      | _ => go(l)
      }
    and* p = go(p);
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
    ]);
  | Label(l) =>
    text_to_pretty(pat |> Pat.rep_id, Sort.Pat, Token.label_quote(l))
  | Parens(p) =>
    let id = pat |> Pat.rep_id;
    let+ p = go(p);
    [mk_form(ParensPat, id, [p])];
  | Probe(p, _) =>
    /* Not sure about this case*/
    go(p)
  | MultiHole(es) =>
    let id = pat |> Pat.rep_id;
    let+ es = es |> List.map(any_to_pretty(~settings: Settings.t)) |> all;
    ListUtil.flat_intersperse(
      Grout({
        id,
        shape: Concave,
      }),
      es,
    );
  | Ap(p1, p2) =>
    let id = pat |> Pat.rep_id;
    let+ p1 = go(p1)
    and+ p2 = go(p2);
    p1 @ [mk_form(ApPat, id, [p2])];
  | Asc(p, t) =>
    let id = pat |> Pat.rep_id;
    let+ p = go(p)
    and+ t = typ_to_pretty(~settings: Settings.t, t);
    p @ [mk_form(Typeann, id, [])] @ t;
  };
}
and typ_to_pretty = (~settings: Settings.t, typ: Typ.t): pretty => {
  let go = typ_to_pretty(~settings: Settings.t);
  let go_constructor: ConstructorMap.variant(Typ.t) => pretty =
    fun
    | Variant(c, ids, None) => {
        text_to_pretty(
          Option.value(~default=Id.invalid, ListUtil.hd_opt(ids)),
          Sort.Typ,
          c,
        );
      }
    | Variant(c, ids, Some(x)) => {
        let+ constructor =
          text_to_pretty(
            Option.value(~default=Id.invalid, List.nth_opt(ids, 1)),
            Sort.Typ,
            c,
          );
        constructor
        @ [
          mk_form(
            ApTyp,
            Option.value(~default=Id.invalid, ListUtil.hd_opt(ids)),
            [go(x)],
          ),
        ];
      }
    | BadEntry(x) => go(x);
  switch (typ |> Typ.term_of) {
  | Unknown({term: Hole(Invalid(s)), _}) =>
    text_to_pretty(typ |> Typ.rep_id, Sort.Typ, s)
  | Unknown({term: Internal, _})
  | Unknown({term: SynSwitch, _})
  | Unknown({term: LArrow(_), _})
  | Unknown({term: RArrow(_), _})
  | Unknown({term: MList(_), _})
  | Unknown({term: NProduct(_), _})
  | Unknown({term: RForall(_), _})
  | Unknown({term: TupLabel(_), _})
  | Unknown({term: TupLabelArg(_), _})
  | Unknown({term: Join(_), _})
  | Unknown({term: Hole(EmptyHole), _}) // TOOD: (THI) need special cycle hole graphic
  | Unknown({term: Hole(CycleHole), _}) =>
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
    }
  | Unknown({term: Hole(MultiHole(es)), _}) =>
    let id = typ |> Typ.rep_id;
    let+ es = es |> List.map(any_to_pretty(~settings: Settings.t)) |> all;
    ListUtil.flat_intersperse(
      Grout({
        id,
        shape: Concave,
      }),
      es,
    );

  | Var(v) => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, v)
  | Atom(Int) => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Int")
  | Atom(SInt) => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "SInt")
  | Atom(Float) => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Float")
  | Atom(Bool) => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Bool")
  | Atom(String) => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "String")
  | Atom(Nat) => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "Nat")
  | List(t) =>
    let id = typ |> Typ.rep_id;
    let+ t = go(t);
    [mk_form(ListTyp, id, [t])];
  | Prod([]) => text_to_pretty(typ |> Typ.rep_id, Sort.Typ, "()")
  | Prod([t, ...ts]) =>
    let+ t = go(t)
    and+ ts = ts |> List.map(go) |> all;
    t
    @ List.flatten(
        List.map2(
          (id, t) => [mk_form(CommaTyp, id, [])] @ t,
          IdTagged.ids(typ) |> pad_ids(ts |> List.length),
          ts,
        ),
      );
  | Label(l) =>
    text_to_pretty(typ |> Typ.rep_id, Sort.Typ, Token.label_quote(l))
  | TupLabel(l, t) =>
    let+ l =
      switch (l.term) {
      | Label(l') =>
        label_to_pretty(
          ~label_only_position=true,
          Sort.Typ,
          l',
          l |> Typ.rep_id,
        )
      | _ => go(l)
      }
    and+ t = go(t);

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
    ]);
  | Parens(t) =>
    let id = typ |> Typ.rep_id;
    let+ t = go(t);
    [mk_form(ParensTyp, id, [t])];
  | Rec(tp, t) =>
    let id = typ |> Typ.rep_id;
    let+ tp = tpat_to_pretty(~settings: Settings.t, tp)
    and+ t = go(t);
    [mk_form(Rec, id, [tp])] @ t;
  | Forall(tp, t) =>
    let id = typ |> Typ.rep_id;
    let+ tp = tpat_to_pretty(~settings: Settings.t, tp)
    and+ t = go(t);
    [mk_form(Forall, id, [tp])] @ t;
  | Arrow(t1, t2) =>
    let id = typ |> Typ.rep_id;
    let+ t1 = go(t1)
    and+ t2 = go(t2);
    t1 @ [mk_form(TypeArrow, id, [])] @ t2;
  | Sum([]) => failwith("Empty Sums are not allowed")
  | Sum([t]) =>
    let id = typ |> Typ.rep_id;
    let+ t = go_constructor(t);
    [mk_form(TypSumSingle, id, [])] @ t;
  | Sum([t, ...ts]) =>
    let ids = IdTagged.ids(typ) |> pad_ids(List.length(ts) + 1);
    let id = List.hd(ids);
    let ids = List.tl(ids);
    let+ t = go_constructor(t)
    and+ ts = ts |> List.map(go_constructor) |> all;
    [mk_form(TypSumSingle, id, [])]
    @ t
    @ List.flatten(
        List.map2((id, t) => [mk_form(TypPlus, id, [])] @ t, ids, ts),
      );
  };
}
and tpat_to_pretty = (~settings: Settings.t, tpat: TPat.t): pretty => {
  switch (tpat |> IdTagged.term_of) {
  | Invalid(t) => text_to_pretty(tpat |> TPat.rep_id, Sort.TPat, t)
  | EmptyHole =>
    let id = tpat |> TPat.rep_id;
    p_just([
      Grout({
        id,
        shape: Convex,
      }),
    ]);
  | MultiHole(xs) =>
    let id = tpat |> TPat.rep_id;
    let+ xs = xs |> List.map(any_to_pretty(~settings: Settings.t)) |> all;
    ListUtil.flat_intersperse(
      Grout({
        id,
        shape: Concave,
      }),
      xs,
    );
  | Var(v) => text_to_pretty(tpat |> TPat.rep_id, Sort.TPat, v)
  };
}
and any_to_pretty = (~settings: Settings.t, any: Any.t): pretty => {
  switch (any) {
  | Exp(e) => exp_to_pretty(~settings: Settings.t, e)
  | Pat(p) => pat_to_pretty(~settings: Settings.t, p)
  | Typ(t) => typ_to_pretty(~settings: Settings.t, t)
  | TPat(tp) => tpat_to_pretty(~settings: Settings.t, tp)
  | Any(_)
  | Rul(_) =>
    //TODO: print out invalid rules properly
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
    (~label_only_position, sort: Sort.t, label: string, id: Uuidm.t): pretty => {
  text_to_pretty(
    id,
    sort,
    if (label_only_position) {
      Token.quote_label_when_necessary(label);
    } else {
      label;
    },
  );
};

let exp_to_segment =
    (~already_paren=false, ~settings: Settings.t, exp: Exp.t): Segment.t => {
  let exp =
    exp |> parenthesize(~already_paren, ~show_filters=settings.show_filters);
  let p = exp_to_pretty(~settings, exp);
  p |> PrettySegment.select;
};

let typ_to_segment = (~settings, typ: Typ.t): Segment.t => {
  let typ = parenthesize_typ(typ);
  let p = typ_to_pretty(~settings, typ(~show_filters=settings.show_filters));
  p |> PrettySegment.select;
};

let any_to_segment =
    (~already_paren=false, ~settings: Settings.t, any: Any.t): Segment.t => {
  let any =
    any
    |> parenthesize_any(~already_paren, ~show_filters=settings.show_filters);
  let p = any_to_pretty(~settings, any);
  p |> PrettySegment.select;
};
