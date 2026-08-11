open Language;

/* Rewrite generated terms to the shape print+parse would produce.
   Crash PBTs should keep the raw generator; only print/parse round-trips
   should run this before comparing. */

let atom = (a: Atom.t): Atom.t =>
  switch (a) {
  | Nat(n) => Int(n)
  | SInt(n) => Int(Bigint.of_int(n))
  | Float(f) =>
    switch (float_of_string_opt(Atom.to_literal(Float(f)))) {
    | Some(f') => Float(f')
    | None => Float(f)
    }
  | a => a
  };

/* `f((_))` is not a deferral — neither parser peels parens here. Only a
   direct `_` argument (or one behind an invisible Projector) defers. */
let rec is_deferral = (e: Exp.t): bool =>
  switch (e.term) {
  | Deferral(_) => true
  | Projector(_, inner) => is_deferral(inner)
  | _ => false
  };

let rec mark_in_ap = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Deferral(_) => {
      ...e,
      term: Deferral(InAp),
    }
  | Projector(data, inner) => {
      ...e,
      term: Projector(data, mark_in_ap(inner)),
    }
  | _ => e
  };

let is_tup_label = (e: Exp.t): bool =>
  switch (e.term) {
  | TupLabel(_) => true
  | _ => false
  };

let is_tup_label_pat = (p: Pat.t): bool =>
  switch (p.term) {
  | TupLabel(_) => true
  | _ => false
  };

/* `owner` is the tuple this field is being synthesized for; tagging the field
   with its ids is what lets a parent tuple recognize the wrap as ours
   (see is_own_wrapper). */
let unlabeled = (~owner: Exp.t, inner: Exp.t): Exp.t => {
  ...owner,
  term:
    TupLabel(
      {
        ...owner,
        term: ExplicitNonlabel,
      },
      inner,
    ),
};

let unlabeled_pat = (~owner: Pat.t, inner: Pat.t): Pat.t => {
  ...owner,
  term:
    TupLabel(
      {
        ...owner,
        term: ExplicitNonlabel,
      },
      inner,
    ),
};
/* A bare `lab=e` is wrapped into a singleton tuple below, because that is what
   the printer's parens reparse as. As a direct tuple field it must stay bare,
   so the wrap is undone here — but only our own wrap, which reuses the field's
   ids. A singleton tuple the term already had carries ids of its own and must
   survive: print+parse keeps `((h=_), _)` nested. */
let is_own_wrapper = (outer: IdTagged.t('a), inner: IdTagged.t('b)): bool =>
  IdTagged.ids(outer) == IdTagged.ids(inner);

let unwrap_labeled_singleton = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Tuple([{term: TupLabel(_), _} as tl]) when is_own_wrapper(e, tl) => tl
  | _ => e
  };

let unwrap_labeled_singleton_pat = (p: Pat.t): Pat.t =>
  switch (p.term) {
  | Tuple([{term: TupLabel(_), _} as tl]) when is_own_wrapper(p, tl) => tl
  | _ => p
  };

let unwrap_labeled_singleton_typ = (t: Typ.t): Typ.t =>
  switch (t.term) {
  | Prod([{term: TupLabel(_), _} as tl]) when is_own_wrapper(t, tl) => tl
  | _ => t
  };

/* `continue` rewrites a bare `_` to Deferral/Wild; restore when it's a
   TupLabel name so `_=e` / `_=p` stay ExplicitNonlabel. */
let restore_unlabel_exp = (lab: Exp.t): Exp.t =>
  switch (lab.term) {
  | Deferral(_) => {
      ...lab,
      term: ExplicitNonlabel,
    }
  | _ => lab
  };

let restore_unlabel_pat = (lab: Pat.t): Pat.t =>
  switch (lab.term) {
  | Wild => {
      ...lab,
      term: ExplicitNonlabel,
    }
  | _ => lab
  };

/* The `.` RHS is a field (Label or hole); singleton tuples are peeled as
   parens, anything else becomes a MultiHole. */
let rec peel_dot_rhs = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(inner)
  | Projector(_, inner) => peel_dot_rhs(inner)
  | Tuple([inner]) => peel_dot_rhs(inner)
  | _ => e
  };

let rec peel_proj_rhs = (t: Typ.t): Typ.t =>
  switch (t.term) {
  | Parens(inner)
  | Projector(_, inner) => peel_proj_rhs(inner)
  /* `(a=t)` reparses as Parens(Prod([TupLabel…])) — a labeled field is not
     a valid projection field, so the Prod wrapper survives. Keep it. */
  | Prod([{term: TupLabel(_), _}]) => t
  | Prod([inner]) => peel_proj_rhs(inner)
  | _ => t
  };

let rec as_dot_field = (e: Exp.t): Exp.t => {
  let e = peel_dot_rhs(e);
  switch (e.term) {
  | Label(_)
  | EmptyHole => e
  /* Conversion may already have wrapped a non-field in MultiHole; look
     through so we don't leave a Tuple([TupLabel]) inside. */
  | MultiHole([Exp(inner)]) => as_dot_field(inner)
  | MultiHole(_) => e
  | Var(s)
  | Constructor(s, _) => {
      ...e,
      term: Label(s),
    }
  | _ => {
      ...e,
      term: MultiHole([Exp(e)]),
    }
  };
};

let as_proj_field = (t: Typ.t): Typ.t => {
  let t = peel_proj_rhs(t);
  switch (t.term) {
  | Label(_) => t
  | Var(s) => {
      ...t,
      term: Label(s),
    }
  | _ => t
  };
};

/* `+ a` prints the same for BadEntry(Var("a")) and Variant("a"); parsers
   classify TypVar as Variant. */
let rec peel_typ_parens = (t: Typ.t): Typ.t =>
  switch (t.term) {
  | Parens(inner)
  | Projector(_, inner) => peel_typ_parens(inner)
  | _ => t
  };

let canon_sumterm = (v: ConstructorMap.variant(Typ.t)) =>
  switch (v) {
  | BadEntry(inner) =>
    switch (peel_typ_parens(inner).term) {
    | Var(name) =>
      ConstructorMap.Variant(
        name,
        ConstructorMap.mk_variant_ann(~ids=IdTagged.ids(inner), ()),
        None,
      )
    | _ => v
    }
  | Variant(_) => v
  };

let f_exp = (continue: Exp.t => Exp.t, e: Exp.t): Exp.t => {
  let e = continue(e);
  switch (e.term) {
  | BuiltinFun(name) => {
      ...e,
      term: Var(name),
    }
  | Atom(a) => {
      ...e,
      term: Atom(atom(a)),
    }
  | DynamicErrorHole(inner, _) => inner
  | ExplicitNonlabel => {
      ...e,
      term: Deferral(OutsideAp),
    }
  | Fun(p, body, _, _) => {
      ...e,
      term: Fun(p, body, None, None),
    }
  | TypFun(tp, body, _) => {
      ...e,
      term: TypFun(tp, body, None),
    }
  | Constructor(s, _) => {
      ...e,
      term: Constructor(s, None),
    }
  /* Forward only: `_ |> f` is a pipeline whose operand happens to be `_`,
     not a deferral. */
  | Ap(Forward, f, arg) when is_deferral(arg) => {
      ...e,
      term: DeferredAp(f, [mark_in_ap(arg)]),
    }
  | Ap(Forward, f, {term: Tuple(es), _}) when List.exists(is_deferral, es) => {
      ...e,
      term: DeferredAp(f, List.map(mark_in_ap, es)),
    }
  | DeferredAp(f, es) => {
      ...e,
      term: DeferredAp(f, List.map(mark_in_ap, es)),
    }
  | Tuple(es) =>
    let es = List.map(unwrap_labeled_singleton, es);
    switch (es) {
    /* Singleton unlabeled tuples print as `(_=e)`. */
    | [elt] when !is_tup_label(elt) => {
        ...e,
        term: Tuple([unlabeled(~owner=e, elt)]),
      }
    | es => {
        ...e,
        term: Tuple(es),
      }
    };
  | TupLabel(lab, value) =>
    let field: Exp.t = {
      ...e,
      term: TupLabel(restore_unlabel_exp(lab), value),
    };
    {
      ...e,
      term: Tuple([field]),
    };
  | Dot(e1, e2) => {
      ...e,
      term: Dot(e1, as_dot_field(e2)),
    }
  /* Parsing can only return a bare hole/invalid item as the Mod-sorted
     EmptyHole/Invalid. A generator item behind a print-invisible wrapper
     stays a ModExp, so promote it once the wrapper is stripped. */
  | Module(items) => {
      ...e,
      term:
        Module(
          List.map(
            (item: Mod.t): Mod.t =>
              switch (item.term) {
              | ModExp({term: EmptyHole, _}) => {
                  ...item,
                  term: EmptyHole,
                }
              | ModExp({term: Invalid(s), _}) => {
                  ...item,
                  term: Invalid(s),
                }
              | _ => item
              },
            items,
          ),
        ),
    }
  | _ => e
  };
};

let f_pat = (continue: Pat.t => Pat.t, p: Pat.t): Pat.t => {
  let p = continue(p);
  switch (p.term) {
  | Atom(a) => {
      ...p,
      term: Atom(atom(a)),
    }
  | ExplicitNonlabel => {
      ...p,
      term: Wild,
    }
  | Constructor(s, _) => {
      ...p,
      term: Constructor(s, None),
    }
  | Tuple(ps) =>
    let ps = List.map(unwrap_labeled_singleton_pat, ps);
    switch (ps) {
    | [elt] when !is_tup_label_pat(elt) => {
        ...p,
        term: Tuple([unlabeled_pat(~owner=p, elt)]),
      }
    | ps => {
        ...p,
        term: Tuple(ps),
      }
    };
  | TupLabel(lab, value) =>
    let field: Pat.t = {
      ...p,
      term: TupLabel(restore_unlabel_pat(lab), value),
    };
    {
      ...p,
      term: Tuple([field]),
    };
  | _ => p
  };
};

let f_typ = (continue: Typ.t => Typ.t, t: Typ.t): Typ.t => {
  let t = continue(t);
  switch (t.term) {
  | Prod(ts) => {
      ...t,
      term: Prod(List.map(unwrap_labeled_singleton_typ, ts)),
    }
  | TupLabel(_) => {
      ...t,
      term: Prod([t]),
    }
  | ProdProjection(t1, t2) => {
      ...t,
      term: ProdProjection(t1, as_proj_field(t2)),
    }
  | Sum(vs) => {
      ...t,
      term: Sum(List.map(canon_sumterm, vs)),
    }
  | _ => t
  };
};

let exp = (e: Exp.t): Exp.t =>
  TermBase.Exp.map_term(~f_exp, ~f_pat, ~f_typ, e);

let pat = (p: Pat.t): Pat.t =>
  TermBase.Pat.map_term(~f_exp, ~f_pat, ~f_typ, p);

let typ = (t: Typ.t): Typ.t =>
  TermBase.Typ.map_term(~f_exp, ~f_pat, ~f_typ, t);

/* Round-trip compare: canonicalize the generated term, ignore parens
   (Menhir drops `(e)`). */
let roundtrip_eq =
  Equality.(
    equality({
      ...syntactic_settings,
      ignore_parens: true,
      /* Projector wrappers are print-invisible, and upstream split this out
         of ignore_parens, so the round-trip comparison must set it too. */
      ignore_projectors: true,
    })
  );
