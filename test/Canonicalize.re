open Language;

/* Rewrite generated terms to the shape print+parse would produce: the
   generator also emits shapes no parse returns (a bare `a=e`, a one-element
   tuple, an `SInt` literal outside `use SInt`), and some of them take
   different paths through statics and dynamics.

   - Print/parse round-trips run this on the generated side before comparing.
   - Semantic properties (elaboration preserves type, sub-expression
     synthesis, preservation, stepper consistency, pattern equivalence) run
     it first, so they judge only terms a user can write.
   - Crash properties keep the raw generator: crashing on any term is a bug.
   - Id-keyed properties (elaboration ids, incremental evaluation) keep it
     raw too: they compare a term's ids across runs, which any term
     exercises. The nested-id property is the exception: it runs on
     canonical terms, as a generated constructor's type payload drifts by
     a path no user term takes.
   The nodes a rewrite adds take ids derived from the node they wrap, so a
   canonical term still has distinct ids, as a parsed one does. */

let atom = (a: Atom.t): Atom.t =>
  switch (a) {
  | Nat(n) => Int(n)
  | SInt(n) => Int(Bigint.of_int(n))
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

/* The nodes a wrap adds take ids derived from the tuple's, so no two nodes
   share an id and a parent tuple can still recognize the wrap of a bare
   `lab=e` as ours (see is_own_wrapper). A genuine one-element tuple's
   `_=` field takes another salt: it is a real tuple, which a parent keeps. */
let derived_ids = (~salt, owner: IdTagged.t('a)): IdTagged.IdTag.t => {
  ...owner.annotation,
  ids: List.map(~f=Id.derive(~salt), IdTagged.ids(owner)),
};
let field_ids = owner => derived_ids(~salt="canonical-field", owner);

/* `owner` is the tuple this field is being synthesized for. */
let unlabeled = (~owner: Exp.t, inner: Exp.t): Exp.t => {
  annotation: derived_ids(~salt="canonical-unlabeled-field", owner),
  term:
    TupLabel(
      {
        annotation: derived_ids(~salt="canonical-nonlabel", owner),
        term: ExplicitNonlabel,
      },
      inner,
    ),
};

let unlabeled_pat = (~owner: Pat.t, inner: Pat.t): Pat.t => {
  annotation: derived_ids(~salt="canonical-unlabeled-field", owner),
  term:
    TupLabel(
      {
        annotation: derived_ids(~salt="canonical-nonlabel", owner),
        term: ExplicitNonlabel,
      },
      inner,
    ),
};
/* A bare `lab=e` is wrapped into a singleton tuple below, because that is what
   the printer's parens reparse as. As a direct tuple field it must stay bare,
   so the wrap is undone here, giving the field back the ids it had — but only
   our own wrap. A singleton tuple the term already had carries ids of its own
   and must survive: print+parse keeps `((h=_), _)` nested. */
let is_own_wrapper = (outer: IdTagged.t('a), inner: IdTagged.t('a)): bool =>
  List.equal(Id.equal, IdTagged.ids(inner), field_ids(outer).ids);

let unwrap_labeled_singleton = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Tuple([{term: TupLabel(_), _} as tl]) when is_own_wrapper(e, tl) => {
      ...tl,
      annotation: e.annotation,
    }
  | _ => e
  };

let unwrap_labeled_singleton_pat = (p: Pat.t): Pat.t =>
  switch (p.term) {
  | Tuple([{term: TupLabel(_), _} as tl]) when is_own_wrapper(p, tl) => {
      ...tl,
      annotation: p.annotation,
    }
  | _ => p
  };

let unwrap_labeled_singleton_typ = (t: Typ.t): Typ.t =>
  switch (t.term) {
  | Prod([{term: TupLabel(_), _} as tl]) when is_own_wrapper(t, tl) => {
      ...tl,
      annotation: t.annotation,
    }
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

/* The `.` RHS is a field (Label or hole) only when it is one bare: neither
   parser looks through parens there, so `x.(y)` is a MultiHole around the
   parenthesized tip. Projectors are print-invisible, so they are peeled. */
let rec peel_dot_rhs = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Projector(_, inner) => peel_dot_rhs(inner)
  | _ => e
  };

let rec peel_proj_rhs = (t: Typ.t): Typ.t =>
  switch (t.term) {
  | Projector(_, inner) => peel_proj_rhs(inner)
  /* `(a=t)` reparses as Parens(Prod([TupLabel…])) — a labeled field is not
     a valid projection field, so the Prod wrapper survives. Keep it. */
  | Prod([{term: TupLabel(_), _}]) => t
  | Prod([inner]) => peel_proj_rhs(inner)
  | _ => t
  };

let as_dot_field = (e: Exp.t): Exp.t => {
  let e = peel_dot_rhs(e);
  switch (e.term) {
  | Label(_)
  | EmptyHole
  /* Conversion has already wrapped a generated non-field. */
  | MultiHole(_) => e
  | Var(s)
  | Constructor(s, _) => {
      ...e,
      term: Label(s),
    }
  | _ => {
      annotation: derived_ids(~salt="canonical-dot-field", e),
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

let rec peel_typ_parens = (t: Typ.t): Typ.t =>
  switch (t.term) {
  | Parens(inner)
  | Projector(_, inner) => peel_typ_parens(inner)
  | _ => t
  };

/* `+ a` prints the same for BadEntry(Var("a")) and Variant("a"); parsers
   classify TypVar as Variant. */
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
  | Ap(Forward, f, {term: Tuple(es), _})
      when List.exists(~f=is_deferral, es) => {
      ...e,
      term: DeferredAp(f, List.map(~f=mark_in_ap, es)),
    }
  /* A labeled argument is an argument, not a one-field record: undo the
     wrap the TupLabel arm below gave it, as the Tuple arm does for fields. */
  | DeferredAp(f, es) => {
      ...e,
      term:
        DeferredAp(
          f,
          List.map(~f=e => mark_in_ap(unwrap_labeled_singleton(e)), es),
        ),
    }
  | Tuple(es) =>
    let es = List.map(~f=unwrap_labeled_singleton, es);
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
      annotation: field_ids(e),
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
            ~f=
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
    let ps = List.map(~f=unwrap_labeled_singleton_pat, ps);
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
      annotation: field_ids(p),
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
      term: Prod(List.map(~f=unwrap_labeled_singleton_typ, ts)),
    }
  | TupLabel(_) => {
      ...t,
      term:
        Prod([
          {
            ...t,
            annotation: field_ids(t),
          },
        ]),
    }
  | ProdProjection(t1, t2) => {
      ...t,
      term: ProdProjection(t1, as_proj_field(t2)),
    }
  | Sum(vs) => {
      ...t,
      term: Sum(List.map(~f=canon_sumterm, vs)),
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

/* An elaborated or evaluated term in the shape its print reads back as.
   Elaboration writes in types (a function's argument type, which prints as
   an ascription, and a constructor's); turns `module M = e in body` into a
   `let` of the variable `M`, and the module's uses into variables, all
   written as constructors; unwraps the hole a non-name after `.` reads as;
   and makes `?` and unbound type variables into holes of other kinds. */
let elaborated = (e: Exp.t): Exp.t => {
  let f_exp = (continue: Exp.t => Exp.t, e: Exp.t): Exp.t => {
    let e = continue(e);
    switch (e.term) {
    | BuiltinFun(name) => {
        ...e,
        term: Var(name),
      }
    | DynamicErrorHole(inner, _) => inner
    | Fun(p, body, t, _) => {
        ...e,
        term:
          Fun(
            switch (t) {
            | Some(t) => Pat.fresh(Asc(p, t))
            | None => p
            },
            body,
            None,
            None,
          ),
      }
    | Let({term: Var(name), _} as p, def, body) when Token.is_ctr(name) => {
        ...e,
        term:
          ModuleExp(
            {
              ...p,
              term: Var(name),
            },
            def,
            body,
          ),
      }
    | Var(name) when Token.is_ctr(name) => {
        ...e,
        term: Constructor(name, None),
      }
    | Constructor(name, Some(_)) => {
        ...e,
        term: Constructor(name, None),
      }
    | Dot(_, {term: Label(_) | Var(_) | Constructor(_) | EmptyHole, _}) => e
    | Dot(l, r) => {
        ...e,
        term: Dot(l, Exp.fresh(MultiHole([Exp(r)]))),
      }
    | _ => e
    };
  };
  let f_typ = (continue: Typ.t => Typ.t, t: Typ.t): Typ.t => {
    let t = continue(t);
    switch (t.term) {
    | Unknown(SynSwitch | Internal) => {
        ...t,
        term: Unknown(Hole(EmptyHole)),
      }
    | Unknown(Hole(Invalid(name))) when Token.is_typ_var(name) => {
        ...t,
        term: Var(name),
      }
    | _ => t
    };
  };
  let f_pat = (continue: Pat.t => Pat.t, p: Pat.t): Pat.t => {
    let p = continue(p);
    switch (p.term) {
    | Constructor(name, Some(_)) => {
        ...p,
        term: Constructor(name, None),
      }
    | _ => p
    };
  };
  TermBase.Exp.map_term(~f_exp, ~f_pat, ~f_typ, e);
};

/* Round-trip compare: ignore parens (Menhir drops `(e)`). */
let roundtrip_eq =
  Equality.(
    equality({
      ...syntactic_settings,
      ignore_parens: true,
      /* Projector wrappers are print-invisible. */
      ignore_projectors: true,
      /* Evaluated lists carry their element type, which text can't write. */
      ignore_list_types: true,
    })
  );
