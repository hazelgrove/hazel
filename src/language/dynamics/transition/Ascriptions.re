/* Ascription Transitions */

/*
 Handles the transition of type ascriptions.
 In the case of a stuck ascription, it will return None.

 Ascriptions should be propagated inside of expressions when consistent.
 e.g. [1, 2] : [Int] -> [1 : Int, 2 : Int]

 ID PRESERVATION FOR PROBES:
 When an ascription transition transforms an expression, we preserve the original
 expression's ID. This is critical for the probe system, which tracks expressions
 by ID in probe_map.

 The general principle: any case that returns Some(...) should preserve the ID of
 the expression being ascribed (`e` from the outer `Asc(e, t)` pattern). We do this
 either by returning `Some(e)` directly, or by using `IdTagged.fast_copy(DHExp.rep_id(e), ...)`
 when constructing a new expression structure.
 */
/* Whether pushing an ascription through a list's or tuple's parts gave
   back the very same parts. Then the value already had the type -- plain
   data, well typed, no function to wrap -- and the push gives the value
   itself rather than a copy of it: no new nodes, and the same objects for
   anything downstream that keeps values by identity. Only a recursive
   push (transition_multiple) can give a part back unchanged; a stepwise
   one wraps each part in a new ascription. */
let unchanged = (ds: list(DHExp.t), ds': list(DHExp.t)): bool =>
  List.for_all2((d, d') => d === d', ds, ds');

let rec transition = (~recursive=false, d: DHExp.t): option(DHExp.t) => {
  let recur = (d: DHExp.t): DHExp.t =>
    if (recursive) {
      transition(~recursive, d) |> Option.value(~default=d);
    } else {
      d;
    };
  switch (DHExp.term_of(d)) {
  | Asc(e, t) =>
    switch (DHExp.term_of(e), Typ.term_of(Typ.unroll(t))) {
    | (Asc(e', t'), _)
        // This is only necessary because sometimes we add two ascriptions and aren't marking it as a non-value
        when Typ.is_consistent(Ctx.empty, Typ.unroll(t), Typ.unroll(t')) =>
      switch (Typ.meet(Ctx.empty, Typ.unroll(t), Typ.unroll(t'))) {
      | Some(t) =>
        Some(
          recur(
            IdTagged.fast_copy(DHExp.rep_id(e), Asc(e', t) |> DHExp.fresh),
          ),
        )
      | None => None //TODO  This is an impossible case since we checked consistency
      }
    | (_, Parens(t)) =>
      /* An impossible case: types are normalized before they come to
         transitions. e as it is, not a fresh copy: its id is kept, as
         everywhere here, and an e that already has the type comes back
         as itself. */
      transition(~recursive, Asc(e, t) |> DHExp.fresh)
    | (Closure(ce, d), t) =>
      transition(~recursive, Asc(d, t |> Typ.fresh) |> DHExp.fresh)
      |> Option.map(d => Closure(ce, d) |> DHExp.fresh)
    | (Fun(p, body, closure_ty, name), Arrow(t1, t2)) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          IdTagged.FreshGrammar.(
            Exp.(fn(Pat.(asc(p, t1)), asc(body, t2), closure_ty, name))
          ),
        ),
      )
    | (TupLabel({term: ExplicitNonlabel, _}, inner), _) =>
      Some(recur(Asc(inner, t) |> DHExp.fresh))
    /* Different labels: the value doesn't have the type, so the cast
       stays, as for every other mismatch below. Pushing it inside kept the
       value's label and silently changed the type (`(x=e) : (y=T)` stepped
       to `(x=e:T)`). */
    | (TupLabel({term: Label(a), _}, _), TupLabel({term: Label(b), _}, _))
        when a != b =>
      None
    | (TupLabel(l, inner), TupLabel(_l2, inner_ty)) =>
      let inner' = recur(Asc(inner, inner_ty) |> DHExp.fresh);
      inner' === inner
        ? Some(e)
        : Some(
            IdTagged.fast_copy(
              DHExp.rep_id(e),
              TupLabel(l, inner') |> DHExp.fresh,
            ),
          );
    | (Tuple(es), Prod(tys)) when List.length(es) == List.length(tys) =>
      let es' =
        List.map2((e, ty) => recur(Asc(e, ty) |> DHExp.fresh), es, tys);
      unchanged(es, es')
        ? Some(e)
        : Some(
            IdTagged.fast_copy(DHExp.rep_id(e), Tuple(es') |> DHExp.fresh),
          );
    | (_, Unknown(_)) => Some(e)
    | (Atom(value), Atom(typ)) =>
      switch (value, typ) {
      | (Int(_), Int)
      | (String(_), String)
      | (Nat(_), Nat)
      | (Float(_), Float)
      | (SInt(_), SInt)
      | (Bool(_), Bool) => Some(e)
      | (Int(_), _)
      | (String(_), _)
      | (Nat(_), _)
      | (Float(_), _)
      | (SInt(_), _)
      | (Bool(_), _) => None
      }
    | (ListLit(ds), List(ty)) =>
      let ds' = List.map(d => recur(Asc(d, ty) |> DHExp.fresh), ds);
      unchanged(ds, ds')
        ? Some(e)
        : Some(
            IdTagged.fast_copy(
              DHExp.rep_id(e),
              ListLit(ds') |> DHExp.fresh,
            ),
          );
    | (Cons(d1, d2), List(ty)) =>
      let d1' = recur(Asc(d1, ty) |> DHExp.fresh);
      let d2' = recur(Asc(d2, t) |> DHExp.fresh);
      d1' === d1 && d2' === d2
        ? Some(e)
        : Some(
            IdTagged.fast_copy(
              DHExp.rep_id(e),
              Cons(d1', d2') |> DHExp.fresh,
            ),
          );
    | (TypFun(tp, body, name), Poly(tp', t')) =>
      let new_ty: Typ.t =
        switch (TPat.tyvar_of_utpat(tp)) {
        | Some(tyvar) => Var(tyvar) |> Typ.temp
        | None => Unknown(Internal) |> Typ.temp
        };
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          TypFun(
            tp,
            recur(Asc(body, Typ.subst(new_ty, tp', t')) |> DHExp.fresh),
            name,
          )
          |> DHExp.fresh,
        ),
      );
    | (If(cond, e1, e2), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          If(
            recur(cond),
            recur(Asc(e1, t) |> DHExp.fresh),
            recur(Asc(e2, t) |> DHExp.fresh),
          )
          |> DHExp.fresh,
        ),
      )
    | (Match(scrut, rules), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Match(
            scrut,
            List.map(
              ((p, body)) => (p, Asc(body, t) |> DHExp.fresh),
              rules,
            ),
          )
          |> DHExp.fresh,
        ),
      )
    | (
        Ap(
          Forward,
          {term: Constructor(c, Some(Some({term: Arrow(_, sumt), _}))), _} as con,
          payload,
        ),
        Sum(m) as sumt',
      )
        when
          Typ.is_consistent(Ctx.empty, Typ.unroll(sumt), sumt' |> Typ.temp) =>
      let entry = ConstructorMap.get_entry(c, m);
      switch (entry) {
      | Some(Some(t')) =>
        let payload' = recur(Asc(payload, t') |> DHExp.fresh);
        payload' === payload
          ? Some(e)
          : Some(
              IdTagged.fast_copy(
                DHExp.rep_id(e),
                Ap(Forward, con, payload') |> DHExp.fresh,
              ),
            );
      | Some(None)
      | None => None
      };
    | (Constructor(_, Some(Some(t))), t')
        when Typ.is_consistent(Ctx.empty, Typ.unroll(t), t' |> Typ.temp) =>
      Some(e)
    | (ProofObject(e1), ProofOf(e2)) when Exp.fast_equal(e1, e2) =>
      Some(
        IdTagged.fast_copy(DHExp.rep_id(e), ProofObject(e1) |> DHExp.fresh),
      )
    | (Test(_), Prod([])) => Some(e)
    // These are non-value cases we're handling to process ascriptions as early as possible
    | (BinOp(bin_op, _, _), _) =>
      switch (Operators.semantics_of_bin_op(bin_op)) {
      | DefinedPoly(Equals | NotEquals)
          when Typ.is_consistent(Ctx.empty, t, Atom(Bool) |> Typ.temp) =>
        Some(e)
      | Defined(_, _, ty_out, _)
          when
            Typ.is_consistent(
              Ctx.empty,
              t,
              Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp,
            ) =>
        Some(e)
      | Undefined(_)
      | DefinedPoly(_)
      | Defined(_) => None
      }
    | (UnOp(un_op, _), _) =>
      switch (Operators.semantics_of_un_op(un_op)) {
      | Defined(_, ty_out, _)
          when
            Typ.is_consistent(
              Ctx.empty,
              t,
              Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp,
            ) =>
        Some(e)
      | Undefined(_)
      | Defined(_) => None
      }
    | (ListConcat(d1, d2), List(_)) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          ListConcat(
            recur(Asc(d1, t) |> DHExp.fresh),
            recur(Asc(d2, t) |> DHExp.fresh),
          )
          |> DHExp.fresh,
        ),
      )
    | (Let(p, e1, e2), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Let(p, e1, Asc(e2, t) |> DHExp.fresh) |> DHExp.fresh,
        ),
      )
    | (Seq(e1, e2), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Seq(e1, Asc(e2, t) |> DHExp.fresh) |> DHExp.fresh,
        ),
      )
    | (Parens(pe), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Parens(Asc(pe, t) |> DHExp.fresh) |> DHExp.fresh,
        ),
      )
    | (Projector(data, pe), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Projector(data, Asc(pe, t) |> DHExp.fresh) |> DHExp.fresh,
        ),
      )
    // We _could_ do this, but it would be a bit weird
    | (Use(_), _) // I'm scaredto do Use because the type-directed literals might make this look weird in the stepper
    | (BuiltinFun(_), _)
    | (FixF(_), _)
    | (TypAp(_), _)
    | (Filter(_), _)
    | (TyAlias(_), _)
    | (Theorem(_), _)
    | (Forall(_), _)
    | (Asc(_), _) => None
    // These are non-value cases we don't want to handle
    | (EmptyHole, _)
    | (DynamicErrorHole(_), _)
    | (Dot(_), _)
    | (Undefined, _)
    | (Invalid(_), _)
    | (MultiHole(_), _)
    | (Label(_), _)
    | (ExplicitNonlabel, _)
    | (Var(_), _)
    | (Ap(_), _)
    | (DeferredAp(_), _)
    | (Deferral(_), _)
    | (LivelitName(_), _)
    | (TupleExtension(_, _), _)
    | (ListConcat(_), _) => None
    // These are handled above and must have the wrong type
    | (Atom(_), _)
    | (DrvQuote(_), _)
    | (ListLit(_), _)
    | (TupLabel(_), _)
    | (Tuple(_), _)
    | (Fun(_), _)
    | (TypFun(_), _)
    | (Test(_), _)
    | (HintedTest(_), _)
    | (Cons(_), _)
    | (ProofObject(_), _)
    | (Constructor(_), _)
    | (Module(_), _)
    | (ModuleExp(_), _) => None
    }
  | _ => None
  };
};

let rec transition_multiple = (d: DHExp.t): DHExp.t => {
  switch (transition(~recursive=true, d)) {
  | Some(d'') => transition_multiple(d'')
  | None => d
  };
};
