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
    | (Asc(e, t'), t)
        // This is only necessary because sometimes we add two ascriptions and aren't marking it as a non-value
        when
          Typ.is_consistent(
            Ctx.empty,
            Typ.unroll(t |> Typ.temp),
            Typ.unroll(t'),
          ) =>
      switch (
        Typ.meet(Ctx.empty, Typ.unroll(t |> Typ.temp), Typ.unroll(t'))
      ) {
      | Some(t) => Some(recur(Asc(e, t) |> DHExp.fresh))
      | None => None //TODO  This is an impossible case since we checked consistency
      }
    | (e, Parens(t)) =>
      // This is an impossible case since types should be normalized before coming to transitions
      transition(~recursive, Asc(e |> DHExp.fresh, t) |> DHExp.fresh)
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
    | (TupLabel(l, inner), TupLabel(_l2, inner_ty)) =>
      // TODO Figure out what to do if the labels don't match
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          TupLabel(l, recur(Asc(inner, inner_ty) |> DHExp.fresh))
          |> DHExp.fresh,
        ),
      )
    | (Tuple(es), Prod(tys)) when List.length(es) == List.length(tys) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Tuple(
            List.map2(
              (e, ty) => recur(Asc(e, ty) |> DHExp.fresh),
              es,
              tys,
            ),
          )
          |> DHExp.fresh,
        ),
      )
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
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          ListLit(List.map(d => recur(Asc(d, ty) |> DHExp.fresh), ds))
          |> DHExp.fresh,
        ),
      )
    | (Cons(d1, d2), List(ty)) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Cons(
            recur(Asc(d1, ty) |> DHExp.fresh),
            recur(Asc(d2, t) |> DHExp.fresh),
          )
          |> DHExp.fresh,
        ),
      )
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
    | (If(cond, e1, e2), t) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          If(
            recur(cond),
            recur(Asc(e1, t |> Typ.temp) |> DHExp.fresh),
            recur(Asc(e2, t |> Typ.temp) |> DHExp.fresh),
          )
          |> DHExp.fresh,
        ),
      )
    | (Match(scrut, rules), t) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Match(
            scrut,
            List.map(
              ((p, body)) => (p, Asc(body, t |> Typ.temp) |> DHExp.fresh),
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
        Some(
          Ap(Forward, con, recur(Asc(payload, t') |> DHExp.fresh))
          |> DHExp.fresh,
        )
      | Some(None)
      | None => None
      };
    | (Constructor(_, Some(Some(t))), t')
        when Typ.is_consistent(Ctx.empty, Typ.unroll(t), t' |> Typ.temp) =>
      Some(e)
    | (ProofObject(e1), ProofOf(e2)) when Exp.fast_equal(e1, e2) =>
      Some(ProofObject(e1) |> DHExp.fresh)
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
        ListConcat(
          recur(Asc(d1, t) |> DHExp.fresh),
          recur(Asc(d2, t) |> DHExp.fresh),
        )
        |> DHExp.fresh,
      )
    | (Let(p, e1, e2), _) =>
      Some(Let(p, e1, Asc(e2, t) |> DHExp.fresh) |> DHExp.fresh)
    | (Seq(e1, e2), _) =>
      Some(Seq(e1, Asc(e2, t) |> DHExp.fresh) |> DHExp.fresh)
    | (Parens(e), _) =>
      Some(Parens(Asc(e, t) |> DHExp.fresh) |> DHExp.fresh)
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
    // These are handled above and must have the wrong type
    | (Atom(_), _)
    | (ListLit(_), _)
    | (ListConcat(_), _)
    | (TupLabel(_), _)
    | (Tuple(_), _)
    | (Fun(_), _)
    | (TypFun(_), _)
    | (Test(_), _)
    | (HintedTest(_), _)
    | (Cons(_), _)
    | (ProofObject(_), _)
    | (Constructor(_), _) => None
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
