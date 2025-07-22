/* Ascription Transitions */

/*
 Handles the transition of type ascriptions.
 In the case of a stuck ascription, it will return None.

 Ascriptions should be propagated inside of expressions when consistent.
 e.g. [1, 2] : [Int] -> [1 : Int, 2 : Int]
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
        Typ.join(Ctx.empty, Typ.unroll(t |> Typ.temp), Typ.unroll(t'))
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
    | (Fun(p, e, t, v), Arrow(t1, t2)) =>
      Some(
        IdTagged.FreshGrammar.(
          Exp.(fn(Pat.(asc(p, t1)), asc(e, t2), t, v))
        ),
      )
    | (TupLabel(l, e), TupLabel(_l2, t)) =>
      // TODO Figure out what to do if the labels don't match
      Some(TupLabel(l, recur(Asc(e, t) |> DHExp.fresh)) |> DHExp.fresh)
    | (Tuple(es), Prod(tys)) when List.length(es) == List.length(tys) =>
      Some(
        Tuple(
          List.map2((e, ty) => recur(Asc(e, ty) |> DHExp.fresh), es, tys),
        )
        |> DHExp.fresh,
      )
    | (e, Unknown(_)) => Some(e |> DHExp.fresh)
    | (Atom(value) as d, Atom(typ)) =>
      switch (value, typ) {
      | (Int(_), Int)
      | (String(_), String)
      | (Nat(_), Nat)
      | (Float(_), Float)
      | (SInt(_), SInt)
      | (Bool(_), Bool) => Some(d |> Exp.fresh)
      | (Int(_), _)
      | (String(_), _)
      | (Nat(_), _)
      | (Float(_), _)
      | (SInt(_), _)
      | (Bool(_), _) => None
      }
    | (ListLit(ds), List(ty)) =>
      Some(
        ListLit(List.map(d => recur(Asc(d, ty) |> DHExp.fresh), ds))
        |> DHExp.fresh,
      )
    | (Cons(d1, d2), List(ty)) =>
      Some(
        Cons(
          recur(Asc(d1, ty) |> DHExp.fresh),
          recur(Asc(d2, t) |> DHExp.fresh),
        )
        |> DHExp.fresh,
      )
    | (TypFun(tp, e, v), Poly(tp', t')) =>
      let new_ty: Typ.t =
        switch (TPat.tyvar_of_utpat(tp)) {
        | Some(tyvar) => Var(tyvar) |> Typ.temp
        | None => Unknown(Internal) |> Typ.temp
        };
      Some(
        TypFun(
          tp,
          recur(Asc(e, Typ.subst(new_ty, tp', t')) |> DHExp.fresh),
          v,
        )
        |> DHExp.fresh,
      );
    | (If(e, e1, e2), t) =>
      Some(
        If(
          recur(Asc(e, t |> Typ.temp) |> DHExp.fresh),
          recur(Asc(e1, t |> Typ.temp) |> DHExp.fresh),
          recur(Asc(e2, t |> Typ.temp) |> DHExp.fresh),
        )
        |> DHExp.fresh,
      )
    | (Match(e, rules), t) =>
      Some(
        Match(
          e,
          List.map(
            ((p, e)) => (p, Asc(e, t |> Typ.temp) |> DHExp.fresh),
            rules,
          ),
        )
        |> DHExp.fresh,
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
    | (Test(_), Prod([])) => Some(e)
    // These are non-value cases we don't want to handle
    | (EmptyHole, _)
    | (DynamicErrorHole(_), _)
    | (Dot(_), _)
    | (Undefined, _)
    | (Invalid(_), _)
    | (MultiHole(_), _)
    | (Label(_), _)
    | (Var(_), _)
    | (Ap(_), _)
    | (DeferredAp(_), _)
    | (Deferral(_), _)
    | (LivelitName(_), _)
    | (Probe(_, _), _)
    // We _could_ do this, but it would be a bit weird
    | (Let(_), _)
    | (Use(_), _)
    | (BinOp(_), _)
    | (UnOp(_), _)
    | (BuiltinFun(_), _)
    | (FixF(_), _)
    | (TypAp(_), _)
    | (Seq(_), _)
    | (Filter(_), _)
    | (Parens(_), _)
    | (TyAlias(_), _)
    | (ListConcat(_), _)
    | (Asc(_), _) => None
    // These are handled above and must have the wrong type
    | (Atom(_), _)
    | (ListLit(_), _)
    | (TupLabel(_), _)
    | (Tuple(_), _)
    | (Fun(_), _)
    | (TypFun(_), _)
    | (Test(_), _)
    | (HintedTest(_), _)
    | (Cons(_), _)
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
