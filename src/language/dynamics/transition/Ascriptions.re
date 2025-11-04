/* Ascription Transitions */

/*
 Handles the transition of type ascriptions.
 In the case of a stuck ascription, it will return None.

 Ascriptions should be propagated inside of expressions when consistent.
 e.g. [1, 2] : [Int] -> [1 : Int, 2 : Int]
 */
type closure_closures = list(Probe.call_stack => Dynamics.Probe.Closure.t);

module ClosureWriter =
  Util.WriterMonad.Make({
    type t = closure_closures;
    let empty = [];
    let append = (@);
  });

let rec transition =
        (~recursive=false, d: DHExp.t): option(ClosureWriter.t(DHExp.t)) => {
  let recur = (d: DHExp.t): ClosureWriter.t(DHExp.t) =>
    if (recursive) {
      transition(~recursive, d)
      |> Option.value(~default=ClosureWriter.return(d));
    } else {
      ClosureWriter.return(d);
    };
  switch (DHExp.term_of(d)) {
  | Asc(e, t) =>
    switch (DHExp.term_of(e), Typ.term_of(Typ.unroll(t))) {
    | (_, Probe(t', p)) =>
      Some(
        ClosureWriter.Syntax.(
          let* d =
            Asc(e, t')
            |> DHExp.fresh
            |> transition(~recursive)
            |> Option.value(
                 ~default=ClosureWriter.return(Asc(e, t') |> DHExp.fresh),
               );
          let* () =
            ClosureWriter.tell([
              Dynamics.Probe.Closure.mk(
                Typ.rep_id(t),
                e,
                Environment.empty,
                _,
                p,
              ),
            ]);
          ClosureWriter.return(d)
        ),
      )
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
      Some(
        ClosureWriter.Syntax.(
          let* d =
            transition(~recursive, Asc(d, t |> Typ.fresh) |> DHExp.fresh)
            |> Option.value(
                 ~default=
                   ClosureWriter.return(
                     Asc(d, t |> Typ.fresh) |> DHExp.fresh,
                   ),
               );
          ClosureWriter.return(Closure(ce, d) |> DHExp.fresh)
        ),
      )
    | (Fun(p, e, t, v), Arrow(t1, t2)) =>
      Some(
        ClosureWriter.return(
          IdTagged.FreshGrammar.(
            Exp.(fn(Pat.(asc(p, t1)), asc(e, t2), t, v))
          ),
        ),
      )
    | (TupLabel({term: ExplicitNonlabel, _}, e), _) =>
      Some(recur(Asc(e, t) |> DHExp.fresh))
    | (TupLabel(l, e), TupLabel(_l2, t)) =>
      // TODO Figure out what to do if the labels don't match
      Some(
        ClosureWriter.Syntax.(
          let* e = recur(Asc(e, t) |> DHExp.fresh);
          ClosureWriter.return(TupLabel(l, e) |> DHExp.fresh)
        ),
      )
    | (Tuple(es), Prod(tys)) when List.length(es) == List.length(tys) =>
      Some(
        ClosureWriter.Syntax.(
          let* es =
            List.map2((e, ty) => recur(Asc(e, ty) |> DHExp.fresh), es, tys)
            |> ClosureWriter.sequence;
          ClosureWriter.return(Tuple(es) |> DHExp.fresh)
        ),
      )
    | (_, Unknown(_)) =>
      Some(
        ClosureWriter.Syntax.(let* e = recur(e); ClosureWriter.return(e)),
      )
    | (Atom(value) as d, Atom(typ)) =>
      switch (value, typ) {
      | (Int(_), Int)
      | (String(_), String)
      | (Nat(_), Nat)
      | (Float(_), Float)
      | (SInt(_), SInt)
      | (Bool(_), Bool) => Some(ClosureWriter.return(d |> Exp.fresh))
      | (Int(_), _)
      | (String(_), _)
      | (Nat(_), _)
      | (Float(_), _)
      | (SInt(_), _)
      | (Bool(_), _) => None
      }
    | (ListLit(ds), List(ty)) =>
      Some(
        ClosureWriter.Syntax.(
          let* ds =
            List.map(d => recur(Asc(d, ty) |> DHExp.fresh), ds)
            |> ClosureWriter.sequence;
          ClosureWriter.return(ListLit(ds) |> DHExp.fresh)
        ),
      )
    | (Cons(d1, d2), List(ty)) =>
      Some(
        ClosureWriter.Syntax.(
          let* d1 = recur(Asc(d1, ty) |> DHExp.fresh);
          let* d2 = recur(Asc(d2, t) |> DHExp.fresh);
          ClosureWriter.return(Cons(d1, d2) |> DHExp.fresh)
        ),
      )
    | (TypFun(tp, e, v), Forall(tp', t')) =>
      let new_ty: Typ.t =
        switch (TPat.tyvar_of_utpat(tp)) {
        | Some(tyvar) => Var(tyvar) |> Typ.temp
        | None => Unknown(Internal) |> Typ.temp
        };
      Some(
        ClosureWriter.Syntax.(
          let* e = recur(Asc(e, Typ.subst(new_ty, tp', t')) |> DHExp.fresh);
          ClosureWriter.return(TypFun(tp, e, v) |> DHExp.fresh)
        ),
      );
    | (If(e, e1, e2), t) =>
      Some(
        ClosureWriter.Syntax.(
          let* e = recur(Asc(e, t |> Typ.temp) |> DHExp.fresh);
          let* e1 = recur(Asc(e1, t |> Typ.temp) |> DHExp.fresh);
          let+ e2 = recur(Asc(e2, t |> Typ.temp) |> DHExp.fresh);
          If(e, e1, e2) |> DHExp.fresh
        ),
      )
    | (Match(e, rules), t) =>
      Some(
        ClosureWriter.return(
          Match(
            e,
            List.map(
              ((p, e)) => (p, Asc(e, t |> Typ.temp) |> DHExp.fresh),
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
          ClosureWriter.Syntax.(
            let* e = recur(Asc(payload, t') |> DHExp.fresh);
            ClosureWriter.return(Ap(Forward, con, e) |> DHExp.fresh)
          ),
        )
      | Some(None)
      | None => None
      };
    | (Constructor(_, Some(Some(t))), t')
        when Typ.is_consistent(Ctx.empty, Typ.unroll(t), t' |> Typ.temp) =>
      Some(ClosureWriter.return(e))
    | (Test(_), Prod([])) => Some(ClosureWriter.return(e))
    // These are non-value cases we're handling to process ascriptions as early as possible
    | (BinOp(bin_op, _, _), _) =>
      switch (Operators.semantics_of_bin_op(bin_op)) {
      | DefinedPoly(Equals | NotEquals)
          when Typ.is_consistent(Ctx.empty, t, Atom(Bool) |> Typ.temp) =>
        Some(ClosureWriter.return(e))
      | Defined(_, _, ty_out, _)
          when
            Typ.is_consistent(
              Ctx.empty,
              t,
              Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp,
            ) =>
        Some(ClosureWriter.return(e))
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
        Some(ClosureWriter.return(e))
      | Undefined(_)
      | Defined(_) => None
      }
    | (ListConcat(d1, d2), List(_)) =>
      Some(
        ClosureWriter.Syntax.(
          let* e1 = recur(Asc(d1, t) |> DHExp.fresh);
          let* e2 = recur(Asc(d2, t) |> DHExp.fresh);
          ClosureWriter.return(ListConcat(e1, e2) |> DHExp.fresh)
        ),
      )
    | (Let(p, e1, e2), _) =>
      Some(
        ClosureWriter.return(
          Let(p, e1, Asc(e2, t) |> DHExp.fresh) |> DHExp.fresh,
        ),
      )
    | (Seq(e1, e2), _) =>
      Some(
        ClosureWriter.return(
          Seq(e1, Asc(e2, t) |> DHExp.fresh) |> DHExp.fresh,
        ),
      )
    | (Parens(e), _) =>
      Some(
        ClosureWriter.return(
          Parens(Asc(e, t) |> DHExp.fresh) |> DHExp.fresh,
        ),
      )
    // We _could_ do this, but it would be a bit weird
    | (Use(_), _) // I'm scaredto do Use because the type-directed literals might make this look weird in the stepper
    | (BuiltinFun(_), _)
    | (FixF(_), _)
    | (TypAp(_), _)
    | (Filter(_), _)
    | (TyAlias(_), _)
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
    | (Probe(_, _), _)
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
    | (Constructor(_), _) => None
    }
  | _ => None
  };
};

let rec transition_multiple = (d: DHExp.t): (closure_closures, DHExp.t) => {
  switch (transition(~recursive=true, d)) {
  | Some(writer_result) =>
    let (closures, d') = writer_result;
    let (c, d) = transition_multiple(d');
    (closures @ c, d);
  | None => ([], d)
  };
};
