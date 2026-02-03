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
type closure_closures = list((Sample.call_stack, int, int) => Sample.t);
module ClosureWriter =
  Util.WriterMonad.Make({
    type t = closure_closures;
    let empty = [];
    let append = (@);
  });

/* Record a sample for a probed type.
 * When a type has a probe on it (i.e., its ID is in targets), and we're
 * processing an ascription, we record the value being ascribed as a sample
 * for that type's probe. */
let record_type_probe =
    (~targets: Sample.targets, typ: Typ.t, value: DHExp.t)
    : ClosureWriter.t(unit) => {
  let typ_id = Typ.rep_id(typ);
  switch (Id.Map.find_opt(typ_id, targets)) {
  | Some(spec) =>
    ClosureWriter.tell([
      (call_stack: Sample.call_stack, step_start: int, step_end: int) =>
        Sample.mk(
          ~step_start,
          ~step_end,
          typ_id,
          value,
          Environment.empty,
          call_stack,
          spec,
        ),
    ])
  | None => ClosureWriter.return()
  };
};

let rec transition =
        (~recursive=false, ~targets: Sample.targets, d: DHExp.t)
        : ClosureWriter.t(option(DHExp.t)) => {
  open ClosureWriter.Syntax;
  let recur = (d: DHExp.t): ClosureWriter.t(DHExp.t) =>
    if (recursive) {
      let+ d' = transition(~recursive, ~targets, d);
      Option.value(~default=d, d');
    } else {
      ClosureWriter.return(d);
    };
  switch (DHExp.term_of(d)) {
  | Asc(e, t) =>
    /* Record sample if this type is probed */
    let* () = record_type_probe(~targets, t, e);
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
      | Some(t) =>
        let+ d' = recur(Asc(e, t) |> DHExp.fresh);
        Some(d');
      | None => ClosureWriter.return(None) //TODO  This is an impossible case since we checked consistency
      }
    | (e, Parens(t)) =>
      // This is an impossible case since types should be normalized before coming to transitions
      transition(
        ~recursive,
        ~targets,
        Asc(e |> DHExp.fresh, t) |> DHExp.fresh,
      )
    | (Closure(ce, d), t) =>
      let+ d' =
        transition(
          ~recursive,
          ~targets,
          Asc(d, t |> Typ.fresh) |> DHExp.fresh,
        );
      Option.map(d => Closure(ce, d) |> DHExp.fresh, d');
    | (Fun(p, body, closure_ty, name), Arrow(t1, t2)) =>
      ClosureWriter.return(
        Some(
          IdTagged.fast_copy(
            DHExp.rep_id(e),
            IdTagged.FreshGrammar.(
              Exp.(fn(Pat.(asc(p, t1)), asc(body, t2), closure_ty, name))
            ),
          ),
        ),
      )
    | (TupLabel({term: ExplicitNonlabel, _}, inner), _) =>
      let+ d = recur(Asc(inner, t) |> DHExp.fresh);
      Some(d);
    | (TupLabel(l, inner), TupLabel(_l2, inner_ty)) =>
      let+ inner = recur(Asc(inner, inner_ty) |> DHExp.fresh);
      // TODO Figure out what to do if the labels don't match
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          TupLabel(l, inner) |> DHExp.fresh,
        ),
      );
    | (Tuple(es), Prod(tys)) when List.length(es) == List.length(tys) =>
      let+ es =
        List.map2((e, ty) => {recur(Asc(e, ty) |> DHExp.fresh)}, es, tys)
        |> ClosureWriter.sequence;
      Some(IdTagged.fast_copy(DHExp.rep_id(e), Tuple(es) |> DHExp.fresh));
    | (_, Unknown(_)) =>
      let+ e = recur(e);
      Some(e);
    | (Cons(d1, d2), List(ty)) =>
      let* d1 = recur(Asc(d1, ty) |> DHExp.fresh);
      let+ d2 = recur(Asc(d2, t) |> DHExp.fresh);
      Some(
        IdTagged.fast_copy(DHExp.rep_id(e), Cons(d1, d2) |> DHExp.fresh),
      );
    | (Atom(value), Atom(typ)) =>
      ClosureWriter.return(
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
        },
      )
    | (ListLit(ds), List(ty)) =>
      let+ ds =
        List.map(d => recur(Asc(d, ty) |> DHExp.fresh), ds)
        |> ClosureWriter.sequence;

      Some(
        IdTagged.fast_copy(DHExp.rep_id(e), ListLit(ds) |> DHExp.fresh),
      );

    | (TypFun(tp, body, name), Poly(tp', t')) =>
      let new_ty: Typ.t =
        switch (TPat.tyvar_of_utpat(tp)) {
        | Some(tyvar) => Var(tyvar) |> Typ.temp
        | None => Unknown(Internal) |> Typ.temp
        };

      let+ body' =
        recur(Asc(body, Typ.subst(new_ty, tp', t')) |> DHExp.fresh);
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          TypFun(tp, body', name) |> DHExp.fresh,
        ),
      );

    | (If(cond, e1, e2), t) =>
      let* cond = recur(cond);
      let* e1 = recur(Asc(e1, t |> Typ.temp) |> DHExp.fresh);
      let+ e2 = recur(Asc(e2, t |> Typ.temp) |> DHExp.fresh);
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          If(cond, e1, e2) |> DHExp.fresh,
        ),
      );
    | (Match(scrut, rules), t) =>
      ClosureWriter.return(
        Some(
          IdTagged.fast_copy(
            DHExp.rep_id(e),
            Match(
              scrut,
              List.map(
                ((p, body)) =>
                  (p, Asc(body, t |> Typ.temp) |> DHExp.fresh),
                rules,
              ),
            )
            |> DHExp.fresh,
          ),
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
        let+ e = recur(Asc(payload, t') |> DHExp.fresh);
        Some(Ap(Forward, con, e) |> DHExp.fresh);
      | Some(None)
      | None => ClosureWriter.return(None)
      };
    | (Constructor(_, Some(Some(t))), t')
        when Typ.is_consistent(Ctx.empty, Typ.unroll(t), t' |> Typ.temp) =>
      ClosureWriter.return(Some(e))
    | (ProofObject(e1), ProofOf(e2)) when Exp.fast_equal(e1, e2) =>
      ClosureWriter.return(Some(ProofObject(e1) |> DHExp.fresh))
    | (Test(_), Prod([])) => ClosureWriter.return(Some(e))
    // These are non-value cases we're handling to process ascriptions as early as possible
    | (BinOp(bin_op, _, _), _) =>
      switch (Operators.semantics_of_bin_op(bin_op)) {
      | DefinedPoly(Equals | NotEquals)
          when Typ.is_consistent(Ctx.empty, t, Atom(Bool) |> Typ.temp) =>
        ClosureWriter.return(Some(e))
      | Defined(_, _, ty_out, _)
          when
            Typ.is_consistent(
              Ctx.empty,
              t,
              Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp,
            ) =>
        ClosureWriter.return(Some(e))
      | Undefined(_)
      | DefinedPoly(_)
      | Defined(_) => ClosureWriter.return(None)
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
        ClosureWriter.return(Some(e))
      | Undefined(_)
      | Defined(_) => ClosureWriter.return(None)
      }
    | (ListConcat(d1, d2), List(_)) =>
      let* e1 = recur(Asc(d1, t) |> DHExp.fresh);
      let+ e2 = recur(Asc(d2, t) |> DHExp.fresh);
      Some(ListConcat(e1, e2) |> DHExp.fresh);
    | (Let(p, e1, e2), _) =>
      ClosureWriter.return(
        Some(Let(p, e1, Asc(e2, t) |> DHExp.fresh) |> DHExp.fresh),
      )
    | (Seq(e1, e2), _) =>
      ClosureWriter.return(
        Some(Seq(e1, Asc(e2, t) |> DHExp.fresh) |> DHExp.fresh),
      )
    | (Parens(e), _) =>
      ClosureWriter.return(
        Some(Parens(Asc(e, t) |> DHExp.fresh) |> DHExp.fresh),
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
    | (Asc(_), _) => ClosureWriter.return(None)
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
    | (Constructor(_), _)
    | (ProofObject(_), _) => ClosureWriter.return(None)
    };
  | _ => ClosureWriter.return(None)
  };
};

let rec transition_multiple =
        (~targets: Sample.targets, d: DHExp.t): (closure_closures, DHExp.t) => {
  switch (transition(~targets, ~recursive=true, d)) {
  | (closures, Some(d'')) =>
    let (c, d) = transition_multiple(~targets, d'');
    (closures @ c, d);
  | _ => ([], d)
  };
};
