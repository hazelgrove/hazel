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
// TODO Rename to samples
[@deriving show]
type ascription_samples = list((Id.t, Sample.capture_spec, Exp.t));
module SampleWriter =
  Util.WriterMonad.Make({
    type t = ascription_samples;
    let empty = [];
    let append = (@);
  });

let rec transition =
        (~targets: Sample.targets, ~recursive=false, d: DHExp.t)
        : SampleWriter.t(option(DHExp.t)) => {
  open SampleWriter.Syntax;
  let recur = (d: DHExp.t): SampleWriter.t(DHExp.t) =>
    if (recursive) {
      transition(~targets, ~recursive, d)
      |> SampleWriter.map(_, Option.value(~default=d));
    } else {
      SampleWriter.return(d);
    };
  switch (DHExp.term_of(d)) {
  | Asc(e, t) =>
    switch (DHExp.term_of(e), Typ.term_of(Typ.unroll(t))) {
    | (Asc(e', t'), _)
        // This is only necessary because sometimes we add two ascriptions and aren't marking it as a non-value
        when Typ.is_consistent(Ctx.empty, Typ.unroll(t), Typ.unroll(t')) =>
      let* () =
        switch (Id.Map.find_opt(Exp.rep_id(e), targets)) {
        | Some(spec) => SampleWriter.tell([(Exp.rep_id(e), spec, e')])
        | None => SampleWriter.return()
        };
      switch (Typ.meet(Ctx.empty, Typ.unroll(t), Typ.unroll(t'))) {
      | Some(t) =>
        let+ result = recur(Asc(e', t) |> DHExp.fresh);
        Some(result);
      | None => SampleWriter.return(None) //TODO  This is an impossible case since we checked consistency
      };
    | (e, Parens(t)) =>
      // This is an impossible case since types should be normalized before coming to transitions
      transition(
        ~targets,
        ~recursive,
        Asc(e |> DHExp.fresh, t) |> DHExp.fresh,
      )
    | (Closure(ce, d), t) =>
      let+ result =
        transition(
          ~targets,
          ~recursive,
          Asc(d, t |> Typ.fresh) |> DHExp.fresh,
        );
      Option.map(d => Closure(ce, d) |> DHExp.fresh, result);
    | (Fun(p, body, closure_ty, name), Arrow(t1, t2)) =>
      SampleWriter.return(
        Some(
          IdTagged.fast_copy(
            DHExp.rep_id(e),
            IdTagged.FreshGrammar.(
              Exp.(
                fn(
                  Pat.(asc(p, t1)),
                  asc(body, t2),
                  ~typ=?closure_ty,
                  ~name?,
                )
              )
            ),
          ),
        ),
      )
    | (TupLabel({term: ExplicitNonlabel, _}, inner), _) =>
      let+ result = recur(Asc(inner, t) |> DHExp.fresh);
      Some(result);
    | (TupLabel(l, inner), TupLabel(_l2, inner_ty)) =>
      // TODO Figure out what to do if the labels don't match
      let+ inner' = recur(Asc(inner, inner_ty) |> DHExp.fresh);
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          TupLabel(l, inner') |> DHExp.fresh,
        ),
      );
    | (Tuple(es), Prod(tys)) when List.length(es) == List.length(tys) =>
      let+ es' =
        SampleWriter.sequence(
          List.map2((e, ty) => recur(Asc(e, ty) |> DHExp.fresh), es, tys),
        );
      Some(IdTagged.fast_copy(DHExp.rep_id(e), Tuple(es') |> DHExp.fresh));
    | (_, Unknown(_)) => SampleWriter.return(Some(e))
    | (Atom(value), Atom(typ)) =>
      switch (value, typ) {
      | (Int(_), Int)
      | (String(_), String)
      | (Nat(_), Nat)
      | (Float(_), Float)
      | (SInt(_), SInt)
      | (Bool(_), Bool) => SampleWriter.return(Some(e))
      | (Int(_), _)
      | (String(_), _)
      | (Nat(_), _)
      | (Float(_), _)
      | (SInt(_), _)
      | (Bool(_), _) => SampleWriter.return(None)
      }
    | (ListLit(ds), List(ty)) =>
      let+ ds' =
        SampleWriter.sequence(
          List.map(d => recur(Asc(d, ty) |> DHExp.fresh), ds),
        );
      Some(
        IdTagged.fast_copy(DHExp.rep_id(e), ListLit(ds') |> DHExp.fresh),
      );
    | (Cons(d1, d2), List(ty)) =>
      let* d1' = recur(Asc(d1, ty) |> DHExp.fresh);
      let+ d2' = recur(Asc(d2, t) |> DHExp.fresh);
      Some(
        IdTagged.fast_copy(DHExp.rep_id(e), Cons(d1', d2') |> DHExp.fresh),
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
    | (If(cond, e1, e2), _) =>
      let* cond' = recur(cond);
      let* e1' = recur(Asc(e1, t) |> DHExp.fresh);
      let+ e2' = recur(Asc(e2, t) |> DHExp.fresh);
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          If(cond', e1', e2') |> DHExp.fresh,
        ),
      );
    | (Match(scrut, rules), _) =>
      SampleWriter.return(
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
        let+ payload' = recur(Asc(payload, t') |> DHExp.fresh);
        Some(Ap(Forward, con, payload') |> DHExp.fresh);
      | Some(None)
      | None => SampleWriter.return(None)
      };
    | (Constructor(_, Some(Some(t))), t')
        when Typ.is_consistent(Ctx.empty, Typ.unroll(t), t' |> Typ.temp) =>
      SampleWriter.return(Some(e))
    | (ProofObject(e1), ProofOf(e2)) when Exp.fast_equal(e1, e2) =>
      SampleWriter.return(Some(ProofObject(e1) |> DHExp.fresh))
    | (Test(_), Prod([])) => SampleWriter.return(Some(e))
    // These are non-value cases we're handling to process ascriptions as early as possible
    | (BinOp(bin_op, _, _), _) =>
      switch (Operators.semantics_of_bin_op(bin_op)) {
      | DefinedPoly(Equals | NotEquals)
          when Typ.is_consistent(Ctx.empty, t, Atom(Bool) |> Typ.temp) =>
        SampleWriter.return(Some(e))
      | Defined(_, _, ty_out, _)
          when
            Typ.is_consistent(
              Ctx.empty,
              t,
              Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp,
            ) =>
        SampleWriter.return(Some(e))
      | Undefined(_)
      | DefinedPoly(_)
      | Defined(_) => SampleWriter.return(None)
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
        SampleWriter.return(Some(e))
      | Undefined(_)
      | Defined(_) => SampleWriter.return(None)
      }
    | (ListConcat(d1, d2), List(_)) =>
      let* d1' = recur(Asc(d1, t) |> DHExp.fresh);
      let+ d2' = recur(Asc(d2, t) |> DHExp.fresh);
      Some(ListConcat(d1', d2') |> DHExp.fresh);
    | (Let(p, e1, e2), _) =>
      SampleWriter.return(
        Some(Let(p, e1, Asc(e2, t) |> DHExp.fresh) |> DHExp.fresh),
      )
    | (Seq(e1, e2), _) =>
      SampleWriter.return(
        Some(Seq(e1, Asc(e2, t) |> DHExp.fresh) |> DHExp.fresh),
      )
    | (Parens(e), _) =>
      SampleWriter.return(
        Some(Parens(Asc(e, t) |> DHExp.fresh) |> DHExp.fresh),
      )
    | (Projector(data, e), _) =>
      SampleWriter.return(
        Some(Projector(data, Asc(e, t) |> DHExp.fresh) |> DHExp.fresh),
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
    | (Asc(_), _) => SampleWriter.return(None)
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
    | (Constructor(_), _) => SampleWriter.return(None)
    }
  | _ => SampleWriter.return(None)
  };
};

let rec transition_multiple =
        (~targets: Sample.targets, d: DHExp.t): SampleWriter.t(DHExp.t) => {
  open SampleWriter.Syntax;
  let* transitioned = transition(~targets, ~recursive=true, d);
  switch (transitioned) {
  | Some(d'') => transition_multiple(~targets, d'')
  | None => SampleWriter.return(d)
  };
};

// TODO return samples
let transition_multiple = (~targets: Sample.targets, d: DHExp.t): DHExp.t =>
  snd(transition_multiple(~targets, d));
