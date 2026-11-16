/* Ascription Transitions */
open Util.OptUtil.Syntax;

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
  List.for_all2_exn(ds, ds', ~f=(d, d') => phys_equal(d, d'));

/* Whether [ty_rec], unrolled, is consistent with [ty]. A self-loop has no
   unrolling and is consistent with nothing. */
let unrolled_consistent = (ty_rec: Typ.t, ty: Typ.t): bool =>
  switch (Typ.unroll(ty_rec)) {
  | Some(ty_rec) => Typ.is_consistent(Ctx.empty, ty_rec, ty)
  | None => false
  };

/* Whether a value cast to, or matched against, a one-field labeled type
   `(l=…)` is lifted into it: `1` at `(a=Int)` becomes `(a=1)`. Statics
   lifts a value of known type by its type; one of unknown type is lifted
   here, when the cast or the pattern meets it, by the same rule: anything
   but a one-field tuple with that label or with none. Only a form that
   already shows it can't become that record is lifted; one that may still
   evaluate to anything (a variable, a call, a hole) waits. */
let rec lifts_into = (l: LabeledTuple.label, d: DHExp.t): bool =>
  switch (DHExp.term_of(d)) {
  | Closure(_, d) => lifts_into(l, d)
  | Tuple([entry]) =>
    switch (Exp.match_tup_label(entry)) {
    | Some((existing, _)) => !String.equal(existing, l)
    | None => false
    }
  | Constructor(c, _) => !String.is_prefix(c, ~prefix="$")
  | Ap(_, {term: Constructor(c, _), _}, _) =>
    !String.is_prefix(c, ~prefix="$")
  | Tuple(_)
  | Atom(_)
  | Fun(_)
  | TypFun(_)
  | BuiltinFun(_)
  | ListLit(_)
  | Cons(_) => true
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | DynamicErrorHole(_)
  | Deferral(_)
  | Undefined
  | DrvQuote(_)
  | Label(_)
  | ExplicitNonlabel
  | TupLabel(_)
  | Dot(_)
  | LivelitName(_)
  | Var(_)
  | Let(_)
  | Theorem(_)
  | ProofObject(_)
  | Forall(_)
  | FixF(_)
  | TyAlias(_)
  | Use(_)
  | Ap(_)
  | TypAp(_)
  | DeferredAp(_)
  | If(_)
  | Seq(_)
  | Test(_)
  | HintedTest(_)
  | Filter(_)
  | Parens(_)
  | Projector(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | Match(_)
  | TupleExtension(_)
  | Asc(_)
  | Module(_)
  | ModuleExp(_) => false
  };

let rec transition = (~recursive=false, d: DHExp.t): option(DHExp.t) => {
  let recur = (d: DHExp.t): DHExp.t =>
    if (recursive) {
      transition(~recursive, d) |> Option.value(~default=d);
    } else {
      d;
    };
  /* A cast pushed onto a part that is itself a cast composes with it, as a
     cast on a cast does here, so the result doesn't depend on which cast
     arrives first, and the stepper and the evaluator agree. */
  let ascribe = (part: DHExp.t, ty: Typ.t): DHExp.t => {
    let d = Asc(part, ty) |> DHExp.fresh;
    switch (DHExp.term_of(part)) {
    | Asc(_) => transition(d) |> Option.value(~default=d)
    | _ => d
    };
  };
  switch (DHExp.term_of(d)) {
  | Asc(e, t) =>
    /* A self-loop ascribes nothing, so the ascription is stuck. */
    let* t_unrolled = Typ.unroll(t);
    switch (DHExp.term_of(e), Typ.term_of(t_unrolled)) {
    // This is only necessary because sometimes we add two ascriptions and aren't marking it as a non-value
    | (Asc(e', t'), _) =>
      let* t' = Typ.unroll(t');
      let+ t = Typ.meet(Ctx.empty, t_unrolled, t');
      recur(
        IdTagged.fast_copy(DHExp.rep_id(e), Asc(e', t) |> DHExp.fresh),
      );
    | (_, Parens(t)) =>
      /* An impossible case: types are normalized before they come to
         transitions. e as it is, not a fresh copy: its id is kept, as
         everywhere here, and an e that already has the type comes back
         as itself. */
      transition(~recursive, Asc(e, t) |> DHExp.fresh)
    /* Only at a written one-field labeled type, as statics: a recursive
       type that unrolls to one would lift again at each unrolling. Before
       the closure arm, so a function's whole closure becomes the field
       rather than the record being stuck inside the closure. */
    | (_, Prod([{term: TupLabel({term: Label(l), _}, field_ty), _}]))
        when
          lifts_into(l, e)
          && (
            switch (Typ.term_of(t)) {
            | Rec(_) => false
            | _ => true
            }
          ) =>
      let label: DHExp.t = Label(l) |> DHExp.fresh;
      let field: DHExp.t =
        TupLabel(label, recur(ascribe(e, field_ty))) |> DHExp.fresh;
      Some(Tuple([field]) |> DHExp.fresh);
    | (Closure(ce, d), _) =>
      transition(~recursive, Asc(d, t) |> DHExp.fresh)
      |> Option.map(~f=d => Closure(ce, d) |> DHExp.fresh)
    | (Fun(p, body, closure_ty, name), Arrow(t1, t2)) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          IdTagged.FreshGrammar.(
            Exp.(fn(Pat.(asc(p, t1)), ascribe(body, t2), closure_ty, name))
          ),
        ),
      )
    | (TupLabel({term: ExplicitNonlabel, _}, inner), _) =>
      Some(recur(ascribe(inner, t)))
    /* Different labels: the value doesn't have the type, so the cast
       stays, as for every other mismatch below. Pushing it inside kept the
       value's label and silently changed the type (`(x=e) : (y=T)` stepped
       to `(x=e:T)`). */
    | (TupLabel({term: Label(a), _}, _), TupLabel({term: Label(b), _}, _))
        when !LabeledTuple.match_labels(a, b) =>
      None
    | (TupLabel(l, inner), TupLabel(_l2, inner_ty)) =>
      let inner' = recur(ascribe(inner, inner_ty));
      phys_equal(inner', inner)
        ? Some(e)
        : Some(
            IdTagged.fast_copy(
              DHExp.rep_id(e),
              TupLabel(l, inner') |> DHExp.fresh,
            ),
          );
    | (Tuple(es), Prod(tys)) when List.length(es) == List.length(tys) =>
      let es' =
        List.map2_exn(~f=(e, ty) => recur(ascribe(e, ty)), es, tys);
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
    | (ListLit((ds, ty0)), List(ty)) =>
      let ds' = List.map(~f=d => recur(ascribe(d, ty)), ds);
      let same_ty =
        switch (ty0) {
        | Some(ty0) => Typ.fast_equal(ty0, ty)
        | None => false
        };
      unchanged(ds, ds') && same_ty
        ? Some(e)
        : Some(
            IdTagged.fast_copy(
              DHExp.rep_id(e),
              ListLit((ds', Some(ty))) |> DHExp.fresh,
            ),
          );
    | (Cons(d1, d2), List(ty)) =>
      let d1' = recur(ascribe(d1, ty));
      let d2' = recur(ascribe(d2, t));
      phys_equal(d1', d1) && phys_equal(d2', d2)
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
            recur(ascribe(body, Typ.subst(new_ty, tp', t'))),
            name,
          )
          |> DHExp.fresh,
        ),
      );
    | (If(cond, e1, e2), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          If(recur(cond), recur(ascribe(e1, t)), recur(ascribe(e2, t)))
          |> DHExp.fresh,
        ),
      )
    | (Match(scrut, rules), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Match(
            scrut,
            List.map(~f=((p, body)) => (p, ascribe(body, t)), rules),
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
        when unrolled_consistent(sumt, sumt' |> Typ.temp) =>
      let entry = ConstructorMap.get_entry(c, m);
      switch (entry) {
      | Some(Some(t')) =>
        let payload' = recur(ascribe(payload, t'));
        phys_equal(payload', payload)
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
        when unrolled_consistent(t, t' |> Typ.temp) =>
      Some(e)
    | (ProofObject(e1), ProofOf(e2))
        when Exp.fast_equal_ignoring_list_types(e1, e2) =>
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
          ListConcat(recur(ascribe(d1, t)), recur(ascribe(d2, t)))
          |> DHExp.fresh,
        ),
      )
    | (Let(p, e1, e2), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Let(p, e1, ascribe(e2, t)) |> DHExp.fresh,
        ),
      )
    | (Seq(e1, e2), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Seq(e1, ascribe(e2, t)) |> DHExp.fresh,
        ),
      )
    | (Parens(pe), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Parens(ascribe(pe, t)) |> DHExp.fresh,
        ),
      )
    | (Projector(data, pe), _) =>
      Some(
        IdTagged.fast_copy(
          DHExp.rep_id(e),
          Projector(data, ascribe(pe, t)) |> DHExp.fresh,
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
    | (Forall(_), _) => None
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
    };
  | _ => None
  };
};

let rec transition_multiple = (d: DHExp.t): DHExp.t => {
  switch (transition(~recursive=true, d)) {
  | Some(d'') => transition_multiple(d'')
  | None => d
  };
};
