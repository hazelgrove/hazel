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
/* Whether a constructor's own sum type is consistent with the type it is
   ascribed, remembered by the two types' Merkle hashes (TypHash). Every
   constructor application the evaluator ascribes asks this of two copies
   of the same sum type, a full meet each time; the copies are fresh
   objects, so only their hashes recognize them. The context here is always
   Ctx.empty, and is_consistent ignores an Unknown's provenance, as the hash
   does, so the answer depends only on the two hashes. */
let consistent_memo: Hashtbl.t((TypHash.t, TypHash.t), bool) =
  Hashtbl.create(64);
let consistent_by_hash = (a: Typ.t, b: Typ.t, compute: unit => bool): bool =>
  switch (TypHash.hash(a), TypHash.hash(b)) {
  | (Some(ha), Some(hb)) =>
    let key = (ha, hb);
    switch (Hashtbl.find_opt(consistent_memo, key)) {
    | Some(r) => r
    | None =>
      let r = compute();
      if (Hashtbl.length(consistent_memo) > 4096) {
        Hashtbl.reset(consistent_memo);
      };
      Hashtbl.replace(consistent_memo, key, r);
      r;
    };
  | _ => compute()
  };
/* Whether pushing an ascription through a list's or tuple's parts gave
   back the very same parts. Then the value already had the type -- plain
   data, well typed, no function to wrap -- and the push gives the value
   itself rather than a copy of it: no new nodes, and the same objects for
   anything downstream that keeps values by identity. Only a recursive
   push (transition_multiple) can give a part back unchanged; a stepwise
   one wraps each part in a new ascription. */
let unchanged = (ds: list(DHExp.t), ds': list(DHExp.t)): bool =>
  List.for_all2((d, d') => d === d', ds, ds');

/* Data values already found to have a type, by the type's hash. A push
   through a value that already has the type gives the value back, but it
   still walks all of it. A value is pushed through at every call of a
   function whose parameter is annotated (`fun x : T -> ...`), so one value
   given to several such functions is walked at each, and a recursive one
   over a list is given the list's tail each time, so the walks over one
   list add up to the square of its length. (A function annotated at its
   let, `let f : A -> B = fun x -> ...`, is not pushed through on its
   recursive calls.) Values are immutable, so once a walk gives a value
   back, that value has the type for good, and the next push through it is
   one lookup.

   Keyed by the value's term, which a variable lookup keeps when it copies
   the value's outer record (fast_copy). A list's tail is a new ListLit
   (Unboxing) around the same OCaml list cells, so a list is keyed by its
   cells, and when a list has the type, so has every tail of it. The
   context is always Ctx.empty, as for consistent_memo, so the answer
   depends only on the value and the type's hash. */
let has_type: Util.IdentityTable.t(Obj.t, list(TypHash.t)) =
  Util.IdentityTable.create();
/* Only values with parts are remembered: an atom or a bare constructor
   is checked in one match, cheaper than a lookup, and remembering every
   number in a picture's points cost more than it saved in Firefox (the
   livelits branch's woods picture). */
let type_key = (d: DHExp.t): option(Obj.t) =>
  switch (DHExp.term_of(d)) {
  | ListLit([_, ..._] as ds) => Some(Obj.repr(ds))
  | ListLit([])
  | Atom(_)
  | Constructor(_) => None
  | term => Obj.is_block(Obj.repr(term)) ? Some(Obj.repr(term)) : None
  };
let known_to_have = (d: DHExp.t, t: Typ.t): bool =>
  switch (type_key(d), TypHash.hash(t)) {
  | (Some(k), Some(h)) =>
    switch (Util.IdentityTable.find_opt(has_type, k)) {
    | Some(hs) => List.mem(h, hs)
    | None => false
    }
  | _ => false
  };
let remember_has = (key: Obj.t, h: TypHash.t): unit => {
  let hs =
    Util.IdentityTable.find_opt(has_type, key) |> Option.value(~default=[]);
  if (!List.mem(h, hs)) {
    Util.IdentityTable.replace(has_type, key, [h, ...hs]);
  };
};
/* After a recursive push of Asc(e, t): if it gave e back, e has t. */
let noted = (e: DHExp.t, t: Typ.t, r: option(DHExp.t)): option(DHExp.t) => {
  switch (r, TypHash.hash(t)) {
  | (Some(r'), Some(h)) when r' === e =>
    switch (DHExp.term_of(e)) {
    | ListLit([_, ..._] as ds) =>
      let rec tails = (l: list(DHExp.t)) =>
        switch (l) {
        | [] => ()
        | [_, ...tl] =>
          remember_has(Obj.repr(l), h);
          tails(tl);
        };
      tails(ds);
    | _ =>
      switch (type_key(e)) {
      | Some(k) => remember_has(k, h)
      | None => ()
      }
    }
  | _ => ()
  };
  r;
};

/* How deep in a pushed value has_type is asked and told: the value itself
   (0) and its parts (1), as in `find(rest, c, r)` against
   `([Cell], Int, Int)`, where the list is a part of a tuple that is new at
   every call. Deeper parts are walked without it. A function's argument is
   often new on every call (a JS argument converted by the library, a
   record built for the call), so remembering all of it, down to every
   field and every tail of every inner list, costs a lookup and a write per
   part that no later push asks about, and the hash of each part's type
   besides. A record's label and a list's tail are no deeper than the
   record or the list. */
let memo_depth = 3;

/* A recursive push through a value known to have the type gives it back
   without walking it (has_type). */
let rec transition =
        (~recursive=false, ~depth=0, d: DHExp.t): option(DHExp.t) =>
  switch (DHExp.term_of(d)) {
  | Asc(e, t) when recursive && depth <= memo_depth && known_to_have(e, t) =>
    Some(e)
  | Asc(e, t) when recursive && depth <= memo_depth =>
    noted(e, t, step(~recursive, ~depth, d))
  | _ => step(~recursive, ~depth, d)
  }
and step = (~recursive: bool, ~depth: int, d: DHExp.t): option(DHExp.t) => {
  let recur_at = (depth: int, d: DHExp.t): DHExp.t =>
    if (recursive) {
      transition(~recursive, ~depth, d) |> Option.value(~default=d);
    } else {
      d;
    };
  /* a part of the value, one level down */
  let recur = recur_at(depth + 1);
  /* the same value, seen through a label or as a list's tail */
  let recur_same = recur_at(depth);
  switch (DHExp.term_of(d)) {
  | Asc(e, t) =>
    switch (DHExp.term_of(e), Typ.term_of(Typ.unroll(t))) {
    | (Asc(e', t'), _)
        // This is only necessary because sometimes we add two ascriptions and aren't marking it as a non-value
        when Typ.is_consistent(Ctx.empty, Typ.unroll(t), Typ.unroll(t')) =>
      switch (Typ.meet(Ctx.empty, Typ.unroll(t), Typ.unroll(t'))) {
      | Some(t) =>
        Some(
          recur_same(
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
      transition(~recursive, ~depth, Asc(e, t) |> DHExp.fresh)
    | (Closure(ce, d), t) =>
      transition(~recursive, ~depth, Asc(d, t |> Typ.fresh) |> DHExp.fresh)
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
      Some(recur_same(Asc(inner, t) |> DHExp.fresh))
    | (TupLabel(l, inner), TupLabel(_l2, inner_ty)) =>
      // TODO Figure out what to do if the labels don't match
      let inner' = recur_same(Asc(inner, inner_ty) |> DHExp.fresh);
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
      let d2' = recur_same(Asc(d2, t) |> DHExp.fresh);
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
          consistent_by_hash(sumt, t, () =>
            Typ.is_consistent(Ctx.empty, Typ.unroll(sumt), sumt' |> Typ.temp)
          ) =>
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
