open Util;

/* What is unboxing?

   When you have an expression of type list, and it's finished evaluating,
   is it a list literal? Sadly not necessarily, it might be:

    - an indeterminate list cons: e.g. 1 :: ?
    - a list literal or list cons with some casts wrapped around it
    - an indet term in a cast to a list type

    Unboxing is the process of turning a list literal into a list literal if it is a list literal,
    by pushing casts inside data structures, or giving up if it is not a list literal.
    It may give up in two distinct ways:
    - IndetMatch: Due to holes in the expression it may or may not match the unboxing request
                  depending on possible substitutions of the holes.
                  e.g. 1 :: ? might match a list of length 3 (LitLitn(3))
                  or   ? : [Int] might match a list (of any length)
    - DoesNotMatch: Could not possibly match the unboxing request
                  e.g. 1 :: ? definitely does NOT match a list of length 0 (ListLitn(0))

    Note unboxing only works one layer deep, if we have a list of lists then
    the inner lists may still have casts around them after unboxing.
    */

type unboxed_tfun =
  | TypFun(TPat.t, Exp.t, option(string))
  | TFunCast(DHExp.t, TPat.t, Typ.t, TPat.t, Typ.t);

type unboxed_fun =
  | Constructor(string)
  | FunEnv(Pat.t, Exp.t, ClosureEnvironment.t)
  | FunNoEnv(Pat.t, Exp.t)
  | FunCast(DHExp.t, Typ.t, Typ.t, Typ.t, Typ.t)
  | BuiltinFun(string)
  | DeferredAp(DHExp.t, list(DHExp.t));

type unbox_request('a) =
  | Atom(Atom.kind('a)): unbox_request('a)
  | Label: unbox_request(string)
  | Tuple(int): unbox_request(list(DHExp.t))
  | TupLabel(DHPat.t): unbox_request(DHExp.t)
  | ListLit: unbox_request(list(DHExp.t)) // Unboxes to a known length list LITERAL. Not all list final forms land in this category (e.g. Cons: 1 :: ?)
  | ListLitn(int): unbox_request(list(DHExp.t)) // This request is used for performance reasons to prevent casting lists of the wrong length and for matching list lits against cons expressions
  | Cons: unbox_request((DHExp.t, DHExp.t))
  | SumNoArg(string): unbox_request(unit)
  | SumWithArg(string): unbox_request(DHExp.t)
  | TypFun: unbox_request(unboxed_tfun)
  | Fun: unbox_request(unboxed_fun)
  | TupleElementPivot(LabeledTuple.label)
    : unbox_request((LabeledTuple.label, list(DHExp.t)))
  | LabeledTupleProjection(LabeledTuple.label): unbox_request(DHExp.t)
  | LabeledTupleEntries
      : unbox_request(list((option(LabeledTuple.label), DHExp.t)));

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type unboxed('a) =
  | DoesNotMatch
  | IndetMatch
  | Matches('a);

let ( let* ) = (x: unboxed('a), f: 'a => unboxed('b)): unboxed('b) =>
  switch (x) {
  | IndetMatch => IndetMatch
  | DoesNotMatch => DoesNotMatch
  | Matches(x) => f(x)
  };

let sequence = (l: list(unboxed('a))): unboxed(list('a)) =>
  List.fold_left(
    (acc, x) => {
      let* acc = acc;
      let* x = x;
      Matches([x, ...acc]);
    },
    Matches([]),
    l,
  );
let fixup_cast = Casts.transition_multiple;

/* This function has a different return type depending on what kind of request
   it is given. This unfortunately uses a crazy OCaml feature called GADTS, but
   it avoids having to write a separate unbox function for each kind of request.
   */

let rec unbox: type a. (unbox_request(a), DHExp.t) => unboxed(a) =
  (request, expr) => {
    switch (request, DHExp.term_of(expr)) {
    /* Remove parentheses from casts */
    | (_, Cast(d, {term: Parens(x), _}, y))
    | (_, Cast(d, x, {term: Parens(y), _})) =>
      unbox(request, Cast(d, x, y) |> DHExp.fresh)

    /* $e and $v could have any type, but are indet */

    | (_, UnOp(Meta(Unquote), _)) => IndetMatch
    | (_, Constructor(c, _)) when String.starts_with(c, ~prefix="$") =>
      IndetMatch

    /* TupLabels can be anything except for tuplabels with unmatching labels */
    | (TupLabel(tuplabel), TupLabel(_, e)) =>
      if (Option.equal(
            LabeledTuple.equal_label,
            Pat.get_label(tuplabel),
            Exp.get_label(expr),
          )) {
        Matches(e);
      } else {
        DoesNotMatch;
      }
    | (
        TupLabel(tl),
        Cast(t, {term: TupLabel(_, ty1), _}, {term: TupLabel(_, ty2), _}),
      ) =>
      let* t = unbox(TupLabel(tl), t);
      let t = fixup_cast(Cast(t, ty1, ty2) |> DHExp.fresh);
      Matches(t);
    | (TupLabel(_), _) => Matches(expr)

    /* Remove Tuplabels from casts otherwise */
    | (_, Cast(e, {term: TupLabel(_, e1), _}, e2)) =>
      switch (DHExp.term_of(e)) {
      | TupLabel(_, e) =>
        unbox(request, fixup_cast(Cast(e, e1, e2) |> DHExp.fresh))
      | _ => unbox(request, fixup_cast(Cast(e, e1, e2) |> DHExp.fresh))
      }
    | (LabeledTupleProjection(l), Cast(_, _, {term: List(_), _})) =>
      let* ls: list(TermBase.exp_t) = unbox(ListLit, expr);
      let* elements: list(TermBase.exp_t) =
        sequence(List.map(unbox(LabeledTupleProjection(l)), ls));
      let exp: TermBase.exp_t = ListLit(elements) |> Exp.fresh;
      Matches(exp);
    | (LabeledTupleProjection(l), Cast(_, {term: List(_), _}, _)) =>
      let* ls: list(TermBase.exp_t) = unbox(ListLit, expr);
      let* elements: list(TermBase.exp_t) =
        sequence(List.map(unbox(LabeledTupleProjection(l)), ls));
      let exp: TermBase.exp_t = ListLit(elements) |> Exp.fresh;
      Matches(exp);
    | (LabeledTupleProjection(_), Cast(d, _, {term: Prod(_), _})) =>
      unbox(request, d)
    | (LabeledTupleProjection(l), Tuple(ds)) =>
      switch (LabeledTuple.find_label(Exp.match_tup_label, ds, l)) {
      | Some(_) => Matches(expr) // Choosing to just return the original expression here
      | _ => IndetMatch // TODO Should this be DoesNotMatch?
      }
    | (LabeledTupleProjection(_), ListLit(_)) => Matches(expr)
    // TODO Do tuple element pivot with LabeledTupleEntries
    | (TupleElementPivot(_), Cast(d, _, {term: Unknown(_), _})) =>
      unbox(request, d)
    | (TupleElementPivot(_), Cast(d, ty1, ty2))
        when Typ.is_consistent(Ctx.empty_pre_elaboration, ty1, ty2) =>
      // TODO: This is a hack. We need a better way to handle this than an empty context.
      unbox(request, d)
    | (TupleElementPivot(l), Tuple(ds)) =>
      let found_pivot: option((string, list(Exp.t))) =
        ListUtil.find_with_rest(
          exp => {
            switch (Exp.match_tup_label(DHExp.strip_casts(exp))) {
            // TODO: I need a better plan than stripping casts
            | Some((name, {term: Atom(String(e)), _})) when name == l =>
              Some(e)
            | (_d: option((string, TermBase.exp_t))) => None
            }
          },
          ds,
        );
      switch (found_pivot) {
      | None => DoesNotMatch
      | Some(a) => Matches(a)
      };
    | (
        LabeledTupleEntries,
        Cast(d, {term: Prod(tys), _}, {term: Unknown(_), _}),
      ) =>
      let* entries: list((option(LabeledTuple.label), DHExp.t)) =
        unbox(request, d);

      if (List.length(entries) == List.length(tys)) {
        let entries =
          List.map2(
            ((lab, e), ty) =>
              (
                lab,
                fixup_cast(
                  Cast(e, ty, Unknown(Internal) |> Typ.fresh) |> DHExp.fresh,
                ),
              ),
            entries,
            tys,
          );
        Matches(entries);
      } else {
        DoesNotMatch;
      };
    | (
        LabeledTupleEntries,
        Cast(t, {term: Prod(t1s), _}, {term: Prod(t2s), _}),
      )
        when List.length(t1s) == List.length(t2s) =>
      let* t = unbox(LabeledTupleEntries, t);
      let t =
        ListUtil.map3(
          ((lab, d), t1, t2) => {
            let value_ty1 =
              Typ.match_tup_label(t1)
              |> Option.map(snd)
              |> Option.value(~default=t1);
            let value_ty2 =
              Typ.match_tup_label(t2)
              |> Option.map(snd)
              |> Option.value(~default=t2);

            (lab, fixup_cast(Cast(d, value_ty1, value_ty2) |> DHExp.fresh));
          },
          t,
          t1s,
          t2s,
        );
      Matches(t);
    | (LabeledTupleEntries, Tuple(ds)) =>
      let rec unbox_tup_label =
              (d: Exp.t): option((option(LabeledTuple.label), Exp.t)) => {
        switch (d.term) {
        | TupLabel({term: Label(l), _}, e) => Some((Some(l), e))
        | Cast(
            tl,
            {term: TupLabel(_, ty1), _},
            {term: TupLabel(_, ty2), _},
          ) =>
          Option.map(
            ((lab, e)) =>
              (lab, fixup_cast(Cast(e, ty1, ty2) |> DHExp.fresh)),
            unbox_tup_label(tl),
          )
        | Cast(
            tl,
            {term: TupLabel(_, ty1), _},
            {term: Unknown(Internal), _},
          ) =>
          Option.map(
            ((lab, e)) =>
              (
                lab,
                fixup_cast(
                  Cast(e, ty1, Unknown(Internal) |> Typ.fresh) |> DHExp.fresh,
                ),
              ),
            unbox_tup_label(tl),
          )
        | Cast(_) => assert(false) // Figure out what to do here
        | _ => Some((None, d))
        };
      };

      let entries: list(option((option(string), Exp.t))) =
        List.map(unbox_tup_label, ds);

      switch (OptUtil.sequence(entries)) {
      | Some(entries) => Matches(entries)
      | None => DoesNotMatch
      };
    /* Base types are always already unboxed because of the ITCastID rule*/
    | (Atom(r), Atom(x)) =>
      switch (Atom.unbox(r, x)) {
      | Some(x) => Matches(x)
      | None => DoesNotMatch
      }

    /* Lists can be either lists or cons with indet tail or list casts */
    | (ListLit, ListLit(l)) => Matches(l)
    | (ListLitn(n), ListLit(l)) when ListUtil.is_length(n, l) => Matches(l)
    | (ListLitn(_), ListLit(_)) => DoesNotMatch
    /* A cons final form is always indet, so either does NOT match or indet matches with a listliteral*/
    | (ListLitn(0), Cons(_)) => DoesNotMatch // Cons is not an empty list
    | (ListLitn(n), Cons(_, xs)) => unbox(ListLitn(n - 1), xs)
    | (ListLit, Cons(_)) => IndetMatch // WIthout length of ListLit we cannot know

    | (Cons, ListLit([x, ...xs])) =>
      Matches((x, ListLit(xs) |> DHExp.fresh))
    | (Cons, ListLit([])) => DoesNotMatch
    | (Cons, Cons(x, xs)) => Matches((x, xs))

    | (ListLit, Cast(l, {term: List(t1), _}, {term: List(t2), _})) =>
      let* l = unbox(ListLit, l);
      let l = List.map(d => Cast(d, t1, t2) |> DHExp.fresh, l);
      let l = List.map(fixup_cast, l);
      Matches(l);
    | (ListLitn(n), Cast(l, {term: List(t1), _}, {term: List(t2), _})) =>
      let* l = unbox(ListLitn(n), l);
      let l = List.map(d => Cast(d, t1, t2) |> DHExp.fresh, l);
      let l = List.map(fixup_cast, l);
      Matches(l);
    | (
        Cons,
        Cast(l, {term: List(t1), _} as ct1, {term: List(t2), _} as ct2),
      ) =>
      let* l = unbox(Cons, l);
      switch (l) {
      | (x, xs) =>
        Matches((
          Cast(x, t1, t2) |> DHExp.fresh |> fixup_cast,
          Cast(xs, ct1, ct2) |> DHExp.fresh,
        ))
      };

    /* Tuples can be either tuples or tuple casts */
    | (Tuple(n), Tuple(t)) when List.length(t) == n => Matches(t)
    | (Tuple(_), Tuple(_)) => DoesNotMatch
    | (Tuple(n), Cast(t, {term: Prod(t1s), _}, {term: Prod(t2s), _}))
        when n == List.length(t1s) && n == List.length(t2s) =>
      let* t = unbox(Tuple(n), t);
      let t =
        ListUtil.map3(
          (d, t1, t2) => Cast(d, t1, t2) |> DHExp.fresh,
          t,
          t1s,
          t2s,
        );
      let t = List.map(fixup_cast, t);
      Matches(t);

    /* Sum constructors can be either sum constructors, sum constructors
       applied to some value or sum casts */
    | (SumNoArg(name1), Constructor(name2, _)) when name1 == name2 =>
      Matches()
    | (SumNoArg(_), Constructor(_)) => DoesNotMatch
    | (SumNoArg(_), Ap(_, {term: Constructor(_), _}, _)) => DoesNotMatch
    | (SumNoArg(name), Cast(d1, {term: Sum(_), _}, {term: Sum(s2), _}))
        when
          ConstructorMap.has_constructor_no_args(name, s2)
          || ConstructorMap.has_bad_entry(s2) =>
      let* d1 = unbox(SumNoArg(name), d1);
      Matches(d1);
    | (SumNoArg(_), Cast(_, {term: Sum(_), _}, {term: Sum(_), _})) =>
      IndetMatch

    | (SumWithArg(_), Constructor(_)) => DoesNotMatch
    | (SumWithArg(name1), Ap(_, {term: Constructor(name2, _), _}, d3))
        when name1 == name2 =>
      Matches(d3)
    | (SumWithArg(_), Ap(_, {term: Constructor(_), _}, _)) => DoesNotMatch
    | (SumWithArg(name), Cast(d1, {term: Sum(s1), _}, {term: Sum(s2), _})) =>
      let get_entry_or_bad = s =>
        switch (ConstructorMap.get_entry(name, s)) {
        | Some(Some(x)) => Some(x)
        | Some(None) => None
        | None when ConstructorMap.has_bad_entry(s) =>
          Some(Typ.temp(Unknown(Internal)))
        | None => None
        };
      switch (get_entry_or_bad(s1), get_entry_or_bad(s2)) {
      | (Some(x), Some(y)) =>
        let* d1 = unbox(SumWithArg(name), d1);
        Matches(Cast(d1, x, y) |> Exp.fresh |> fixup_cast);
      | _ => IndetMatch
      };
    // There should be some sort of failure here when the cast doesn't go through.

    /* Function-like things can look like the following when values */
    | (Fun, Constructor(name, _)) => Matches(Constructor(name)) // Perhaps we should check if the constructor actually is a function?
    | (Fun, Closure(env', {term: Fun(dp, d3, _, _), _})) =>
      Matches(FunEnv(dp, d3, env'))
    | (Fun, Fun(dp, d3, _, _)) => Matches(FunNoEnv(dp, d3))
    | (
        Fun,
        Cast(
          d3',
          {term: Arrow(ty1, ty2), _},
          {term: Arrow(ty1', ty2'), _},
        ),
      ) =>
      Matches(FunCast(d3', ty1, ty2, ty1', ty2'))
    | (Fun, BuiltinFun(name)) => Matches(BuiltinFun(name))
    | (Fun, DeferredAp(d1, ds)) => Matches(DeferredAp(d1, ds))

    /* TypFun-like things can look like the following when values */
    | (TypFun, Closure(env', {term: TypFun(utpat, tfbody, name), _})) =>
      Matches(TypFun(utpat, Closure(env', tfbody) |> Exp.fresh, name))
    | (TypFun, TypFun(utpat, tfbody, name)) =>
      Matches(TypFun(utpat, tfbody, name))
    // Note: We might be able to handle this cast like other casts
    | (
        TypFun,
        Cast(
          d'',
          {term: Forall(tp1, _), _} as t1,
          {term: Forall(tp2, _), _} as t2,
        ),
      ) =>
      Matches(TFunCast(d'', tp1, t1, tp2, t2))

    /* Any cast from unknown is indet */
    | (_, Cast(_, {term: Unknown(_), _}, _)) => IndetMatch

    /* Any failed cast is indet */
    | (_, FailedCast(_)) => IndetMatch

    /* Forms that are the wrong type of value - these cases indicate an error
       in elaboration or in the cast calculus. */
    | (
        _,
        Atom(_) | Label(_) | Constructor(_) | BuiltinFun(_) | Deferral(_) |
        DeferredAp(_) |
        ListLit(_) |
        Cons(_) |
        TupLabel(_) |
        Tuple(_) |
        Cast(_) |
        TypFun(_, _, _) |
        Ap(_, {term: Constructor(_), _}, _),
      ) =>
      switch (request) {
      | TupLabel(_) =>
        raise(EvaluatorError.Exception(InvalidBoxedTupLabel(expr)))
      | Atom(Bool) =>
        raise(EvaluatorError.Exception(InvalidBoxedBoolLit(expr)))
      | Atom(SInt)
      | Atom(Int) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(expr)))
      | Atom(Float) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(expr)))
      | Atom(String) =>
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(expr)))
      | Atom(Nat) =>
        raise(EvaluatorError.Exception(InvalidBoxedNatLit(expr)))
      | Label => raise(EvaluatorError.Exception(InvalidBoxedLabel(expr)))
      | Tuple(_) => raise(EvaluatorError.Exception(InvalidBoxedTuple(expr)))
      | ListLit
      | ListLitn(_) =>
        raise(EvaluatorError.Exception(InvalidBoxedListLit(expr)))
      | Cons => raise(EvaluatorError.Exception(InvalidBoxedListCons(expr)))
      | SumNoArg(_)
      | SumWithArg(_) =>
        raise(EvaluatorError.Exception(InvalidBoxedSumConstructor(expr)))
      | Fun => raise(EvaluatorError.Exception(InvalidBoxedFun(expr)))
      | TypFun => raise(EvaluatorError.Exception(InvalidBoxedTypFun(expr)))
      | TupleElementPivot(_) =>
        raise(EvaluatorError.Exception(InvalidBoxedTuple(expr))) // todo: better error message
      | LabeledTupleProjection(_) =>
        raise(EvaluatorError.Exception(InvalidBoxedTuple(expr))) // todo: better error message
      | LabeledTupleEntries =>
        raise(EvaluatorError.Exception(InvalidBoxedTuple(expr))) // todo: better error message
      }

    /* Forms that are not yet or will never be a value */
    | (
        _,
        Invalid(_) | Undefined | EmptyHole | MultiHole(_) | DynamicErrorHole(_) |
        Var(_) |
        Let(_) |
        Fun(_, _, _, _) |
        TypAp(_) |
        FixF(_) |
        TyAlias(_) |
        Use(_) |
        Ap(_) |
        If(_) |
        Seq(_) |
        Test(_) |
        Filter(_) |
        Closure(_) |
        Parens(_) |
        Probe(_) |
        ListConcat(_) |
        Dot(_) |
        UnOp(_) |
        BinOp(_) |
        LivelitName(_) |
        Match(_),
      ) =>
      IndetMatch
    };
  };
