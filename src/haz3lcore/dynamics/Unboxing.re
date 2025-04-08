open Util;

/* What is unboxing?

   When you have an expression of type list, and it's finished evaluating,
   is it a list? Sadly not necessarily, it might be:

    - indeterminate, e.g. it has a hole in it
    - a list with some casts wrapped around it

    Unboxing is the process of turning a list into a list if it is a list,
    by pushing casts inside data structures, or giving up if it is not a list.

    Note unboxing only works one layer deep, if we have a list of lists then
    the inner lists may still have casts around them after unboxing.
    */

type unboxed_tfun =
  | TypFun(TPat.t, Exp.t, option(string))
  | TFunCast(DHExp.t, TPat.t, TypSlice.t, TPat.t, TypSlice.t);

type unboxed_fun =
  | Constructor(string)
  | FunEnv(Pat.t, Exp.t, ClosureEnvironment.t)
  | FunCast(DHExp.t, TypSlice.t, TypSlice.t, TypSlice.t, TypSlice.t)
  | BuiltinFun(string)
  | DeferredAp(DHExp.t, list(DHExp.t));

type unbox_request('a) =
  | Int: unbox_request(int)
  | Float: unbox_request(float)
  | Bool: unbox_request(bool)
  | String: unbox_request(string)
  | Label: unbox_request(string)
  | Tuple(int): unbox_request(list(DHExp.t))
  | TupLabel(DHPat.t): unbox_request(DHExp.t)
  | List: unbox_request(list(DHExp.t))
  | ListLit(int): unbox_request(list(DHExp.t)) // This request is used for performance reasons to prevent casting lists of the wrong length
  | Cons: unbox_request((DHExp.t, DHExp.t))
  | SumNoArg(string): unbox_request(unit)
  | SumWithArg(string): unbox_request(DHExp.t)
  | TypFun: unbox_request(unboxed_tfun)
  | Fun: unbox_request(unboxed_fun);

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

let fixup_cast = Casts.transition_multiple;

/* This function has a different return type depending on what kind of request
   it is given. This unfortunately uses a crazy OCaml feature called GADTS, but
   it avoids having to write a separate unbox function for each kind of request.
   */

let rec unbox: type a. (unbox_request(a), DHExp.t) => unboxed(a) =
  (request, expr) => {
    switch (request, DHExp.term_of(expr)) {
    /* Remove parentheses from casts */
    | (_, Cast(d, s1, s2)) when TypSlice.is_parens(s1) =>
      unbox(request, Cast(d, TypSlice.unparens(s1), s2) |> DHExp.fresh)
    | (_, Cast(d, s1, s2)) when TypSlice.is_parens(s2) =>
      unbox(request, Cast(d, s1, TypSlice.unparens(s2)) |> DHExp.fresh)
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
    | (TupLabel(tl), Cast(t, s1, s2))
        when
          TypSlice.is_tuplabel(s1, ~ignore_parens=false)
          && TypSlice.is_tuplabel(s2, ~ignore_parens=false) =>
      let ((_, s1), (_, s2)) = (
        TypSlice.untuplabel(s1),
        TypSlice.untuplabel(s2),
      );
      let* t = unbox(TupLabel(tl), t);
      let t = fixup_cast(Cast(t, s1, s2) |> DHExp.fresh);
      Matches(t);
    | (TupLabel(_), _) => Matches(expr)

    /* Remove Tuplabels from casts otherwise */
    | (_, Cast(e, s1, s2))
        when TypSlice.is_tuplabel(s1, ~ignore_parens=false) =>
      let (_, s1) = TypSlice.untuplabel(s1);
      switch (DHExp.term_of(e)) {
      | TupLabel(_, e) => unbox(request, Cast(e, s1, s2) |> DHExp.fresh)
      | _ => unbox(request, Cast(e, s1, s2) |> DHExp.fresh)
      };

    /* Base types are always already unboxed because of the ITCastID rule*/
    | (Bool, Bool(b)) => Matches(b)
    | (Int, Int(i)) => Matches(i)
    | (Float, Float(f)) => Matches(f)
    | (String, String(s)) => Matches(s)
    | (Label, Label(s)) => Matches(s)

    /* Lists can be either lists or list casts */
    | (List, ListLit(l)) => Matches(l)
    | (ListLit(n), ListLit(l)) when ListUtil.is_length(n, l) => Matches(l)
    | (ListLit(_), ListLit(_)) => DoesNotMatch
    | (Cons, ListLit([x, ...xs])) =>
      Matches((x, ListLit(xs) |> DHExp.fresh))
    | (Cons, ListLit([])) => DoesNotMatch
    | (List, Cast(l, s1, s2))
        when
          TypSlice.is_list(~ignore_parens=false, s1)
          && TypSlice.is_list(~ignore_parens=false, s2) =>
      // TODO: consider if incremental slices on the list should be retained or not here. (currently not)
      let* l = unbox(List, l);
      let l =
        List.map(
          d =>
            Cast(d, TypSlice.unlist(s1), TypSlice.unlist(s2)) |> DHExp.fresh,
          l,
        );
      let l = List.map(fixup_cast, l);
      Matches(l);
    | (ListLit(n), Cast(l, s1, s2))
        when TypSlice.is_list(s1) && TypSlice.is_list(s2) =>
      let* l = unbox(ListLit(n), l);
      let l =
        List.map(
          d =>
            Cast(d, TypSlice.unlist(s1), TypSlice.unlist(s2)) |> DHExp.fresh,
          l,
        );
      let l = List.map(fixup_cast, l);
      Matches(l);
    | (Cons, Cast(l, s1, s2))
        when
          TypSlice.is_list(~ignore_parens=false, s1)
          && TypSlice.is_list(~ignore_parens=false, s2) =>
      let* l = unbox(List, l);
      switch (l) {
      | [] => DoesNotMatch
      | [x, ...xs] =>
        Matches((
          Cast(x, TypSlice.unlist(s1), TypSlice.unlist(s2))
          |> DHExp.fresh
          |> fixup_cast,
          Cast(ListLit(xs) |> DHExp.fresh, s1, s2) |> DHExp.fresh,
        ))
      };

    /* Tuples can be either tuples or tuple casts */
    | (Tuple(n), Tuple(t)) when List.length(t) == n => Matches(t)
    | (Tuple(_), Tuple(_)) => DoesNotMatch
    | (Tuple(n), Cast(t, s1, s2))
        when
          n == List.length(TypSlice.unprod(s1))
          && n == List.length(TypSlice.unprod(s2)) =>
      let (s1s, s2s) = (TypSlice.unprod(s1), TypSlice.unprod(s2));
      let* t = unbox(Tuple(n), t);
      let t =
        ListUtil.map3(
          (d, s1, s2) => Cast(d, s1, s2) |> DHExp.fresh,
          t,
          s1s,
          s2s,
        );
      let t = List.map(fixup_cast, t);
      Matches(t);

    /* Sum constructors can be either sum constructors, sum constructors
       applied to some value or sum casts */
    | (SumNoArg(name1), Constructor(name2, _)) when name1 == name2 =>
      Matches()
    | (SumNoArg(_), Constructor(_)) => DoesNotMatch
    | (SumNoArg(_), Ap(_, {term: Constructor(_), _}, _)) => DoesNotMatch
    | (SumNoArg(name), Cast(d1, s1, s2))
        when
          TypSlice.is_sum(~ignore_parens=false, s1)
          && TypSlice.is_sum(~ignore_parens=false, s2)
          && (
            ConstructorMap.has_constructor_no_args(
              name,
              TypSlice.get_sum(s2.term),
            )
            || ConstructorMap.has_bad_entry(TypSlice.get_sum(s2.term))
          ) =>
      let* d1 = unbox(SumNoArg(name), d1);
      Matches(d1);
    | (SumNoArg(_), Cast(_, s1, s2))
        when TypSlice.is_sum(s1) && TypSlice.is_sum(s2) =>
      IndetMatch

    | (SumWithArg(_), Constructor(_)) => DoesNotMatch
    | (SumWithArg(name1), Ap(_, {term: Constructor(name2, _), _}, d3))
        when name1 == name2 =>
      Matches(d3)
    | (SumWithArg(_), Ap(_, {term: Constructor(_), _}, _)) => DoesNotMatch
    | (SumWithArg(name), Cast(d1, s1, s2))
        when TypSlice.is_sum(s1) && TypSlice.is_sum(s2) =>
      let get_entry_or_bad = s =>
        switch (ConstructorMap.get_entry(name, s)) {
        | Some(Some(x)) => Some(x)
        | Some(None) => None
        | None when ConstructorMap.has_bad_entry(s) =>
          Some(`Typ(Unknown(Internal)) |> TypSlice.temp)
        | None => None
        };
      switch (
        get_entry_or_bad(TypSlice.get_sum(s1.term)),
        get_entry_or_bad(TypSlice.get_sum(s2.term)),
      ) {
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
    | (Fun, Cast(d3', s1, s2))
        when TypSlice.is_arrow(s1) && TypSlice.is_arrow(s2) =>
      let ((s1, s2), (s1', s2')) = (
        TypSlice.unarrow(s1),
        TypSlice.unarrow(s2),
      );
      Matches(FunCast(d3', s1, s2, s1', s2'));
    | (Fun, BuiltinFun(name)) => Matches(BuiltinFun(name))
    | (Fun, DeferredAp(d1, ds)) => Matches(DeferredAp(d1, ds))

    /* TypFun-like things can look like the following when values */
    | (TypFun, Closure(env', {term: TypFun(utpat, tfbody, name), _})) =>
      Matches(TypFun(utpat, Closure(env', tfbody) |> Exp.fresh, name))
    | (TypFun, TypFun(utpat, tfbody, name)) =>
      Matches(TypFun(utpat, tfbody, name))
    // Note: We might be able to handle this cast like other casts
    | (TypFun, Cast(d'', s1, s2))
        when TypSlice.is_forall(s1) && TypSlice.is_forall(s2) =>
      let ((tp1, s1'), (tp2, s2')) = (
        TypSlice.unforall(s1),
        TypSlice.unforall(s2),
      );
      Matches(TFunCast(d'', tp1, s1', tp2, s2'));

    /* Any cast from unknown is indet */
    | (_, Cast(_, s1, _)) when TypSlice.is_unknown(s1) => IndetMatch

    /* Any failed cast is indet */
    | (_, FailedCast(_)) => IndetMatch

    /* Forms that are the wrong type of value - these cases indicate an error
       in elaboration or in the cast calculus. */
    | (
        _,
        Bool(_) | Int(_) | Float(_) | String(_) | Label(_) | Constructor(_) |
        BuiltinFun(_) |
        Deferral(_) |
        DeferredAp(_) |
        ListLit(_) |
        TupLabel(_) |
        Tuple(_) |
        Cast(_) |
        Ap(_, {term: Constructor(_), _}, _) |
        TypFun(_) |
        TypAp(_),
      ) =>
      switch (request) {
      | TupLabel(_) =>
        raise(EvaluatorError.Exception(InvalidBoxedTupLabel(expr)))
      | Bool => raise(EvaluatorError.Exception(InvalidBoxedBoolLit(expr)))
      | Int => raise(EvaluatorError.Exception(InvalidBoxedIntLit(expr)))
      | Float => raise(EvaluatorError.Exception(InvalidBoxedFloatLit(expr)))
      | String =>
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(expr)))
      | Label => raise(EvaluatorError.Exception(InvalidBoxedLabel(expr)))
      | Tuple(_) => raise(EvaluatorError.Exception(InvalidBoxedTuple(expr)))
      | List
      | ListLit(_)
      | Cons => raise(EvaluatorError.Exception(InvalidBoxedListLit(expr)))
      | SumNoArg(_)
      | SumWithArg(_) =>
        raise(EvaluatorError.Exception(InvalidBoxedSumConstructor(expr)))
      | Fun => raise(EvaluatorError.Exception(InvalidBoxedFun(expr)))
      | TypFun => raise(EvaluatorError.Exception(InvalidBoxedTypFun(expr)))
      }

    /* Forms that are not yet or will never be a value */
    | (
        _,
        Invalid(_) | Undefined | EmptyHole | MultiHole(_) | DynamicErrorHole(_) |
        Var(_) |
        Let(_) |
        Fun(_, _, _, _) |
        FixF(_) |
        TyAlias(_) |
        Ap(_) |
        If(_) |
        Seq(_) |
        Test(_) |
        Filter(_) |
        Closure(_) |
        Parens(_) |
        Probe(_) |
        Cons(_) |
        ListConcat(_) |
        Dot(_) |
        UnOp(_) |
        BinOp(_) |
        Match(_),
      ) =>
      IndetMatch
    };
  };
