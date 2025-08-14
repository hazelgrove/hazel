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
  | TFunCast(DHExp.t, TPat.t, TypSlice.t, TPat.t, TypSlice.t);

type unboxed_fun =
  | Constructor(string)
  | FunEnv(Pat.t, Exp.t, ClosureEnvironment.t)
  | FunNoEnv(Pat.t, Exp.t)
  | FunCast(DHExp.t, TypSlice.t, TypSlice.t, TypSlice.t, TypSlice.t)
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
  | Fun: unbox_request(unboxed_fun);

[@deriving (show({with_path: false}), eq)]
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
    | (ListLit, Cast(l, s1, s2))
        when
          TypSlice.is_list(~ignore_parens=false, s1)
          && TypSlice.is_list(~ignore_parens=false, s2) =>
      // TODO: consider if incremental slices on the list should be retained or not here. (currently not)
      let* l = unbox(ListLit, l);
      let l =
        List.map(
          d =>
            Cast(d, TypSlice.unlist(s1), TypSlice.unlist(s2)) |> DHExp.fresh,
          l,
        );
      let l = List.map(fixup_cast, l);
      Matches(l);
    | (ListLitn(n), Cast(l, s1, s2))
        when
          TypSlice.is_list(s1, ~ignore_parens=false)
          && TypSlice.is_list(s2, ~ignore_parens=false) =>
      let* l = unbox(ListLitn(n), l);
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
      let* l = unbox(Cons, l);
      switch (l) {
      | (x, xs) =>
        Matches((
          Cast(x, TypSlice.unlist(s1), TypSlice.unlist(s2))
          |> DHExp.fresh
          |> fixup_cast,
          Cast(xs, s1, s2) |> DHExp.fresh,
        ))
      };

    /* Tuples can be either tuples or tuple casts */
    | (Tuple(n), Tuple(t)) when List.length(t) == n => Matches(t)
    | (Tuple(_), Tuple(_)) => DoesNotMatch
    | (Tuple(n), Cast(t, s1, s2))
        when
          TypSlice.is_prod(s1, ~ignore_parens=false)
          && TypSlice.is_prod(s2, ~ignore_parens=false)
          && n == List.length(TypSlice.unprod(s1))
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
        when
          TypSlice.is_sum(s1, ~ignore_parens=false)
          && TypSlice.is_sum(s2, ~ignore_parens=false) =>
      IndetMatch
    /* Matches curried constructor. Note: does not check type consistency between arrow arg type
       and sum constrctor arg type -- TODO: fix (issue only occurs if two sum types use same ctr name
       and wrong one is passed into match, debatable if we even want to allow any matching at all here) */
    | (SumNoArg(name), Cast(_, _, s))
        when TypSlice.is_arrow(~ignore_parens=false, s) =>
      switch (unbox(Fun, expr)) {
      | Matches(Constructor(name')) when name == name' => Matches()
      | Matches(FunCast(d', _, _, _, _)) =>
        let* d' = unbox(SumNoArg(name), d');
        Matches(d');
      | _ => DoesNotMatch
      }

    | (SumWithArg(_), Constructor(_)) => DoesNotMatch
    | (SumWithArg(name1), Ap(_, {term: Constructor(name2, _), _}, d3))
        when name1 == name2 =>
      Matches(d3)
    | (SumWithArg(_), Ap(_, {term: Constructor(_), _}, _)) => DoesNotMatch
    | (SumWithArg(name), Cast(d1, s1, s2))
        when
          TypSlice.is_sum(s1, ~ignore_parens=false)
          && TypSlice.is_sum(s2, ~ignore_parens=false) =>
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
    | (Fun, Fun(dp, d3, _, _)) => Matches(FunNoEnv(dp, d3))
    | (Fun, Cast(d3', s1, s2))
        when
          TypSlice.is_arrow(s1, ~ignore_parens=false)
          && TypSlice.is_arrow(s2, ~ignore_parens=false) =>
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
        when
          TypSlice.is_forall(s1, ~ignore_parens=false)
          && TypSlice.is_forall(s2, ~ignore_parens=false) =>
      let ((tp1, s1'), (tp2, s2')) = (
        TypSlice.unforall(s1),
        TypSlice.unforall(s2),
      );
      Matches(TFunCast(d'', tp1, s1', tp2, s2'));

    /* Any cast from unknown is indet */
    | (_, Cast(_, s1, _)) when TypSlice.is_unknown(s1) => IndetMatch

    /* Any failed cast does not match. Why was this previously indet? Being indet breaks dynamic pattern matching. */
    | (_, FailedCast(_)) => DoesNotMatch

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
        Match(_),
      ) =>
      IndetMatch
    };
  };
