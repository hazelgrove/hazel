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
  | Tuple(int): unbox_request(list(DHExp.t))
  | List: unbox_request(list(DHExp.t))
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
    //TODO: move all these to TypSlice
    let unparens = TypSlice.unparens;
    let unlist =
      TypSlice.map_merge(
        ~drop_incr=true,
        fun
        | List(x) => TypSlice.t_of_typ_t(x)
        | _ => failwith("Not a list"),
        fun
        | List(x) => x
        | _ => failwith("Not a a list"),
      );
    let unprod = (s: TypSlice.t) => {
      let unprod =
        TypSlice.apply(
          fun
          | Prod(tys) => tys |> List.map(TypSlice.t_of_typ_t)
          | _ => failwith("Not a product"),
          fun
          | Prod(ss) => ss
          | _ => failwith("Not a product"),
        );
      let s = TypSlice.term_of(s);
      switch (s) {
      | `Typ(_)
      | `SliceIncr(_) => unprod(s) // Drop incremental slices
      | `SliceGlobal(_, slice_global) =>
        unprod(s) |> List.map(TypSlice.wrap_global(slice_global))
      };
    };

    let unarrow = (s: TypSlice.t) => {
      let unarrow =
        TypSlice.apply(
          fun
          | Arrow(ty1, ty2) =>
            (ty1, ty2) |> TupleUtil.map2(TypSlice.t_of_typ_t)
          | _ => failwith("Not an arrow"),
          fun
          | Arrow(s1, s2) => (s1, s2)
          | _ => failwith("Not an arrow"),
        );
      let s = TypSlice.term_of(s);
      switch (s) {
      | `Typ(_)
      | `SliceIncr(_) => unarrow(s) // Drop incremental slices
      | `SliceGlobal(_, slice_global) =>
        unarrow(s) |> TupleUtil.map2(TypSlice.wrap_global(slice_global))
      };
    };
    // get forall term
    let unforall = (s: TypSlice.t) => {
      // TODO: Move these into TypSlice.re?
      let unforall =
        TypSlice.apply(
          fun
          | Forall(tpat, ty) => (tpat, ty |> TypSlice.t_of_typ_t)
          | _ => failwith("Not a forall"),
          fun
          | Forall(tpat, s) => (tpat, s)
          | _ => failwith("Not a forall"),
        );
      let s = TypSlice.term_of(s);
      switch (s) {
      | `Typ(_)
      | `SliceIncr(_) => unforall(s) // Drop incremental slices
      | `SliceGlobal(_, slice_global) =>
        unforall(s)
        |> (((x, y)) => (x, y |> TypSlice.wrap_global(slice_global)))
      };
    };

    let get_sum =
      TypSlice.apply(
        fun
        | Sum(m) => m |> ConstructorMap.map_vals(TypSlice.t_of_typ_t)
        | _ => failwith("Not a sum"),
        fun
        | Sum(m) => m
        | _ => failwith("Not a sum"),
      );

    switch (request, DHExp.term_of(expr)) {
    /* Remove parentheses from casts */
    | (_, Cast(d, s1, s2)) when TypSlice.is_parens(s1) =>
      unbox(request, Cast(d, unparens(s1), s2) |> DHExp.fresh)
    | (_, Cast(d, s1, s2)) when TypSlice.is_parens(s2) =>
      unbox(request, Cast(d, s1, unparens(s2)) |> DHExp.fresh)
    /* Base types are always already unboxed because of the ITCastID rule*/
    | (Bool, Bool(b)) => Matches(b)
    | (Int, Int(i)) => Matches(i)
    | (Float, Float(f)) => Matches(f)
    | (String, String(s)) => Matches(s)

    /* Lists can be either lists or list casts */
    | (List, ListLit(l)) => Matches(l)
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
        List.map(d => Cast(d, unlist(s1), unlist(s2)) |> DHExp.fresh, l);
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
          Cast(x, unlist(s1), unlist(s2)) |> DHExp.fresh |> fixup_cast,
          Cast(ListLit(xs) |> DHExp.fresh, s1, s2) |> DHExp.fresh,
        ))
      };

    /* Tuples can be either tuples or tuple casts */
    | (Tuple(n), Tuple(t)) when List.length(t) == n => Matches(t)
    | (Tuple(_), Tuple(_)) => DoesNotMatch
    | (Tuple(n), Cast(t, s1, s2))
        when n == List.length(unprod(s1)) && n == List.length(unprod(s2)) =>
      let (s1s, s2s) = (unprod(s1), unprod(s2));
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
            ConstructorMap.has_constructor_no_args(name, get_sum(s2.term))
            || ConstructorMap.has_bad_entry(get_sum(s2.term))
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
        | Some(x) => Some(x)
        | None when ConstructorMap.has_bad_entry(s) =>
          Some(`Typ(Unknown(Internal)) |> TypSlice.temp)
        | None => None
        };
      switch (
        get_entry_or_bad(get_sum(s1.term)),
        get_entry_or_bad(get_sum(s2.term)),
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
      let ((s1, s2), (s1', s2')) = (unarrow(s1), unarrow(s2));
      Matches(FunCast(d3', s1, s2, s1', s2'));
    | (Fun, BuiltinFun(name)) => Matches(BuiltinFun(name))
    | (Fun, DeferredAp(d1, ds)) => Matches(DeferredAp(d1, ds))

    /* TypFun-like things can look like the following when values */
    | (TypFun, TypFun(utpat, tfbody, name)) =>
      Matches(TypFun(utpat, tfbody, name))
    // Note: We might be able to handle this cast like other casts
    | (TypFun, Cast(d'', s1, s2))
        when TypSlice.is_forall(s1) && TypSlice.is_forall(s2) =>
      let ((tp1, s1'), (tp2, s2')) = (unforall(s1), unforall(s2));
      Matches(TFunCast(d'', tp1, s1', tp2, s2'));

    /* Any cast from unknown is indet */
    | (_, Cast(_, s1, _)) when TypSlice.is_unknown(s1) => IndetMatch

    /* Any failed cast is indet */
    | (_, FailedCast(_)) => IndetMatch

    /* Forms that are the wrong type of value - these cases indicate an error
       in elaboration or in the cast calculus. */
    | (
        _,
        Bool(_) | Int(_) | Float(_) | String(_) | Constructor(_) |
        BuiltinFun(_) |
        Deferral(_) |
        DeferredAp(_) |
        ListLit(_) |
        Tuple(_) |
        Cast(_) |
        Ap(_, {term: Constructor(_), _}, _) |
        TypFun(_) |
        TypAp(_),
      ) =>
      switch (request) {
      | Bool => raise(EvaluatorError.Exception(InvalidBoxedBoolLit(expr)))
      | Int => raise(EvaluatorError.Exception(InvalidBoxedIntLit(expr)))
      | Float => raise(EvaluatorError.Exception(InvalidBoxedFloatLit(expr)))
      | String =>
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(expr)))
      | Tuple(_) => raise(EvaluatorError.Exception(InvalidBoxedTuple(expr)))
      | List
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
        Cons(_) |
        ListConcat(_) |
        UnOp(_) |
        BinOp(_) |
        Match(_),
      ) =>
      IndetMatch
    };
  };
