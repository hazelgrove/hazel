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

type unbox_request('a) =
  | Int: unbox_request(int)
  | Float: unbox_request(float)
  | Bool: unbox_request(bool)
  | String: unbox_request(string)
  | Tuple(int): unbox_request(list(DHExp.t))
  | List: unbox_request(list(DHExp.t))
  | Cons: unbox_request((DHExp.t, DHExp.t))
  | SumNoArg(string): unbox_request(unit)
  | SumWithArg(string): unbox_request(DHExp.t);

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
    | (_, Cast(d, {term: Parens(x), _}, y))
    | (_, Cast(d, x, {term: Parens(y), _})) =>
      unbox(request, Cast(d, x, y) |> DHExp.fresh)

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
    | (List, Cast(l, {term: List(t1), _}, {term: List(t2), _})) =>
      let* l = unbox(List, l);
      let l = List.map(d => Cast(d, t1, t2) |> DHExp.fresh, l);
      let l = List.map(fixup_cast, l);
      Matches(l);
    | (
        Cons,
        Cast(l, {term: List(t1), _} as ct1, {term: List(t2), _} as ct2),
      ) =>
      let* l = unbox(List, l);
      switch (l) {
      | [] => DoesNotMatch
      | [x, ...xs] =>
        Matches((
          Cast(x, t1, t2) |> DHExp.fresh |> fixup_cast,
          Cast(ListLit(xs) |> DHExp.fresh, ct1, ct2) |> DHExp.fresh,
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
    | (SumNoArg(name1), Constructor(name2)) when name1 == name2 => Matches()
    | (SumNoArg(_), Constructor(_)) => DoesNotMatch
    | (SumNoArg(_), Ap(_, {term: Constructor(_), _}, _)) => DoesNotMatch
    | (SumNoArg(name), Cast(d1, {term: Sum(_), _}, {term: Sum(s2), _}))
        when ConstructorMap.has_constructor_no_args(name, s2) =>
      let* d1 = unbox(SumNoArg(name), d1);
      Matches(d1);
    | (SumNoArg(_), Cast(_, {term: Sum(_), _}, {term: Sum(_), _})) =>
      IndetMatch

    | (SumWithArg(_), Constructor(_)) => DoesNotMatch
    | (SumWithArg(name1), Ap(_, {term: Constructor(name2), _}, d3))
        when name1 == name2 =>
      Matches(d3)
    | (SumWithArg(_), Ap(_, {term: Constructor(_), _}, _)) => DoesNotMatch
    | (SumWithArg(name), Cast(d1, {term: Sum(_), _}, {term: Sum(s2), _}))
        when ConstructorMap.get_entry(name, s2) != None =>
      let* d1 = unbox(SumWithArg(name), d1);
      Matches(d1 |> fixup_cast);
    | (SumWithArg(_), Cast(_, {term: Sum(_), _}, {term: Sum(_), _})) =>
      IndetMatch

    /* Any cast from unknown is indet */
    | (_, Cast(_, {term: Unknown(_), _}, _)) => IndetMatch

    /* Forms that are the wrong type of value - these cases indicate an error
       in elaboration or in the cast calculus. */
    | (
        _,
        Bool(_) | Int(_) | Float(_) | String(_) | Constructor(_) |
        BuiltinFun(_) |
        Deferral(_) |
        DeferredAp(_) |
        Fun(_, _, _, Some(_)) |
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
      }

    /* Forms that are not yet or will never be a value */
    | (
        _,
        Invalid(_) | EmptyHole | MultiHole(_) | DynamicErrorHole(_) |
        FailedCast(_) |
        Var(_) |
        Let(_) |
        Fun(_, _, _, None) |
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
