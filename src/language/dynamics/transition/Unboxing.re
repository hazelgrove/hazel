open Util;

/* What is unboxing?

   When you have an expression of type list, and it's finished evaluating,
   is it a list literal? Sadly not necessarily, it might be:

    - an indeterminate list cons: e.g. 1 :: ?
    - an indet term in a ascription to a list type

    Unboxing is the process of turning a list literal into a list literal if it is a list literal, or giving up if it is not a list literal.
    It may give up in two distinct ways:
    - IndetMatch: Due to holes in the expression it may or may not match the unboxing request
                  depending on possible substitutions of the holes.
                  e.g. 1 :: ? might match a list of length 3 (LitLitn(3))
                  or   ? : [Int] might match a list (of any length)
    - DoesNotMatch: Could not possibly match the unboxing request
                  e.g. 1 :: ? definitely does NOT match a list of length 0 (ListLitn(0))

    Note unboxing only works one layer deep, if we have a list of lists then
    the inner lists may still have indet ascriptions around them after unboxing.
    */

type unboxed_tfun =
  | TypFun(TPat.t, Exp.t, option(string));

[@deriving show({with_path: false})]
type unboxed_fun =
  | Constructor(string)
  | FunEnv(Pat.t, Exp.t, [@show.opaque] Environment.t(Exp.t))
  | FunNoEnv(Pat.t, Exp.t)
  | BuiltinFun(string)
  | DeferredAp(DHExp.t, list(DHExp.t));

type unbox_request('a) =
  | Atom(Atom.kind('a)): unbox_request('a)
  | Label: unbox_request(string)
  | Tuple(int): unbox_request(list(DHExp.t))
  | TupLabel(DHPat.t): unbox_request(DHExp.t)
  | ListLit: unbox_request(list(DHExp.t)) // Unboxes to a known length list LITERAL. Not all list final forms land in this category (e.g. Cons: 1 :: ?)
  | ListLitn(int): unbox_request(list(DHExp.t)) // This request is used for performance reasons to prevent matching lists of the wrong length and for matching list lits against cons expressions
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
let fixup_ascriptions = Ascriptions.transition_multiple;

/* This function has a different return type depending on what kind of request
   it is given. This unfortunately uses a crazy OCaml feature called GADTS, but
   it avoids having to write a separate unbox function for each kind of request.
   */

let rec unbox: type a. (unbox_request(a), DHExp.t) => unboxed(a) =
  (request, expr) => {
    switch (request, DHExp.term_of(expr)) {
    /* $e and $v could have any type, but are indet */
    | (_, Parens(e)) => unbox(request, e) // TODO Currently for Proj only move this into there
    | (_, UnOp(Meta(Unquote), _)) => IndetMatch
    | (_, Constructor(c, _)) when String.starts_with(c, ~prefix="$") =>
      IndetMatch

    /* proofs can also have any type, but are indet */
    | (_, ProofObject(_)) => IndetMatch

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
    | (TupLabel(_), _) => Matches(expr)
    /* Remove Tuplabels from casts otherwise */
    | (Label, Label(l)) => Matches(l)
    | (LabeledTupleProjection(l), Tuple(ds)) =>
      switch (LabeledTuple.find_label(Exp.match_tup_label, ds, l)) {
      | Some(_) => Matches(expr) // Choosing to just return the original expression here
      | _ => IndetMatch // TODO Should this be DoesNotMatch?
      }
    | (LabeledTupleProjection(_), ListLit(_)) => Matches(expr)
    | (TupleElementPivot(l), Tuple(ds)) =>
      let found_pivot: option((string, list(Exp.t))) =
        ListUtil.find_with_rest(
          exp => {
            switch (Exp.match_tup_label(DHExp.strip_ascriptions(exp))) {
            | Some((name, {term: Atom(String(e)), _})) when name == l =>
              Some(e)
            | _ => None
            }
          },
          ds,
        );
      switch (found_pivot) {
      | None => DoesNotMatch
      | Some(a) => Matches(a)
      };

    | (LabeledTupleEntries, Tuple(ds)) =>
      let unbox_tup_label =
          (d: Exp.t): option((option(LabeledTuple.label), Exp.t)) => {
        switch (d.term) {
        | TupLabel({term: Label(l), _}, e) => Some((Some(l), e))
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
      | None => IndetMatch
      }

    /* Lists can be either lists or cons with indet tail */
    | (ListLit, ListLit(l)) => Matches(l)
    | (ListLitn(n), ListLit(l)) when ListUtil.is_length(n, l) => Matches(l)
    | (ListLitn(_), ListLit(_)) => DoesNotMatch
    /* A cons final form is always indet, so either does NOT match or indet matches with a listliteral*/
    | (ListLitn(0), Cons(_)) => DoesNotMatch // Cons is not an empty list
    | (ListLitn(n), Cons(x, xs)) =>
      let* tail = unbox(ListLitn(n - 1), xs);
      Matches([x, ...tail]);
    | (ListLit, Cons(_)) => IndetMatch // Without length of ListLit we cannot know

    | (Cons, ListLit([x, ...xs])) =>
      Matches((x, ListLit(xs) |> DHExp.fresh))
    | (Cons, ListLit([])) => DoesNotMatch
    | (Cons, Cons(x, xs)) => Matches((x, xs))
    /* Tuples  */
    | (Tuple(n), Tuple(t)) when List.length(t) == n => Matches(t)
    | (Tuple(_), Tuple(_)) => IndetMatch
    /* Sum constructors can be either sum constructors or sum constructors
       applied to some value  */
    | (SumNoArg(name1), Constructor(name2, _)) when name1 == name2 =>
      Matches()
    | (SumNoArg(_), Constructor(_)) => DoesNotMatch
    | (SumNoArg(_), Ap(_, {term: Constructor(_), _}, _)) => DoesNotMatch
    | (SumWithArg(_), Constructor(_)) => DoesNotMatch
    | (SumWithArg(name1), Ap(_, {term: Constructor(name2, _), _}, d3))
        when name1 == name2 =>
      Matches(d3)
    | (SumWithArg(_), Ap(_, {term: Constructor(_), _}, _)) => DoesNotMatch
    /* Function-like things can look like the following when values */
    | (Fun, Constructor(name, _)) => Matches(Constructor(name)) // Perhaps we should check if the constructor actually is a function?
    | (Fun, Closure(env', {term: Fun(dp, d3, _, _), _})) =>
      Matches(FunEnv(dp, d3, env'))
    | (Fun, Fun(dp, d3, _, _)) => Matches(FunNoEnv(dp, d3))
    | (Fun, BuiltinFun(name)) => Matches(BuiltinFun(name))
    | (Fun, DeferredAp(d1, ds)) => Matches(DeferredAp(d1, ds))

    /* TypFun-like things can look like the following when values */
    | (TypFun, Closure(env', {term: TypFun(utpat, tfbody, name), _})) =>
      Matches(TypFun(utpat, Closure(env', tfbody) |> Exp.fresh, name))
    | (TypFun, TypFun(utpat, tfbody, name)) =>
      Matches(TypFun(utpat, tfbody, name))
    /* Forms that are the wrong type of value - these cases indicate an error */
    | (
        _,
        Atom(_) | Label(_) | Constructor(_) | BuiltinFun(_) | Deferral(_) |
        DeferredAp(_) |
        ListLit(_) |
        Cons(_) |
        TupLabel(_) |
        Tuple(_) |
        Asc(_) |
        TypFun(_, _, _) |
        Ap(_, {term: Constructor(_), _}, _),
      ) =>
      switch (request) {
      | TupLabel(_)
      | Atom(Bool)
      | Atom(SInt)
      | Atom(Int)
      | Atom(Float)
      | Atom(String)
      | Atom(Nat)
      | Label
      | Tuple(_)
      | ListLit
      | ListLitn(_)
      | Cons
      | SumNoArg(_)
      | TupleElementPivot(_)
      | LabeledTupleProjection(_)
      | LabeledTupleEntries
      | SumWithArg(_)
      | Fun
      | TypFun => IndetMatch
      }

    /* Forms that are not yet or will never be a value */
    | (
        _,
        Invalid(_) | Undefined | EmptyHole | MultiHole(_) | DynamicErrorHole(_) |
        ExplicitNonlabel |
        Var(_) |
        Let(_) |
        Theorem(_) |
        Forall(_) |
        Fun(_, _, _, _) |
        TypAp(_) |
        FixF(_) |
        TyAlias(_) |
        Use(_) |
        Ap(_) |
        If(_) |
        Seq(_) |
        Test(_) |
        HintedTest(_) |
        Filter(_) |
        Closure(_) |
        Probe(_) |
        ListConcat(_) |
        TupleExtension(_) |
        Dot(_) |
        UnOp(_) |
        BinOp(_) |
        LivelitName(_) |
        Match(_),
      ) =>
      IndetMatch
    };
  };
