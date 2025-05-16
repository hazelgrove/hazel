open Util;

/* The cast calculus is based off the POPL 2019 paper:
   https://arxiv.org/pdf/1805.00155.pdf */

/* GROUND TYPES */

/* You can think of a ground type as a typet that tells you what the root of the
      type expression is, but nothing more. For example: Int, [?], ? -> ?, ... are
      ground types and [Int], ? -> Float are not.

      The most important property of ground types is:
          If two types are ground types,
          and the two types are consistent,
          then they are equal.

       Make sure this holds for your new feature!!

       e.g. [?] and [?] are equal, but [?] and [Int] are not (because [Int] is not
       ground, even though [Int] and [?] are consistent).

   */

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(Typ.t) /* the argument is the corresponding ground type */;

let grounded_Arrow =
  NotGroundOrHole(
    Arrow(Unknown(Internal) |> Typ.temp, Unknown(Internal) |> Typ.temp)
    |> Typ.temp,
  );
let grounded_Forall =
  NotGroundOrHole(
    Forall(EmptyHole |> TPat.fresh, Unknown(Internal) |> Typ.temp)
    |> Typ.temp,
  );
let grounded_Prod = length =>
  NotGroundOrHole(
    Prod(ListUtil.replicate(length, Unknown(Internal) |> Typ.temp))
    |> Typ.temp,
  );
let grounded_Sum: unit => Typ.sum_map =
  () => [BadEntry(Typ.temp(Unknown(Internal)))];
let grounded_List =
  NotGroundOrHole(List(Unknown(Internal) |> Typ.temp) |> Typ.temp);

let rec ground_cases_of = (ty: Typ.t): ground_cases => {
  let is_hole: Typ.t => bool =
    fun
    | {term: Unknown(_), _} => true
    | _ => false;
  switch (Typ.term_of(ty)) {
  | Unknown(_) => Hole
  | Atom(_)
  | Label(_)
  | TupLabel(_, {term: Unknown(_), _})
  | Var(_)
  | Rec(_)
  | Forall(_, {term: Unknown(_), _})
  | Arrow({term: Unknown(_), _}, {term: Unknown(_), _})
  | List({term: Unknown(_), _}) => Ground
  | Parens(ty) => ground_cases_of(ty)
  | TupLabel(label, _) =>
    NotGroundOrHole(
      TupLabel(label, Unknown(Internal) |> Typ.temp) |> Typ.temp,
    )
  | Prod(tys) =>
    if (List.for_all(
          fun
          | ({term: Unknown(_), _}: Typ.t) => true
          | _ => false,
          tys,
        )) {
      Ground;
    } else {
      tys |> List.length |> grounded_Prod;
    }
  | Sum(sm) =>
    sm |> ConstructorMap.is_ground(is_hole)
      ? Ground : NotGroundOrHole(Sum(grounded_Sum()) |> Typ.temp)
  | Arrow(_, _) => grounded_Arrow
  | Forall(_) => grounded_Forall
  | List(_) => grounded_List
  | Ap(_) => failwith("type application in dynamics")
  };
};

/* CAST CALCULUS */

/* Rules are taken from figure 12 of https://arxiv.org/pdf/1805.00155.pdf  */

/* gives a transition step that can be taken by the cast calculus here if applicable. */
let rec transition = (~recursive=false, d: DHExp.t): option(DHExp.t) => {
  let recur = (d: DHExp.t): DHExp.t =>
    if (recursive) {
      transition(d) |> Option.value(~default=d);
    } else {
      d;
    };
  switch (DHExp.term_of(d)) {
  | Cast(e, _, t) =>
    switch (DHExp.term_of(e), Typ.term_of(Typ.unroll(t))) {
    | (e, Parens(t)) =>
      // This is an impossible case since types should be normalized before coming to transitions
      transition(
        ~recursive,
        Cast(e |> DHExp.fresh, Unknown(Internal) |> Typ.temp, t)
        |> DHExp.fresh,
      )
    | (Closure(ce, d), t) =>
      transition(
        ~recursive,
        Cast(d, Unknown(Internal) |> Typ.fresh, t |> Typ.fresh)
        |> DHExp.fresh,
      )
      |> Option.map(d => Closure(ce, d) |> DHExp.fresh)
    | (Fun(p, e, t, v), Arrow(t1, t2)) =>
      Some(
        IdTagged.FreshGrammar.(
          Exp.(fn(Pat.(asc(p, t1)), asc(e, t2), t, v))
        ),
      )
    | (TupLabel(l, e), TupLabel(_l2, t)) =>
      // TODO Figure out what to do if the labels don't match
      Some(
        TupLabel(
          l,
          recur(Cast(e, Unknown(Internal) |> Typ.temp, t) |> DHExp.fresh),
        )
        |> DHExp.fresh,
      )
    | (Tuple(es), Prod(tys)) when List.length(es) == List.length(tys) =>
      Some(
        Tuple(
          List.map2(
            (e, ty) =>
              recur(
                Cast(e, Unknown(Internal) |> Typ.temp, ty) |> DHExp.fresh,
              ),
            es,
            tys,
          ),
        )
        |> DHExp.fresh,
      )
    | (e, Unknown(_)) => Some(e |> DHExp.fresh)
    | (Atom(value) as d, Atom(typ)) =>
      switch (value, typ) {
      | (Int(_), Int)
      | (String(_), String)
      | (Nat(_), Nat)
      | (Float(_), Float)
      | (SInt(_), SInt)
      | (Bool(_), Bool) => Some(d |> Exp.fresh)
      | (Int(_), _)
      | (String(_), _)
      | (Nat(_), _)
      | (Float(_), _)
      | (SInt(_), _)
      | (Bool(_), _) => None
      }
    | (ListLit(ds), List(ty)) =>
      Some(
        ListLit(
          List.map(
            d =>
              recur(
                Cast(d, Unknown(Internal) |> Typ.temp, ty) |> DHExp.fresh,
              ),
            ds,
          ),
        )
        |> DHExp.fresh,
      )
    | (Cons(d1, d2), List(ty)) =>
      Some(
        Cons(
          recur(Cast(d1, Unknown(Internal) |> Typ.temp, ty) |> DHExp.fresh),
          recur(Cast(d2, Unknown(Internal) |> Typ.temp, t) |> DHExp.fresh),
        )
        |> DHExp.fresh,
      )
    | (TypFun(tp, e, v), Forall(tp', t')) =>
      let new_ty: Typ.t =
        switch (TPat.tyvar_of_utpat(tp)) {
        | Some(tyvar) => Var(tyvar) |> Typ.temp
        | None => Unknown(Internal) |> Typ.temp
        };
      Some(
        TypFun(
          tp,
          recur(
            Cast(
              e,
              Unknown(Internal) |> Typ.temp,
              Typ.subst(new_ty, tp', t'),
            )
            |> DHExp.fresh,
          ),
          v,
        )
        |> DHExp.fresh,
      );
    | (If(e, e1, e2), t) =>
      // Should we do this or leave it if it isn't a value
      Some(
        If(
          recur(
            Cast(e, Unknown(Internal) |> Typ.temp, t |> Typ.temp)
            |> DHExp.fresh,
          ),
          recur(
            Cast(e1, Unknown(Internal) |> Typ.temp, t |> Typ.temp)
            |> DHExp.fresh,
          ),
          recur(
            Cast(e2, Unknown(Internal) |> Typ.temp, t |> Typ.temp)
            |> DHExp.fresh,
          ),
        )
        |> DHExp.fresh,
      )
    | (
        Ap(
          Forward,
          {term: Constructor(c, Some(Some({term: Arrow(_, sumt), _}))), _} as con,
          payload,
        ),
        Sum(m) as sumt',
      )
        when Typ.fast_equal(Typ.unroll(sumt), sumt' |> Typ.temp) =>
      // I would like to unroll/normalize somewhere else
      let entry = ConstructorMap.get_entry(c, m);
      switch (entry) {
      | Some(Some(t')) =>
        Some(
          Ap(
            Forward,
            con,
            recur(
              Cast(payload, Unknown(Internal) |> Typ.temp, t') |> DHExp.fresh,
            ),
          )
          |> DHExp.fresh,
        )
      | Some(None)
      | None => None
      };
    | (Constructor(_, Some(Some(t))), t')
        when Typ.fast_equal(Typ.unroll(t), t' |> Typ.temp) =>
      // Make sure that we don't need to handle the none cases. Also think about what to do if the type has a payload and it's just a constructor
      Some(e)
    | (Test(_), Prod([])) => Some(d)
    // These are non-value cases we don't want to handle
    | (EmptyHole, _)
    | (FailedCast(_), _)
    | (DynamicErrorHole(_), _)
    | (Dot(_), _)
    | (Undefined, _)
    | (Invalid(_), _)
    | (MultiHole(_), _)
    | (Label(_), _)
    | (Var(_), _)
    | (Ap(_), _)
    | (DeferredAp(_), _)
    | (Deferral(_), _)
    | (LivelitName(_), _)
    | (Probe(_, _), _)
    // We _could_ do this, but it would be a bit weird
    | (Let(_), _)
    | (Use(_), _)
    | (BinOp(_), _)
    | (UnOp(_), _)
    | (BuiltinFun(_), _)
    | (FixF(_), _)
    | (TypAp(_), _)
    | (Seq(_), _)
    | (Filter(_), _)
    | (Parens(_), _)
    | (TyAlias(_), _)
    | (ListConcat(_), _)
    | (Match(_), _)
    | (Cast(_), _) => None
    // These are handled above and must have the wrong type
    | (Atom(_), _)
    | (ListLit(_), _)
    | (TupLabel(_), _)
    | (Tuple(_), _)
    | (Fun(_), _)
    | (TypFun(_), _)
    | (Test(_), _)
    | (Cons(_), _)
    | (Constructor(_), _) => None
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
