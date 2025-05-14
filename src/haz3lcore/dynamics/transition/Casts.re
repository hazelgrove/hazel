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
  switch (DHExp.term_of(d)) {
  | Cast(e, _, t) =>
    switch (DHExp.term_of(e), Typ.term_of(t)) {
    | (e, Parens(t)) =>
      // TODO: We need to normalize types to handle aliases? We can consider doing it in elaboration
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
          Cast(e, Unknown(Internal) |> Typ.temp, t) |> DHExp.fresh,
        )
        |> DHExp.fresh,
      )
    | (Tuple(es), Prod(tys)) when List.length(es) == List.length(tys) =>
      Some(
        Tuple(
          List.map2(
            (e, ty) =>
              Cast(e, Unknown(Internal) |> Typ.temp, ty) |> DHExp.fresh,
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
            d => Cast(d, Unknown(Internal) |> Typ.temp, ty) |> DHExp.fresh,
            ds,
          ),
        )
        |> DHExp.fresh,
      )
    | _ => None
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

// So that we don't have to regenerate its id
let hole = EmptyHole |> DHExp.fresh;

// Hacky way to do transition_multiple on patterns by transferring
// the cast to the expression and then back to the pattern.
let pattern_fixup = (p: DHPat.t): DHPat.t => {
  let rec unwrap_casts = (p: DHPat.t): (DHPat.t, DHExp.t) => {
    switch (DHPat.term_of(p)) {
    | Cast(p1, t1, t2) =>
      let (p1, d1) = unwrap_casts(p1);
      (
        p1,
        {
          term: Cast(d1, t1, t2),
          annotation: p.annotation,
        }
        |> transition_multiple,
      );
    | _ => (p, hole)
    };
  };
  let rec rewrap_casts = ((p: DHPat.t, d: DHExp.t)): DHPat.t => {
    switch (DHExp.term_of(d)) {
    | EmptyHole => p
    | Cast(d1, t1, t2) =>
      let p1 = rewrap_casts((p, d1));
      {
        term: Cast(p1, t1, t2),
        annotation: d.annotation,
      };
    | FailedCast(d1, t1, t2) =>
      let p1 = rewrap_casts((p, d1));
      {
        term:
          Cast(
            Cast(p1, t1, Typ.fresh(Unknown(Internal))) |> DHPat.fresh,
            Typ.fresh(Unknown(Internal)),
            t2,
          ),
        annotation: d.annotation,
      };
    | _ => failwith("unexpected term in rewrap_casts")
    };
  };
  p |> unwrap_casts |> rewrap_casts;
};
