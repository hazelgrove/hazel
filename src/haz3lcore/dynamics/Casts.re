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
    Prod(ListUtil.replicate(length, Typ.Unknown(Internal) |> Typ.temp))
    |> Typ.temp,
  );
let grounded_Sum: unit => Typ.sum_map =
  () => [BadEntry(Typ.temp(Unknown(Internal)))];
let grounded_List =
  NotGroundOrHole(List(Unknown(Internal) |> Typ.temp) |> Typ.temp);

let rec ground_cases_of = (ty: Typ.t): ground_cases => {
  let is_hole: Typ.t => bool =
    fun
    | {term: Typ.Unknown(_), _} => true
    | _ => false;
  switch (Typ.term_of(ty)) {
  | Unknown(_) => Hole
  | Bool
  | Int
  | Float
  | String
  | Var(_)
  | Rec(_)
  | Forall(_, {term: Unknown(_), _})
  | Arrow({term: Unknown(_), _}, {term: Unknown(_), _})
  | List({term: Unknown(_), _}) => Ground
  | Parens(ty) => ground_cases_of(ty)
  | Prod(tys) =>
    if (List.for_all(
          fun
          | ({term: Typ.Unknown(_), _}: Typ.t) => true
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
  | Cast(d1, t1, t2) =>
    let d1 =
      if (recursive) {
        d1 |> transition(~recursive) |> Option.value(~default=d1);
      } else {
        d1;
      };
    switch (ground_cases_of(t1), ground_cases_of(t2)) {
    | (Hole, Hole)
    | (Ground, Ground) =>
      /* if two types are ground and consistent, then they are eq */
      Some(d1) // Rule ITCastId

    | (Ground, Hole) =>
      /* can't remove the cast or do anything else here, so we're done */
      None

    | (Hole, Ground) =>
      switch (DHExp.term_of(d1)) {
      | Cast(d2, t3, {term: Unknown(_), _}) =>
        /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
        if (Typ.eq(t3, t2)) {
          Some
            (d2); // Rule ITCastSucceed
        } else {
          Some
            (FailedCast(d2, t3, t2) |> DHExp.fresh); // Rule ITCastFail
        }
      | _ => None
      }

    | (Hole, NotGroundOrHole(t2_grounded)) =>
      /* ITExpand rule */
      let inner_cast = Cast(d1, t1, t2_grounded) |> DHExp.fresh;
      // HACK: we need to check the inner cast here
      let inner_cast =
        switch (transition(~recursive, inner_cast)) {
        | Some(d1) => d1
        | None => inner_cast
        };
      Some(DHExp.Cast(inner_cast, t2_grounded, t2) |> DHExp.fresh);

    | (NotGroundOrHole(t1_grounded), Hole) =>
      /* ITGround rule */
      Some(
        DHExp.Cast(Cast(d1, t1, t1_grounded) |> DHExp.fresh, t1_grounded, t2)
        |> DHExp.fresh,
      )

    | (Ground, NotGroundOrHole(_)) =>
      switch (DHExp.term_of(d1)) {
      | Cast(d2, t3, _) =>
        if (Typ.eq(t3, t2)) {
          Some(d2);
        } else {
          None;
        }
      | _ => None
      }
    | (NotGroundOrHole(_), Ground) =>
      /* can't do anything when casting between diseq, non-hole types */
      None

    | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
      /* they might be eq in this case, so remove cast if so */
      if (Typ.eq(t1, t2)) {
        Some
          (d1); // Rule ITCastId
      } else {
        None;
      }
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
        {term: DHExp.Cast(d1, t1, t2), copied: p.copied, ids: p.ids}
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
      {term: DHPat.Cast(p1, t1, t2), copied: d.copied, ids: d.ids};
    | FailedCast(d1, t1, t2) =>
      let p1 = rewrap_casts((p, d1));
      {
        term:
          DHPat.Cast(
            DHPat.Cast(p1, t1, Typ.fresh(Unknown(Internal))) |> DHPat.fresh,
            Typ.fresh(Unknown(Internal)),
            t2,
          ),
        copied: d.copied,
        ids: d.ids,
      };
    | _ => failwith("unexpected term in rewrap_casts")
    };
  };
  p |> unwrap_casts |> rewrap_casts;
};
