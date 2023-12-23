type ptyp =
  | Int
  | Float
  | Bool
  | String
  | Var(string)
  | List(pts)
  | Arrow(pts, pts)
  | Sum(list(pts))
  | Prod(list(pts))
and pts = UnionFind.elem(list(ptyp));

module Ctx = {
  type t = Hashtbl.t(Typ.type_provenance, pts);

  let create = (): t => Hashtbl.create(100);

  let lookup = Hashtbl.find_opt;

  let lookup_or_create = (ctx: t, p: Typ.type_provenance): pts => {
    let lookup = Hashtbl.find_opt(ctx, p);
    switch (lookup) {
    | Some(pts) => pts
    | None =>
      let pts = UnionFind.make([]);
      Hashtbl.add(ctx, p, pts);
      pts;
    };
  };
};

let rec pts_of_typ = (ctx: Ctx.t, t: Typ.t): pts => {
  switch (t) {
  | Typ.Unknown(p, _) => Ctx.lookup_or_create(ctx, p)
  | _ =>
    let ptyp = ptyp_of_typ(ctx, t);
    UnionFind.make([ptyp]);
  };
}
and ptyp_of_typ = (ctx: Ctx.t, t: Typ.t): ptyp => {
  switch (t) {
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | Var(s) => Var(s)
  | List(t) => List(pts_of_typ(ctx, t))
  | Arrow(t1, t2) => Arrow(pts_of_typ(ctx, t1), pts_of_typ(ctx, t2))
  | Sum(_) => Sum([]) // TODO anand and raef: unimplemented
  | Rec(_) => Sum([]) // TODO anand and raef: unimplemented
  | Prod(ts) => Prod(List.map(pts_of_typ(ctx), ts))
  | Typ.Unknown(_p, _) => failwith("unreachable")
  };
};

// return true if pts1 contains pts2
let rec contains = (pts1: pts, pts2: pts): bool =>
  if (UnionFind.eq(pts1, pts2)) {
    true;
  } else {
    let pts1_tys = UnionFind.get(pts1);
    List.exists(contains_helper(pts2), pts1_tys);
  }
// return true if ptyp contains pts2
and contains_helper = (pts2: pts, ptyp: ptyp): bool => {
  switch (ptyp) {
  | Int
  | Float
  | Bool
  | String
  | Var(_) => false
  | List(pts1) => contains(pts1, pts2)
  | Arrow(pts1, pts2) => contains(pts1, pts2) || contains(pts2, pts2)
  | Sum(tys) => List.exists(contains(pts2), tys)
  | Prod(tys) => List.exists(contains(pts2), tys)
  };
};

// merge two pts
let rec merge = (ctx: Ctx.t, pts1: pts, pts2: pts): pts =>
  // TODO: if pts1 contains pts2 or vice versa, pick one arbitrarily and return it
  if (contains(pts1, pts2) || contains(pts2, pts1)) {
    pts1;
  } else {
    let pts3 = merge_helper(ctx, pts1, pts2);
    let representative = UnionFind.union(pts1, pts2);
    let _ = UnionFind.set(representative, pts3);
    representative;
  }
and merge_helper = (ctx: Ctx.t, pts1: pts, pts2: pts): list(ptyp) => {
  let tys1 = UnionFind.get(pts1);
  let tys2 = UnionFind.get(pts2);
  List.fold_left(extend(ctx), tys1, tys2);
}
and extend = (ctx: Ctx.t, tys: list(ptyp), ptyp: ptyp): list(ptyp) => {
  let (newlist, combined) =
    List.fold_left(
      ((newlist, combined), element) => {
        switch (combine_if_similar(ctx, ptyp, element)) {
        | Some(ptyp_combined) => ([ptyp_combined, ...newlist], true)
        | None => ([element, ...newlist], combined)
        }
      },
      ([], false),
      tys,
    );
  if (combined) {
    newlist;
  } else {
    [ptyp, ...newlist];
  };
}
and combine_if_similar =
    (ctx: Ctx.t, ptyp1: ptyp, ptyp2: ptyp): Option.t(ptyp) => {
  switch (ptyp1, ptyp2) {
  // the same
  | (Int, Int) => Some(Int)
  | (Float, Float) => Some(Float)
  | (Bool, Bool) => Some(Bool)
  | (String, String) => Some(String)
  | (Var(s1), Var(s2)) when s1 == s2 => Some(Var(s1))
  // similar, merge children
  | (List(pts1), List(pts2)) =>
    let pts = merge(ctx, pts1, pts2);
    Some(List(pts));
  | (Arrow(pts1, pts2), Arrow(pts3, pts4)) =>
    let pts1 = merge(ctx, pts1, pts3);
    let pts2 = merge(ctx, pts2, pts4);
    Some(Arrow(pts1, pts2));
  | (Sum(tys1), Sum(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      let tys = List.map2(merge(ctx), tys1, tys2);
      Some(Sum(tys));
    }
  | (Prod(tys1), Prod(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      let tys = List.map2(merge(ctx), tys1, tys2);
      Some(Prod(tys));
    }
  // different, doesn't combine
  | _ => None
  };
};

// API
let constrain = (ctx: Ctx.t, t1: Typ.t, t2: Typ.t): unit => {
  let pts1 = pts_of_typ(ctx, t1);
  let pts2 = pts_of_typ(ctx, t2);
  let _ = merge(ctx, pts1, pts2);
  ();
};

type status =
  | Solved(Typ.t)
  | Unsolved(list(Typ.t));

let unwrap_solution = (s: status): Typ.t => {
  switch (s) {
  | Solved(ty) => ty
  | Unsolved([]) => Unknown(NoProvenance, false) // underdetermined
  | Unsolved([ty]) => ty // recursively contains something unsolved; return suggestion that will contain an unsolved hole
  | Unsolved([_, ..._]) => Unknown(NoProvenance, false) // overdetermined
  };
};

// Since inference has completed, we return all suggestions sans provenance
// If accepted, these will change to TypeHole provenances naturally
let rec get_status_pts = (ctx: Ctx.t, pts: pts): status => {
  let tys = UnionFind.get(pts);
  switch (tys) {
  | [ty] => get_status_ptyp(ctx, ty)
  | [] => Unsolved([])
  | [_, ..._] as xs =>
    Unsolved(
      xs |> List.map(get_status_ptyp(ctx)) |> List.map(unwrap_solution),
    )
  };
}
and get_status_ptyp = (ctx: Ctx.t, ptyp: ptyp): status => {
  switch (ptyp) {
  | Int => Solved(Int)
  | Float => Solved(Float)
  | Bool => Solved(Bool)
  | String => Solved(String)
  | Var(s) => Solved(Var(s))
  | List(pts) =>
    switch (get_status_pts(ctx, pts)) {
    | Solved(ty) => Solved(List(ty))
    | Unsolved(_) => Unsolved([List(Unknown(NoProvenance, false))])
    }
  | Arrow(pts1, pts2) =>
    switch (get_status_pts(ctx, pts1), get_status_pts(ctx, pts2)) {
    | (Solved(ty1), Solved(ty2)) => Solved(Arrow(ty1, ty2))
    | (Solved(ty1), Unsolved(_)) =>
      Unsolved([Arrow(ty1, Unknown(NoProvenance, false))])
    | (Unsolved(_), Solved(ty2)) =>
      Unsolved([Arrow(Unknown(NoProvenance, false), ty2)])
    | (Unsolved(_), Unsolved(_)) =>
      Unsolved([
        Arrow(Unknown(NoProvenance, false), Unknown(NoProvenance, false)),
      ])
    }
  | Sum(tys_inner) =>
    let is_solved = (s: status): bool => {
      switch (s) {
      | Solved(_) => true
      | Unsolved(_) => false
      };
    };
    let force_unwrap_solution = (s: status): Typ.t => {
      switch (s) {
      | Solved(ty) => ty
      | Unsolved(_) => failwith("unreachable")
      };
    };
    let statuses = List.map(get_status_pts(ctx), tys_inner);
    if (List.for_all(is_solved, statuses)) {
      let tys3 =
        statuses
        |> List.map(force_unwrap_solution)
        |> List.map(typ => ("", Some(typ))); // Makes all constructors the empty string! Prob a bad idea!!
      Solved(Sum(tys3));
    } else {
      let tys3 =
        statuses
        |> List.map(unwrap_solution)
        |> List.map(typ => ("", Some(typ)));
      Unsolved([Sum(tys3)]);
    };
  | Prod(tys_inner) =>
    let is_solved = (s: status): bool => {
      switch (s) {
      | Solved(_) => true
      | Unsolved(_) => false
      };
    };
    let force_unwrap_solution = (s: status): Typ.t => {
      switch (s) {
      | Solved(ty) => ty
      | Unsolved(_) => failwith("unreachable")
      };
    };
    let statuses = List.map(get_status_pts(ctx), tys_inner);
    if (List.for_all(is_solved, statuses)) {
      let tys3 = List.map(force_unwrap_solution, statuses);
      Solved(Prod(tys3));
    } else {
      let tys3 = List.map(unwrap_solution, statuses);
      Unsolved([Prod(tys3)]);
    };
  };
};

// Get suggestion will return the solution associated with the provided id
// if it exists as a typehole for which suggestions are present
// TODO: Add logic for indirect suggestions via ExpHoles constrained to TypeHoles
//       eg: scan for ExpHole or Emp
let get_suggestion = (ctx: Ctx.t, id: Id.t): option(status) => {
  open Util.OptUtil.Syntax;
  let+ pts = Ctx.lookup(ctx, Typ.TypeHole(id));
  get_status_pts(ctx, pts);
};