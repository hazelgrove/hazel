type ptyp =
  | Int
  | Float
  | Bool
  | String
  | Var(string)
  | List(pts)
  | Arrow(pts, pts)
  | Sum(pts, pts) // unused
  | Prod(list(pts))
and pts = UnionFind.elem(list(ptyp));

module Ctx = {
  type t = Hashtbl.t(Typ.type_provenance, pts);

  let create = (): t => Hashtbl.create(100);

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
  | Typ.Unknown(p) => Ctx.lookup_or_create(ctx, p)
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
  | Sum(t1, t2) => Sum(pts_of_typ(ctx, t1), pts_of_typ(ctx, t2))
  | Prod(ts) => Prod(List.map(pts_of_typ(ctx), ts))
  | Typ.Unknown(_p) => failwith("unreachable")
  };
};

// merge two pts
let rec merge = (ctx: Ctx.t, pts1: pts, pts2: pts): pts => {
  let representative = UnionFind.union(pts1, pts2);
  let tys = merge_helper(ctx, pts1, pts2);
  let _ = UnionFind.set(representative, tys);
  representative;
}
and merge_helper = (ctx: Ctx.t, pts1: pts, pts2: pts): list(ptyp) => {
  let tys1 = UnionFind.get(pts1);
  let tys2 = UnionFind.get(pts2);
  List.fold_left(extend_helper(ctx), tys1, tys2);
}
// // extend pts with a ptyp
// and extend = (ctx: Ctx.t, pts: pts, ptyp: ptyp): unit => {
//   let types = UnionFind.get(pts);
//   let types2 = extend_helper(ctx, types, ptyp);
//   ();
// }
and extend_helper = (ctx: Ctx.t, tys: list(ptyp), ptyp: ptyp): list(ptyp) => {
  switch (tys) {
  | [] => [ptyp]
  | [hd, ...tl] =>
    let new_tl = extend_helper(ctx, tl, ptyp);
    switch (hd, ptyp) {
    // duplicate
    | (Int, Int) => [Int, ...new_tl]
    | (Float, Float) => [Float, ...new_tl]
    | (Bool, Bool) => [Bool, ...new_tl]
    | (String, String) => [String, ...new_tl]
    | (Var(s1), Var(s2)) when s1 == s2 => [Var(s1), ...new_tl]
    // similar, merge children
    | (List(pts1), List(pts2)) =>
      let pts = merge(ctx, pts1, pts2);
      [List(pts), ...new_tl];
    | (Arrow(pts1, pts2), Arrow(pts3, pts4)) =>
      let pts1 = merge(ctx, pts1, pts3);
      let pts2 = merge(ctx, pts2, pts4);
      [Arrow(pts1, pts2), ...new_tl];
    | (Sum(pts1, pts2), Sum(pts3, pts4)) =>
      let pts1 = merge(ctx, pts1, pts3);
      let pts2 = merge(ctx, pts2, pts4);
      [Sum(pts1, pts2), ...new_tl];
    | (Prod(tys1), Prod(tys2)) =>
      let tys = List.map2(merge(ctx), tys1, tys2);
      [Prod(tys), ...new_tl];
    // different, keep both
    | _ => [ptyp, hd, ...new_tl]
    };
  };
};

// API
let constrain = (ctx: Ctx.t, t1: Typ.t, t2: Typ.t): unit => {
  let pts1 = pts_of_typ(ctx, t1);
  let pts2 = pts_of_typ(ctx, t2);
  if (!UnionFind.eq(pts1, pts2)) {
    let _ = merge(ctx, pts1, pts2);
    ();
  };
};

type status =
  | Solved(Typ.t)
  | Unsolved(list(ptyp));

let rec get_status = (ctx: Ctx.t, id: Id.t): status => {
  let pts = Ctx.lookup_or_create(ctx, Typ.AstNode(id));
  get_status_helper(ctx, pts);
}
and get_status_helper = (ctx: Ctx.t, pts: pts): status => {
  let tys = UnionFind.get(pts);
  switch (tys) {
  | [ty] =>
    switch (ty) {
    | Int => Solved(Int)
    | Float => Solved(Float)
    | Bool => Solved(Bool)
    | String => Solved(String)
    | Var(s) => Solved(Var(s))
    | List(pts) =>
      switch (get_status_helper(ctx, pts)) {
      | Solved(ty) => Solved(List(ty))
      | Unsolved(_) => Unsolved(tys)
      }
    | Arrow(pts1, pts2) =>
      switch (get_status_helper(ctx, pts1), get_status_helper(ctx, pts2)) {
      | (Solved(ty1), Solved(ty2)) => Solved(Arrow(ty1, ty2))
      | _ => Unsolved(tys)
      }
    | Sum(pts1, pts2) =>
      switch (get_status_helper(ctx, pts1), get_status_helper(ctx, pts2)) {
      | (Solved(ty1), Solved(ty2)) => Solved(Sum(ty1, ty2))
      | _ => Unsolved(tys)
      }
    | Prod(tys_inner) =>
      let is_solved = (s: status): bool => {
        switch (s) {
        | Solved(_) => true
        | Unsolved(_) => false
        };
      };
      let unwrap_solution = (s: status): Typ.t => {
        switch (s) {
        | Solved(ty) => ty
        | Unsolved(_) => failwith("unreachable")
        };
      };
      let statuses = List.map(get_status_helper(ctx), tys_inner);
      if (List.for_all(is_solved, statuses)) {
        let tys3 = List.map(unwrap_solution, statuses);
        Solved(Prod(tys3));
      } else {
        Unsolved(tys);
      };
    }
  | []
  | [_, ..._] => Unsolved(tys)
  };
};
