open Sexplib.Std;

[@deriving sexp]
type t =
  | Hole(bool)
  | Int(bool)
  | Float(bool)
  | Bool(bool)
  | Arrow(t, t, bool)
  | Sum(t, t, bool)
  | Prod(list(t), bool)
  | List(t, bool);

let precedence = (diff: t): int => {
  let ty =
    switch (diff) {
    | Hole(_) => HTyp.Hole
    | Int(_) => Int
    | Float(_) => Float
    | Bool(_) => Bool
    | Arrow(_) => Arrow(Hole, Hole)
    | Sum(_) => Sum(Hole, Hole)
    | Prod([], _) => Prod([])
    | Prod(_) => Prod([Hole])
    | List(_) => List(Hole)
    };
  HTyp.precedence(ty);
};

let rec fill_diff = (ty: HTyp.t, highlight_diff: bool): t => {
  switch (ty) {
  | Hole => Hole(highlight_diff)
  | Int => Int(highlight_diff)
  | Float => Float(highlight_diff)
  | Bool => Bool(highlight_diff)
  | Arrow(ty1, ty2) =>
    Arrow(
      fill_diff(ty1, highlight_diff),
      fill_diff(ty2, highlight_diff),
      highlight_diff,
    )
  | Sum(ty1, ty2) =>
    Sum(
      fill_diff(ty1, highlight_diff),
      fill_diff(ty2, highlight_diff),
      highlight_diff,
    )
  | Prod(tys) =>
    Prod(List.map(ty => fill_diff(ty, highlight_diff), tys), highlight_diff)
  | List(ty) => List(fill_diff(ty, highlight_diff), highlight_diff)
  };
};

let rec mk_diff = (ty1: HTyp.t, ty2: HTyp.t): (t, t) => {
  switch (ty1, ty2) {
  | (Hole, _)
  | (_, Hole)
  | (Int, Int)
  | (Float, Float)
  | (Bool, Bool) => (fill_diff(ty1, false), fill_diff(ty2, false))
  | (Int, _)
  | (Float, _)
  | (Bool, _) => (fill_diff(ty1, true), fill_diff(ty2, true))
  | (Arrow(ty1, ty2), Arrow(ty3, ty4)) =>
    let (t1, t3) = mk_diff(ty1, ty3);
    let (t2, t4) = mk_diff(ty2, ty4);
    (Arrow(t1, t2, false), Arrow(t3, t4, false));
  | (Arrow(_), _) => (fill_diff(ty1, true), fill_diff(ty2, true))
  | (Sum(ty1, ty2), Sum(ty3, ty4)) =>
    let (t1, t3) = mk_diff(ty1, ty3);
    let (t2, t4) = mk_diff(ty2, ty4);
    (Sum(t1, t2, false), Sum(t3, t4, false));
  | (Sum(_), _) => (fill_diff(ty1, true), fill_diff(ty2, true))
  | (Prod(tys1), Prod(tys2)) =>
    /* What should you do if these are different lengths? Only highlight ones that don't match
       or that are extra? Of just highlight the whole thing? */
    // This is how to just highlight whole thing if different lengths

    /*switch (ListUtil.map2_opt(find_common, tys1, tys2)) {
      | None => Inconsistency
      | Some(common) => Node(Prod, common)
      }*/
    // This is special handling of different lengths -- just filling in with inconsistency

    let rec special = (tys1, tys2) => {
      switch (tys1, tys2) {
      | ([], []) => ([], [])
      | ([hd, ...tl], []) =>
        let (t1, _) = special(tl, []);
        ([fill_diff(hd, true), ...t1], []);
      | ([], [hd, ...tl]) =>
        let (_, t2) = special([], tl);
        ([], [fill_diff(hd, true), ...t2]);
      | ([hd1, ...tl1], [hd2, ...tl2]) =>
        let (hd_t1, hd_t2) = mk_diff(hd1, hd2);
        let (tl_t1, tl_t2) = special(tl1, tl2);
        ([hd_t1, ...tl_t1], [hd_t2, ...tl_t2]);
      };
    };
    let (t1, t2) = special(tys1, tys2);
    (Prod(t1, false), Prod(t2, false));
  | (Prod(_), _) => (fill_diff(ty1, true), fill_diff(ty2, true))
  | (List(ty1), List(ty2)) =>
    let (t1, t2) = mk_diff(ty1, ty2);
    (List(t1, false), List(t2, false));
  | (List(_), _) => (fill_diff(ty1, true), fill_diff(ty2, true))
  };
};
