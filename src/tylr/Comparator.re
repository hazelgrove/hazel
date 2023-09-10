let rec walk = (d: Dir.t, z: GZipper.t(Atom.t)): list(Terrace.t(Mold.t, Material.t(Sort.t))) =>
  Regex.step(d, z.zipper)
  |> List.concat_map(zipper => walk(d, {...z, zipper}))
  |> ListUtil.dedup
  // extend recursively produced tails with hd z
  |> List.map(t =>
    switch (Mold.of_atom(z)) {
    | Ok(m) => Terrace.link_to_slot(m)
    | Error(s) => Terrace.fill_slot(Tile(s))
    }
  )
  // add singleton terrace of z if applicable
  |> (
    switch (Mold.of_atom(z)) {
    | Ok(m) => List.cons(Terrace.singleton(m))
    | Error(_) => Fun.id
    }
  );

// deep precedence-bounded entry into given sort and its unidelimited
// dependencies, stepping to nearest token
let enter = (~from: Dir.t, ~l=?, ~r=?, s: Sort.t): list(Terrace.t(Mold.t, Material.t(Sort.t))) => {
  let seen = Hashtbl.create(100);
  let rec go = (~l=?, ~r=?, s: Sort.t) => {
    switch (Hashtbl.find_opt(seen, (l, r, s))) {
    | Some(_) => []
    | None =>
      Hashtbl.add(seen, (l, r, s), ());
      GZipper.enter(~from, ~l?, ~r?, s)
      |> List.concat_map(z =>
        switch (Mold.of_atom(z)) {
        | Ok(m) => [Terrace.singleton(m)]
        | Error(s') =>
          let entered_here = walk(Dir.toggle(from), z);
          let entered_deeper = {
            let (l, r) =
              if (Sort.eq(s', s)) {
                switch (from) {
                | L => (l, z.prec)
                | R => (z.prec, r)
                }
              } else {
                // unbounded across sort transitions
                (None, None);
              };
            go(~l?, ~r?, s');
          };
          entered_here @ entered_deeper;
        }
      );
    }
  };
  go(~l?, ~r?, s);
};

let exit = (side: Dir.t, {sort, prec, zipper}: Mold.t) =>
  failwith("todo exit");

let walk_eq = (d: Dir.t, m: Mold.t): list(Terrace.t(Molded.t, Material.t(Sort.t))) =>
  switch (m.mold) {
  | Tile(m) => walk(d, Mold.to_atom(m))
  | Grout(tips) =>
    switch (Dir.choose(d, tips)) {
    | Convex => []
    | Concave =>
      Tip.[Convex, Concave]
      |> List.map(t => {
        let m = Molded.mk(Grout((Concave, t)));
        Terrace.singleton(~slot=Full(Grout), m)
      })
    }
  };

let walk_lt = (m: Mold.t) => list(Terrace.R.t(Mold.t, Material.t(Sort.t))) =>
  switch (m) {
  | Unlabeled((_, Convex)) => []
  | Unlabeled((_, Concave)) => List.map(enter(~from=L, ~l=Prec.max), Sort.all)
  | Labeled(m) =>
    m.zipper
    |> Regex.step(R)
    |> List.concat_map(
      fun
      | (Atom.Tok(_), _) => []
      | (Kid(s), ctx) => {
        let l =
          Sort.eq(s, m.sort) && Regex.Ctx.nullable(R, ctx)
          ? Some(m.prec) : None;
        enter(~from=L, ~l, s);
      }
    )
  };

let walk_gt = (m: Mold.t): list(Terrace.L.t(Mold.t, Material.t(Sort.t))) =>
  switch (m) {
  | Unlabeled((Convex, _)) => []
  | Unlabeled((Concave, _)) => List.map(enter(~r=Prec.max, ~from=R), Sort.all)
  | Labeled(m) =>
    m.zipper
    |> Regex.step(L)
    |> List.concat_map(
      fun
      | (Atom.Tok(_), _) => []
      | (Kid(s), ctx) => {
        let r =
          Sort.eq(s, m.sort) && Regex.Ctx.nullable(L, ctx)
          ? Some(m.prec) : None;
        enter(s, ~r, ~from=R);
      }
    )
  };

let pick = (~slot=Slot.Empty, ~face, ts: list(Terrace.t(Molded.t, Material.t(Sort.t)))) =>
  ts
  |> List.filter(t => Terrace.face(t).mold == face.mold)
  |> (
    switch (slot) {
    | Empty => Fun.id
    | Full(None) => List.filter(Terrace.has_slot(Grout()))
    | Full(Some(s)) => List.filter(Terrace.has_slot(Tile(s)))
    }
  )
  // todo: sort by score
  |> ListUtil.hd_opt;

// exists a match across alternatives
let matches = (~side: Dir.t, m: Mold.t) => walk_eq(~side, m) != [];

// is convex across all alternatives
let is_convex = (~side: Dir.t, m: Mold.t) => walk_lt(~side, m) == [];

// not taking into account slot sort bc matching should override,
// then later any slot content will be appropriately exited
let eq = (l: Mold.t, r: Mold.t) =>
  walk_eq(R, l)
  |> pick(~face=r)
  |> Option.map(t => Terrace.link_slot(l, t));

let lt = (l: Mold.t, ~slot=Slot.Empty, r: Mold.t) =>
  walk_lt(l)
  |> pick(~slot, ~face=r);
  // |> Option.map(t => Ziggurat.mk(Wald.singleton(l), ~dn=[t]));

let gt = (l: Mold.t, ~slot=Slot.Empty, r: Mold.t) =>
  walk_gt(r)
  |> pick(~slot, ~face=l);
  // |> Option.map(t => Ziggurat.mk(~up=[t], Wald.singleton(r)));

let cmp =
    (l: Material.Molded.t, ~slot=Slot.Empty, r: Material.Molded.t)
    : option(Ziggurat.t(Material.Molded.t, Material.Sorted.t)) => {
  let eq_ = Some(Ziggurat.mk(Wald.mk([l, r], [slot])));
  let lt_ = Some(Ziggurat.singleton(l, ~dn=[Terrace.singleton(~slot, r)]));
  let gt_ = Some(Ziggurat.singleton(~up=[Terrace.singleton(l, ~slot)], r));
  switch (l, r) {
  | (Space, Space) => None
  | (Space, _) => gt_
  | (_, Space) => lt_

  | (Grout((_, Convex)), Grout((Convex, _))) => None
  | (Grout((_, Convex)), Grout((Concave, _))) => gt_
  | (Grout((_, Concave)), Grout((Convex, _))) => lt_
  | (Grout((_, Concave)), Grout((Concave, _))) => eq_

  | (Grout((_, Convex)), Tile(m_r)) =>
    is_convex(~side=L, m_r) ? None : gt_
  | (Grout((_, Concave)), Tile(m_r)) =>
    matches(~side=L, m_r) ? gt_ : lt_
  | (Tile(m_l), Grout((Convex, _))) =>
    is_convex(m_l, ~side=R) ? None : lt_
  | (Tile(m_l), Grout((Concave, _))) =>
    matches(m_l, ~side=R) ? lt_ : gt_

  | (Tile(m_l), Tile(m_r)) =>
    switch (lt(m_l, ~slot, m_r), eq(m_l, m_r), gt(m_l, ~slot, m_r)) {
    | (None, None, None) => None
    | (_, Some(top), _) => Some(Ziggurat.mk(top));
    | (Some(t), _, _) => Some(Ziggurat.mk(Wald.singleton(l), ~dn=[t]))
    | (_, _, Some(t)) => Some(Ziggurat.mk(~up=[t], Wald.singleton(r)))
    }
  };
};


// comparator assumes no merging
let eq = (l: Material.Molded.t, ~slot=Slot.Empty, r: Material.Molded.t) =>
  switch (l, r) {
  | (Space((_, Concave)), Space((Concave, _)))
  | (Grout((_, Concave)), Grout((Concave, _))) =>
    Some(Ziggurat.mk(Wald.mk([l, r], [slot])))
  | (Tile(m_l), Tile(m_r)) =>
    walk_eq(R, m_l)
    |> pick(~slot, ~face=m_r)
    |> Option.map(t => Ziggurat.mk(Terrace.link_slot(m_l, t)))
  | _ => None
  };

let lt = (l: Material.Molded.t, ~slot=Slot.Empty, r: Material.Molded.t) => {
  let ok = Some();
  switch (l, r) {
  | (Space((_, Concave)) | Grout((_, Concave)), _) when is_convex(~side=L, r) => ok
  | (Grout((_, Concave)), Space(_)) => ok
  | (Grout((_, Concave)), Tile(m_r)) when !matches(~side=L, m_r) => ok
  | (Tile(m_l), Space(_) | Grout(_)) =>
  | (Tile(m_l), Tile(m_r)) =>
    walk_lt(m_l, ~side=R)
    |> pick(~slot, ~face=m_r)
    |> Option.map(t => Ziggurat.mk(l, ~dn=[t]))
  | _ => None
  };
};

let cmp = (l: Mold.t, ~slot=Slot.Empty, r: Mold.t): option(Ziggurat.t(Mold.t, Material.t(Sort.t))) =>
  switch (lt(l, ~slot, r), eq(l, r), gt(l, ~slot, r)) {
  | (None, None, None) => None
  | (_, Some(top), _) => Some(Ziggurat.mk(top));
  | (Some(t), _, _) => Some(Ziggurat.mk(Wald.singleton(l), ~dn=[t]))
  | (_, _, Some(t)) => Some(Ziggurat.mk(~up=[t], Wald.singleton(r)))
  };

// convex across all alternatives
let is_convex = (side: Dir.t, m: Mold.t): bool =>
  Option.is_none(Dir.choose(side, step_gt, step_lt)(m));

let cmp = (l: Molded.t, ~slot=Slot.Empty, r: Molded.t): Ziggurat.t(Molded.t, Material.t(Sort.t)) =>
  switch (lt(l, ~slot?, r), eq(l, r), gt(l, ~slot?, r)) {
  | (_, Some(top), _) => Ziggurat.mk(top)
  | (Some(r), _, _) => Ziggurat.mk(Wald.singleton(l), ~dn=[r])
  | (_, _, Some(l)) => Ziggurat.mk(~up=[l], Wald.singleton(r))
  | (None, None, None) =>
    if (is_convex(R, l) && is_convex(L, r)) {
      assert(Slot.is_empty(slot));
      let g = Molded.mk_hole(Concave, Concave);
      let l_gt = Option.get(gt(l, g));
      let lt_r = Option.get(lt(g, r));
      Ziggurat.mk(~up=[l_gt], Wald.singleton(g), ~dn=[lt_r]);
    } else if (is_convex(R, l)) {

      failwith("todo: l and r connected by postfix grout")
    } else {

    }
  }





let g = s => Meld.mk(Wald.singleton(Material.Grout(s)));

let steps = (l: Mold.t, ~slot=?, r: Mold.t): option(Ziggurat.t(Mold.t, Sort.t)) =>
  switch (steps_lt(l, ~slot, r), steps_eq(l, r), steps_gt(l, ~slot, r)) {
  | (None, None, None) => None
  | (_, Some(top), _) => Some(Ziggurat.mk(top));
  | (Some(t), _, _) => Some(Ziggurat.mk(Wald.singleton(m_l), ~dn=[t]))
  | (_, _, Some(t)) => Some(Ziggurat.mk(~up=[t], Wald.singleton(m_r)))
  };


let cmp = (l: Molded.t, ~slot=?, r: Molded.t): Ziggurat.t(Molded.t, option(Sort.t)) => {
  let lt_ = (l, ~slot=?, r) =>
    Ziggurat.mk(Wald.singleton(l), ~dn=[Terrace.singleton(~slot?, r)]);
  let gt_ = (l, ~slot=?, r) =>
    Ziggurat.mk(~up=[Terrace.singleton(l, ~slot?)], Wald.singleton(r));
  let neq = b => b ? lt_ : gt_;
  switch (l.mold, r.mold) {
  | (Grout((tip_l, _)), Grout((_, tip_r))) =>
    if (Slot.Profile.has_tokens(slot)) {
      // mediate between whitespace and holes wrapping slot tokens
      let l = Molded.minimize_holes({...l, mold: Grout((tip_l, Concave))});
      let r = Molded.minimize_holes({...r, mold: Grout((Concave, tip_r))});
      Molded.is_hole(l) == Molded.is_hole(r)
      ? Ziggurat.mk(Wald.mk([l, r], [Slot.full(None)]))
      : neq(Molded.is_hole(l), l, ~slot=Slot.full(None), r);
    } else {
      // merge grout
      let mold = Material.Grout((tip_l, tip_r));
      let token = l.token ++ t ++ r.token;
      Ziggurat.singleton(Molded.minimize_holes({mold, token}));
    }
  // mediate between grout and tiles, possibly inserting/removing holes
  | (Grout((tip_l, _)), Tile(m)) =>
    if (Slot.Profile.has_tokens(slot)) {
      let l = Molded.minimize_holes({...l, mold: Grout((tip_l, Concave))});
      neq(Molded.is_hole(l), l, ~slot=Slot.full(None), r);
    } else {
      let tip_r = is_convex(L, m) ? Concave : Convex;
      let l = Mold.minimize_holes({...l, mold: Grout((tip_l, tip_r))});
      neq(tip_r == Concave, l, r);
    };
  // mirror above
  | (Tile(m), Grout((_, tip_r))) =>
    if (Slot.Profile.has_tokens(slot)) {
      let l = Molded.minimize_holes({...l, mold: Grout((tip_l, Concave))});
      neq(!Molded.is_hole(r), l, ~slot=Slot.full(None), r);
    } else {
      let tip_l = is_convex(R, m) ? Concave : Convex;
      let r = Mold.minimize_holes({...r, mold: Grout((tip_l, tip_r))});
      neq(tip_l == Convex, l, r);
    };
  // mediate between tiles
  | (Tile(m_l), Tile(m_r)) =>
    switch (steps(m_l, ~slot, m_r)) {
    | Some((zigg: Ziggurat.t(Mold.t, Sort.t))) =>
      zigg
      |> Ziggurat.map_piece(m => Molded.mk(Tile(m)))
      |> Ziggurat.map_slot(Option.map(Option.some))
      |> Ziggurat.map_face(L, m => Molded.{...m, token: l.token})
      |> Ziggurat.map_face(R, m => Molded.{...m, token: r.token})
    | None =>
      let zigg: Ziggurat.t(Material.Molded.t, Sort.t) =
        if (is_convex(R, m_l) && is_convex(L, m_r)) {
          let (up, dn) = Terrace.([exit(R, m_l)], [exit(L, m_r)]);
          let g = Material.Grout((Concave, Concave));
          Ziggurat.mk(~up, Wald.singleton(g), ~dn);
        } else if (is_convex(R, m_l)) {
          let g = Material.Grout((Concave, Convex));
          Ziggurat.mk(~up=Terrace.[exit(R, m_l), singleton(g)], Wald.singleton(r));
        } else {
          failwith("todo")
        }

    }



    // todo: implement promote so that it promote ends of zigg with l and r tokens
    let zigg =
      switch (steps_lt(m_l, ~slot, m_r), steps_eq(m_l, m_r), steps_gt(m_l, ~slot, m_r)) {
      | (_, Some(top), _) => Ziggurat.mk(top);
      | (Some(t), _, _) => Ziggurat.mk(Wald.singleton(m_l), ~dn=[t])
      | (_, _, Some(t)) => Ziggurat.mk(~up=[t], Wald.singleton(m_r))
      | (None, None, None) =>
        if (is_convex(R, m_l) && is_convex(L, m_r)) {
          let (up, dn) = Terrace.([exit(R, m_l)], [exit(L, m_r)]);
          let g = Material.Grout((Concave, Concave));
          promote(Ziggurat.mk(~up, Wald.singleton(g), ~dn));
        } else if (is_convex(R, m_l)) {
          let g = Molded.mk_hole(Concave, Convex);
          Ziggurat.mk(~up=Terrace.[promote_t(exit(R, m_l)), singleton(g)], Wald.singleton(r));
        } else {
          failwith("todo")
        }
      };
    let promote = failwith("todo promote mold to molded, sort to option(sort)");
    promote(zigg);
  }
};

let cmp = (l: Molded.t, ~slot=?, r: Molded.t): Ziggurat.t(Material.t) =>
  switch (lt(l, ~slot, r), eq(l, ~slot, r), gt(l, ~slot, r)) {
  | (_, Some(z), _) => z
  | (Some(dn), _, _) => Ziggurat.mk(l, ~dn)
  | (_, _, Some(up)) => Ziggurat.mk(~up, r)
  | (None, None, None) =>
    if (is_convex(R, l) && is_convex(L, r)) {
      failwith("todo: return incomparable zigg")
    } else if (is_convex(R, l)) {
      failwith("todo: l and r connected by postfix grout")
    } else {

    }
  };

module Ineq = {
  type t = {
    eq: list(Terrace.t(Material.t)),
    neq: list(Slope.t(Material.t)),
  };
  let empty = {eq: [], lt: []};
  let is_empty = (==)(empty);

  let singleton = (t: Terrace.t(_)) => {eq: [t], lt: []};

  let decrement = ({eq, neq}: t) => {
    eq: [],
    neq: List.map(a => [a], eq) @ neq,
  };

  let cons = (t: Terrace.t(_), ineq: t) => {...decrement(ineq), eq: [t]};

  let merge2 = ({eq, neq}, {eq: eq', neq: neq'}) => {
    eq: eq @ eq',
    neq: neq @ neq',
  };
  let merge = List.fold_left(merge2, empty);
  let filter = f => TupleUtil.map2(List.filter(f));
  let concat_map: (Slope.t => list(Slope.t), t) => t = failwith("todo");
  // let complete: (~from: Terrace.R.t, t) => list(Slope.t) = failwith("todo");

  let step = (d: Dir.t, m: Material.Molded.t): t => {
    let b = Dir.toggle(d);
    let rec go = (z: GZipper.t(Atom.t)) =>
      z.zipper |> Regex.step(d) |> List.map(stop_or_go) |> merge
    and stop_or_go = (z: GZipper.t(Atom.t)) =>
      switch (z.zipper) {
      | (Tok(lbl), ctx) =>
        let m = GZipper.map(_ => lbl, z);
        {lt: [], eq: [Terrace.mk(Wald.singleton(m))]};
      | (Kid(s), ctx) =>
        let entered = (~bound) =>
          GZipper.enter(~from=b, ~bound, s)
          |> List.map(stop_or_go)
          |> concat
          |> decrement;
        let bound =
          Sort.eq(s, z.sort) && Regex.Ctx.nullable(b, ctx)
            ? z.prec : Prec.min;
        let stepped_lt = entered(~bound);
        let stepped_lt_g = {
          let g = s => Terrace.mk(Wald.singleton(Material.Grout(s)));
          let convex = singleton(g(Some(s)));
          let concave =
            bound == Prec.min ? cons(g(None), entered(~bound)) : empty;
          decrement(merge2(convex, concave));
        };
        let stepped_eq = {
          let m = Meld.mk(Wald.singleton(Material.Grout(Some(s))));
          map(Terrace.put_slot(m), go(z));
        };
        merge([stepped_eq, stepped_lt, stepped_lt_g]);
      };
    go(Mold.to_atom(m));
  };
};

let merges = (l: Molded.t, r: Molded.t): option(Molded.t) =>
  switch (l.mold, r.mold) {
  | (Grout, Grout) =>
    let token = Token.dedup_holes(l.token ++ r.token);
    Some({...l, token});
  | _ => None
  };

let replaces = (l: Molded.t, r: Molded.t): option(Ziggurat.m) => {
  // let replaced_l = add_paths(List.map(_ => 0, l.paths), r);
  // let replaced_r = add_paths(List.map(_ => length(l), r.paths), l);
  switch (l.mold, r.mold) {
  | (Grout(l, _), Tile(m)) when Molded.is_hole(l) && Mold.consistent(~l, m) =>
    Some(Ziggurat.singletons(~up=[Molded.prune_hole(l)], r))
  | (Tile(m), Grout(_, r)) when Mold.consistent(m, ~r) =>
    Some(Ziggurat.singletons(l, ~dn=[Molded.prune_hole(r)]))
  | (Tile(m), Tile(r)) when !Molded.is_finished(l) && Mold.eq(m, r) =>
    Some(Ziggurat.singletons(r))
  | (Tile(l), Tile(m)) when Mold.eq(l, m) && !Molded.is_finished(r) =>
    Some(Ziggurat.singletons(r))
  | _ => None
  };
};

let matches = (l: Molded.t, ~slot=?, r: Molded.t): option(Wald.m) =>
  Ineq.mk(R, l).eq
  |> List.filter(t => Terrace.R.face(t) == r.material)
  |> List.filter(failwith("todo: only terraces taking kid"))
  |> List.hd_opt
  |> Option.map(t => Wald.link(l, t.mel, t.wal));

let eq = (l: Molded.t, ~slot=?, r: Molded.t): option(Ziggurat.m) => {
  open OptUtil.Syntax;
  let has_tokens = kid |> Option.map(snd) |> Option.value(~default=false);
  let/ () = has_tokens ? None : merges(l, r);
  let/ () = has_tokens ? None : replaces(l, r);
  Some(Ziggurat.mk(matches(l, ~slot=Option.map(fst, kid), r)));
};

let lt = (l: Molded.t, ~slot=?, r: Molded.t): option(Slope.Dn.m) =>
  switch (l.mold, r.mold) {
  | (Grout(), Tile(_)) when Ineq.(is_empty(mk(L, r))) =>
  };

let lt = (l: Molded.t, ~slot=?, r: Molded.t): option(Slope.Dn.m) =>
  Ineq.mk(R, l).neq
  |> List.filter(t => Terrace.R.face(t) == r.material)
  |> List.filter(failwith("todo: only terraces taking kid"))
  |> Scorer.pick;
// |> Option.map(Slope.push_top(Terrace.mk(Wald.singleton(l))));

let gt = (l: Molded.t, ~slot=?, r: Molded.t): option(Slope.Up.m) =>
  Ineq.mk(L, r).neq
  |> List.filter(t => Terrace.L.face(t) == l.material)
  |> List.filter(failwith("todo: only terraces taking kid"))
  |> Scorer.pick;
// |> Option.map(Slope.push_top(Terrace.mk(Wald.singleton(r))));

// strict convexity, does not count variably convex tips
let is_convex = (side: Dir.t, m: Molded.t) =>
  Ineq.(is_empty(mk(Dir.toggle(side), m)));

let cmp = (l: Molded.t, ~slot=?, r: Molded.t): Ziggurat.m =>
  switch (lt(l, ~slot, r), eq(l, ~slot, r), gt(l, ~slot, r)) {
  | (_, Some(z), _) => z
  | (Some(dn), _, _) => Ziggurat.mk(l, ~dn)
  | (_, _, Some(up)) => Ziggurat.mk(~up, r)
  | (None, None, None) =>
    if (is_convex(R, l) && is_convex(L, r)) {
      failwith("todo: return incomparable zigg")
    } else if (is_convex(R, l)) {
      failwith("todo: l and r connected by postfix grout")
    } else {

    }
  };
