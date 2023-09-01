module Material = {
  include Material;
  type t = Material.t(option(Sort.t), Mold.t);
  let of_molded = Material.map_g(() => None);
};

module Ineq = {
  type t = {
    eq: list(Terrace.t(Material.t)),
    neq: list(Slope.t(Material.t)),
  };
  let empty = {eq: [], lt: []};

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
          let g = s => Terrace.mk(Wald.singleton(Material.Grout(None)));
          let convex = singleton(g(None));
          let concave =
            bound == Prec.min ? cons(g(Some(s)), entered(~bound)) : empty;
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

let cmp = (l: Molded.t, ~slot=?, r: Molded.t): Ziggurat.m =>
  switch (lt(l, ~slot, r), eq(l, ~slot, r), gt(l, ~slot, r)) {
  | (_, Some(z), _) => z
  | (Some(dn), _, _) => Ziggurat.mk(l, ~dn)
  | (_, _, Some(up)) => Ziggurat.mk(~up, r)
  | (None, None, None) => failwith("todo: return incomparable zigg")
  };
