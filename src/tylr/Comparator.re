module Ineq = {
  type t = {
    eq: list(Terrace.t(Material.molded)),
    neq: list(Slope.t(Material.molded)),
  };
  let empty = {eq: [], lt: []};
  let cat = ({eq, neq}, {eq: eq', neq: neq'}) => {
    eq: eq @ eq',
    neq: neq @ neq',
  };
  let concat = List.fold_left(cat, empty);
  let filter = f => TupleUtil.map2(List.filter(f));
  let concat_map: (Slope.t => list(Slope.t), t) => t = failwith("todo");
  // let complete: (~from: Terrace.R.t, t) => list(Slope.t) = failwith("todo");

  let mk = (d: Dir.t, m: Material.molded): t => {
    let rec go = (z: Gram.Zipper.t(Atom.t)) =>
      z.zipper |> Regex.step(d) |> List.map(stop_or_go) |> Result.concat
    and stop_or_go = z =>
      switch (z.zipper) {
      // stop
      | (Tok(lbl), ctx) =>
        let m = failwith("todo mold of lbl ctx sort prec");
        let p = failwith("todo piece of m");
        {lt: [], eq: [Terrace.of_piece(p)]};
      // keep going
      | (Kid(s), _) =>
        // todo: fix unbound `bound`, compute from kid + ctx
        let bound = Sort.eq(l.sort, s) ? bound : Prec.min;
        let stepped_lt =
          G.Zipper.enter(~from=L, ~bound, s)
          |> List.map(stop_or_go)
          |> Result.concat
          |> Result.newline;
        let g = failwith("todo mk grout meld for s");
        let stepped_eq = go(z) |> Result.map(Terrace.R.put_mel(g));
        Result.cat(stepped_eq, stepped_lt);
      };
    // todo: need to handle grout
    go(Mold.to_atom(m));
  };
};

let merges = (l: Molded.t, r: Molded.t): option(Molded.t) =>
  switch (l.mold, r.mold) {
  | (Grout, Grout) =>
    let token = Token.dedup_holes(l.token ++ r.token);
    Some({...l, token})
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

let matches = (l: Molded.t, ~kid=?, r: Molded.t): option(Wald.m) =>
  Ineq.mk(R, l).eq
  |> List.filter(t => Terrace.R.face(t) == r.material)
  |> List.filter(failwith("todo: only terraces taking kid"))
  |> List.hd_opt
  |> Option.map(t => Wald.link(l, t.mel, t.wal));

let eq = (l: Molded.t, ~kid=?, r: Molded.t): option(Ziggurat.m) => {
  open OptUtil.Syntax;
  let has_tokens = kid |> Option.map(snd) |> Option.value(~default=false);
  let/ () = has_tokens ? None : merges(l, r);
  let/ () = has_tokens ? None : replaces(l, r);
  Some(Ziggurat.mk(matches(l, ~kid=Option.map(fst, kid), r)));
};

let lt = (l: Molded.t, ~kid=?, r: Molded.t): option(Slope.Dn.m) =>
  Ineq.mk(R, l).neq
  |> List.filter(t => Terrace.R.face(t) == r.material)
  |> List.filter(failwith("todo: only terraces taking kid"))
  |> Scorer.pick;
// |> Option.map(Slope.push_top(Terrace.mk(Wald.singleton(l))));

let gt = (l: Molded.t, ~kid=?, r: Molded.t): option(Slope.Up.m) =>
  Ineq.mk(L, r).neq
  |> List.filter(t => Terrace.L.face(t) == l.material)
  |> List.filter(failwith("todo: only terraces taking kid"))
  |> Scorer.pick;
// |> Option.map(Slope.push_top(Terrace.mk(Wald.singleton(r))));

// todo: rename meld
let cmp = (l: Molded.t, ~kid=?, r: Molded.t): Ziggurat.m =>
  switch (lt(l, ~kid, r), eq(l, ~kid, r), gt(l, ~kid, r)) {
  | (_, Some(z), _) => z
  | (Some(dn), _, _) => Ziggurat.mk(l, ~dn)
  | (_, _, Some(up)) => Ziggurat.mk(~up, r)
  | (None, None, None) => failwith("todo: return incomparable zigg")
  };
