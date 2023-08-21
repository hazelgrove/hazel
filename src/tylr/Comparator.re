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
};

let zips = (l: Piece.t, r: Piece.t): option(Piece.t) => {
  open OptUtil.Syntax;
  let* () = OptUtil.of_bool(Id.eq(l.id, r.id));
  let+ l =
    switch (Piece.label(l), Piece.label(r)) {
    | (Grout, Tile(_))
    | (Tile(_), Grout) => None
    | (Grout, Grout) => Some(l)
    | (Tile(lbl_l), Tile(lbl_r)) =>
      let+ lbl = Label.zip(lbl_l, lbl_r);
      put_label(lbl, l);
    };
  l
  |> put_token(l.token ++ r.token)
  |> put_paths(l.paths @ List.map(Path.shift(token_length(l)), r.paths));
};

let replaces = (l: Piece.t, r: Piece.t): option(Piece.t) => {
  let replaced_l = add_paths(List.map(_ => 0, l.paths), r);
  let replaced_r = add_paths(List.map(_ => length(l), r.paths), l);
  switch (l.material, r.material) {
  | (Grout((l, _)), Tile(m)) when Mold.consistent(~l, m) =>
    Some(replaced_l)
  | (Tile(m), Grout((_, r))) when Mold.consistent(m, ~r) =>
    Some(replaced_r)
  | (Tile(m), Tile(r)) when !Piece.is_finished(l) && Mold.eq(m, r) =>
    Some(replaced_l)
  | (Tile(l), Tile(m)) when Mold.eq(l, m) && !Piece.is_finished(r) =>
    Some(replaced_r)
  | _ => None
  };
};

let merges = (l: Piece.t, r: Piece.t): option(t) =>
  switch (l.material, r.material) {
  | (Grout((l, _)), Grout((_, r))) =>
    Some({...l, material: Grout((l, r)), token: l.token ++ r.token})
  | _ => None
  };

let fuses = (l: Piece.t, r: Piece.t): option(Piece.t) => {
  open OptUtil.Syntax;
  let/ () = zips(l, r);
  let/ () = replaces(l, r);
  merges(l, r);
};

let step = (d: Dir.t, m: Material.molded): Ineq.t => {
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
        Gram.enter(~from=L, ~bound, s)
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

// // `leq(l, ~kid, r)` steps right from `l` and filters the result
// // to those concluding with `r` and accommodating `kid`
// // postcond: slope is nonempty, top terrace has empty mold
// let leq =
//     (l: Material.molded, ~kid: option(Material.sorted)=?, r: Material.molded)
//     : option(Slope.Dn.m) =>
//   step(R, l)
//   |> Leq.filter(failwith("todo only steps ending with r"))
//   |> Leq.filter(failwith("todo only slopes that take kid"))
//   |> Leq.pick(l);

// // `geq(l, ~kid, r)` steps left from `r`  and filters the result
// // to those concluding with `l` and accommodating `kid`
// // postcond: slope is nonempty, top terrace has empty mold
// let geq =
//     (l: Material.molded, ~kid: option(Material.sorted)=?, r: Material.molded)
//     : option(Slope.Up.m) =>
//   step(L, r)
//   |> Geq.filter(failwith("todo only steps ending with r"))
//   |> Geq.filter(failwith("todo only slopes that take kid"))
//   |> Geq.pick(r);

let matches = (l: Molded.t, ~kid=?, r: Molded.t): option(Wald.m) =>
  step(R, l).eq
  |> List.filter(t => Terrace.R.face(t) == r.material)
  |> List.filter(failwith("todo: only terraces taking kid"))
  |> List.hd_opt
  |> Option.map(t => Wald.link(l, t.mel, t.wal));

let eq = (l: Molded.t, ~kid=?, r: Molded.t): option(Wald.m) => {
  open OptUtil.Syntax;
  let has_tokens = kid |> Option.map(snd) |> Option.value(~default=false);
  let/ () = has_tokens ? None : Option.map(Wald.singleton, fuses(l, r));
  matches(l, ~kid=Option.map(fst, kid), r);
};

let lt = (l: Molded.t, ~kid=?, r: Molded.t): option(Slope.Dn.m) =>
  step(R, l).neq
  |> List.filter(t => Terrace.R.face(t) == r.material)
  |> List.filter(failwith("todo: only terraces taking kid"))
  |> Scorer.pick
  |> Option.map(Slope.push_top(Terrace.mk(Wald.singleton(l))));

let gt = (l: Molded.t, ~kid=?, r: Molded.t): option(Slope.Up.m) =>
  step(L, r).neq
  |> List.filter(t => Terrace.L.face(t) == l.material)
  |> List.filter(failwith("todo: only terraces taking kid"))
  |> Scorer.pick
  |> Option.map(Slope.push_top(Terrace.mk(Wald.singleton(r))));

module Result = {
  type t('lt, 'eq, 'gt) =
    | Lt('lt)
    | Eq('eq)
    | Gt('gt);

  let map = (f_lt, f_eq, f_gt) =>
    fun
    | Lt(lt) => f_lt(lt)
    | Eq(eq) => f_eq(eq)
    | Gt(gt) => f_gt(gt);
};

let cmp =
    (l: Molded.t, ~kid=?, r: Molded.t)
    : option(Result.t(Slope.Dn.m, Wald.m, Slope.Up.m)) =>
  switch (lt(l, ~kid, r), eq(l, ~kid, r), gt(l, ~kid, r)) {
  | (_, Some(eq), _) => Some(Eq(eq))
  | (Some(lt), _, _) => Some(Lt(lt))
  | (_, _, Some(gt)) => Some(Gt(gt))
  | (None, None, None) => None
  };

// let matches = (~side: Dir.t, m: Material.t(Mold.t)): list(Wald.m) => {
//   let rec go = (z: G.Zipper.t(Atom.t)) => {
//     let kid =
//       switch (z.zipper) {
//       | (Kid(_), _) =>
//         failwith("todo mk meld of convex grout")
//       | _ => Meld.empty()
//       };
//     z.zipper
//     |> Regex.step(side)
//     |> List.concat_map(stop_or_go)
//     |> List.map(w =>
//       switch (side) {
//       | L => Wald.knil(w, ~kid, m)
//       | R => Wald.link(m, ~kid, w)
//       }
//     );
//   }
//   and stop_or_go = z =>
//     switch (Mold.of_atom(z)) {
//     | Some(m) => [Wald.singleton(m)]
//     | None => go(z)
//     };
//   go(Mold.to_atom(z));
// };

// let takes = (~side: Dir.t, m: Material.t(Mold.t)): list(Slope.m) => {
//   let rec go = (~entered=false, z: G.Zipper.t(Atom.t)) =>
//     z.zipper
//     |> Regex.step(side)
//     |> List.concat_map(
//       fun
//       | (Kid(s), ctx) =>

//       | (Tok(lbl), ctx) =>
//         entered
//         ?
//     )
//     |> List.map(failwith("todo"))
//   and stop_or_go = (~entered, z) =>
//     switch (z.zipper) {
//     | (Tok(lbl), ctx) =>
//       let p = failwith("mk piece with lbl ctx");
//       entered
//       ? [Slope.mk([Terrace.mk(Wald.singleton(p))])]
//       : []
//     | (Kid(s), ctx) =>

//     }
// };

let rec eq = (l: t, ~kid=Meld.empty(), r: t): option(Padded.t) => {
  open OptUtil.Syntax;
  let/ () = fuses(l, ~kid, r);
  let/ () = matches(l, ~kid, r);
  let* (tl_l, kid_l, p_l) = Chain.unknil(l);
  let* (p_r, kid_r, tl_r) = Chain.unlink(r);
  switch (p_l.shape, Meld.is_porous(kid), p_r.shape) {
  | (G(_), Some(s), _) => eq(tl_l, ~kid=Meld.pad(kid_l, ~r=s), r)
  | (_, Some(s), G(_)) => eq(l, ~kid=Meld.pad(~l=s, kid_r), tl_r)
  | _ => None
  };
};

let cmp =
    (l: Piece.t, ~kid=?, r: Piece.t)
    : option(Result.t(Slope.Dn.t, Wald.t, Slope.Up.t)) =>
  switch (eq(l, ~kid?, r)) {
  | Some(eq) => Some(Eq(eq))
  | None =>
    let kid = Option.map(fst, kid);
    switch (lt(l, ~kid?, r), gt(l, ~kid?, r)) {
    | (Some(lt), _) => Some(Lt(lt))
    | (_, Some(gt)) => Some(Gt(gt))
    | (None, None) => None
    };
  };

let cmp =
    (l: Material.molded, ~kid: option(Material.sorted)=?, r: Material.molded)
    : option(Result.t(Slope.Dn.m, Wald.m, Slope.Up.m)) =>
  switch (leq(l, ~kid?, r), geq(l, ~kid?, r)) {
  | (None, None) => None
  | (
      Some({terrs: [{wal: leq, _}], _}),
      Some({terrs: [{wal: geq, _}], _}),
    )
      when Wald.eq(leq, geq) =>
    Some(Eq(leq))
  // may need to validate some unique precedence relation invariant here
  | (Some(lt), _) => Some(Lt(lt))
  | (_, Some(gt)) => Some(Gt(gt))
  };
